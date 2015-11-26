-module(fs).
-include("logging.hrl").
-include("fsstate.hrl").
-export([server/1, lsd/0, stat/1, del/1, create/1, open/1, count/0, write/2, write/3, read/2, read/3, close/1, rename/2]).


-define(SETUP_TIME, 1000).

%Solicita al worker que cree un paquete y lo envia
send(Cmd, Args) ->
    worker!{makepaq, self(), Cmd, Args},
    receive Paq -> Paq end, send(Paq).
%Envia un paquete y espera su respuesta (reenvia si hubo error)
send(Paq) ->
    worker!{send, Paq},
    receive
    {ans, Ans} -> Ans;
    reset -> timer:apply_after(?SETUP_TIME, ?MODULE, send, [Paq])
    end.

cleanup(File) ->%checks if the file is not open by any connected client
	case isopen_client(File) of
	false -> close(File);%if so, close it.
	true -> ok
	end.
sanitycheck(St) ->
    filesystem!{set_lockeds, getlockeds()}, %update locked files
    lsd(),%one file should be at most in one worker
    lists:foreach(fun cleanup/1, maps:keys(St#fsstate.openlocalfiles)).
    
%servidor de archivos
server(St) ->
    receive
    {ring, Pid, Cmd, Acc} ->%mensaje del anillo
        {NwSt, Ans} = procesar(Cmd, Acc, St),
        Pid!Ans,%envia la respuesta al siguiente worker
        server(NwSt);
    {access_state, Func, Pid} -> Pid!{access_state, Func(St)}, server(St);
    {clientopened, File} -> server(fsstate:open_file_client(File, St));
    {clientclosed, File} -> server(fsstate:close_file_client(File, St));
    {set_lockeds, Lockeds} -> server(fsstate:update_lockeds(Lockeds, St));
    dosanitycheck -> spawn(fun() -> sanitycheck(St) end), server(St);
    Msg -> exit(?ERROR("Unknown Msg: ~p", [Msg]))
    end.

access_state(Func) ->
    filesystem!{access_state, Func, self()},
    receive {access_state, Ans} -> Ans end.

%Funciones que usa el cliente
count() -> send("count", 0).
lsd() -> send("lsd", []).
exists(File) -> {File, Ret}=send("exists", {File, notfound}), Ret.
lock(File, Why) ->
    case access_state(fun(St) -> fsstate:why_locked(File, St) end) of
    notlocked ->
        MyId=access_state(fun(St) -> St#fsstate.id end),
        {File, {MyId, Why, Res}} = send("lock", {File, {MyId, Why, ok}}),
        Res;
    Reason -> {alreadylocked, Reason} 
    end.
unlock(File) -> send("unlock", File).
del(File) ->
    case lock(File, for_delete) of
    ok ->
        {File, Res} = send("del", {File, notfound}),
        unlock(File),
        Res;
    {alreadylocked, open} -> isopen;
    _ -> tryagain
    end.
stat(File) -> {File, Ret}=send("stat", {File, notfound}), Ret.

create(File) -> 
    case lock(File, for_create) of
    ok -> 
        Ans = case exists(File) of
              notfound -> access_state(fun(St) -> fsstate:create_file(File, St) end), ok;
              _ -> alreadyexists
              end,
        unlock(File),
        Ans;
    {alreadylocked, open} -> isopen;
    _ -> tryagain  %some operation is in progress just now
    end.

open(File) ->
    case lock(File, open) of
    ok ->
        {File, Res} = send("opn", {File, notfound}),
        case Res of
        ok -> filesystem!{clientopened, File};
        _-> unlock(File)
        end,
        Res;
    {alreadylocked, open} -> alreadyopen;
    _ -> tryagain
    end.

close(File) ->
    case access_state(fun(St) -> fsstate:why_locked(File, St) end) of
    open -> 
        {File, Res} = send("clo", {File, notfound}),
        case Res of
        ok -> filesystem!{clientclosed, File};
        _ -> ok
        end,
        Res;
    notlocked ->
        case exists(File) of
        ok -> notopen;
        notfound -> notfound
        end;
    _ -> tryagain   %some operation is in progress now
    end.
write(File, Data) ->
    case access_state(fun(St) -> fsstate:why_locked(File, St) end) of
    open -> 
        {File, Ret, _}=send("wrt", {File, notfound, Data}),
        Ret;
    _ -> notopen
    end.
write(File, Offset, Data) ->
    case access_state(fun(St) -> fsstate:why_locked(File, St) end) of
    open -> 
        {File, Ret, _}=send("wrt2", {File, notfound, Offset, Data}),
        Ret;
    _ -> notopen
    end.
read(File, Size) ->
    case access_state(fun(St) -> fsstate:why_locked(File, St) end) of
    open -> 
        {File, Ret, _, Data}=send("rea", {File, notfound, Size, <<>>}),
        {Ret, Data};
    _ -> notopen
    end.
read(File, Offset, Size) ->
    case access_state(fun(St) -> fsstate:why_locked(File, St) end) of
    open -> 
        {File, Ret, _, _, Data}=send("rea2", {File, notfound, Offset, Size, <<>>}),
        {Ret, Data};
    _ -> notopen
    end.
    
rename(Src, Dst) ->
    case lock(Src, to_move) of
    ok -> 
        Ans=case exists(Src) of
        ok ->
            case lock(Dst, to_move) of
            ok ->
                {Src, Dst, Ret}=send("mv", {Src, Dst, notfound}),
                unlock(Dst),
                Ret;
            {alreadylocked, open} -> isopen;
            _ -> tryagain
            end;
        notfound -> notfound
        end,
        unlock(Src),
        Ans;
    {alreadylocked, open} -> isopen;
    _ -> tryagain
    end.
isopen_client(File) -> {File, Ret}=send("isopn_client", {File, false}), Ret.
getlockeds() -> send("getlockeds", #{}).

%procesar recibe el mensaje que llega y el estado. Devuelve el nuevo estado y el mensaje procesado
procesar("lsd", Acc, St) ->
    LocalFiles=fsstate:list_local_files(St),
    case lists:any(fun (File) -> lists:member(File, Acc) end, LocalFiles) of
    false -> ok;
    true -> exit(?ERROR("Duplicated files detected"))
    end,
    {St, Acc++LocalFiles};
procesar("exists", {File, notfound}, St) ->
    case fsstate:exists_locally(File, St) of
    true -> {St, {File, ok}};
    false -> {St, {File, notfound}}
    end;
procesar("count", N, St) -> {St, N+1};
procesar("lock", {File, {Id, Why, ok}}, St) ->
    case fsstate:why_locked(File, St) of
    notlocked -> {fsstate:lock_file(File, Id, Why, St), {File, {Id, Why, ok}}};
    Reason ->
        case fsstate:who_locked(File, St)<Id of
        true -> {fsstate:lock_file(File, Id, Why, St), {File, {Id, Why, ok}}};
        false -> {St, {File, {Id, Why, {alreadylocked, Reason}  }}}
        end
    end;
procesar("unlock", File, St) -> {fsstate:unlock_file(File, St), File};
procesar("del", {File, notfound}, St) ->
    case fsstate:delete_file(File, St) of
    ok -> {St, {File, ok}};
    {error, enoent} -> {St, {File, notfound}};
    _ -> exit(?ERROR("Can't delete file ~p.", [File]))
    end;
procesar("stat", {File, notfound}, St) ->
    case fsstate:file_info(File, St) of
    notfound -> {St, {File, notfound}};
    Info -> {St, {File, Info}}
    end;
procesar("opn", {File, notfound}, St) ->
    case fsstate:exists_locally(File, St) of
    true ->  {fsstate:open_file(File, St), {File, ok}};
    false -> {St, {File, notfound}}
    end;
procesar("clo", {File, Acc}, St) ->
    NwSt = fsstate:unlock_file(File, St),
    case fsstate:is_open_locally(File, NwSt) of
    true ->  {fsstate:close_file(File, NwSt), {File, ok}};
    false -> {NwSt, {File, Acc}}
    end;
procesar("wrt", {File, notfound, Data}, St) ->
    case fsstate:is_open_locally(File, St) of
    true ->
        fsstate:write_file(File, Data, St),
        {St, {File, ok, <<>>}};
    false -> {St, {File, notfound, Data}}
    end;
procesar("wrt2", {File, notfound, Offset, Data}, St) ->
    case fsstate:is_open_locally(File, St) of
    true ->
        fsstate:write_file(File, Offset, Data, St),
        {St, {File, ok, -1, <<>>}};
    false -> {St, {File, notfound, Offset, Data}}
    end;
procesar("rea", {File, notfound, Size, <<>>}, St) ->
    case fsstate:is_open_locally(File, St) of
    true ->
        Data = fsstate:read_file(File, Size, St),
        {St, {File, ok, byte_size(Data), Data}};
    false -> {St, {File, notfound, Size, <<>>}}
    end;
procesar("rea2", {File, notfound, Offset, Size, <<>>}, St) ->
    case fsstate:is_open_locally(File, St) of
    true ->
        Data = fsstate:read_file(File, Offset, Size, St),
        {St, {File, ok, Offset, byte_size(Data), Data}};
    false -> {St, {File, notfound, Offset, Size, <<>>}}
    end;
procesar("mv", {Src, Dst, Acc}, St) ->
    fsstate:delete_file(Dst, St),
    case fsstate:exists_locally(Src, St) of
    true ->
        fsstate:rename_file(Src, Dst, St),
        {St, {Src, Dst, ok}};
    false ->  {St, {Src, Dst, Acc}}
    end;

procesar("isopn_client", {File, false}, St) ->
    {St, {File, fsstate:is_open_client(File, St)}};
procesar("getlockeds", Acc, St) ->
    {St, maps:merge(Acc, St#fsstate.lockedfiles)};
    
%mensajes ya listos reenviarlos directamente
procesar(Str, Acc, St) when Str=="lsd"; Str=="del"; Str=="exists"; Str=="stat"; Str=="lock"; Str=="opn"; Str=="wrt"; Str=="wrt2"; Str=="rea"; Str=="rea2"; Str=="isopn" ->
    {St, Acc};
procesar(Cmd, Acc, _) -> exit(?ERROR("Unknown Command: ~p", [{Cmd, Acc}])).
