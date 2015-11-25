-module(fs).
-include("logging.hrl").
-include("fsstate.hrl").
-export([server/1, lsd/0, stat/1, del/1, create/1, open/1, count/0, write/2, write/3, read/2, read/3, close/1, rename/2]).


    
write(File, Data) ->
    {File, {Ret, _}}=send("wrt", {File, {notfound, Data}}),
    Ret.
write(File, Offset, Data) ->
    {File, {Ret, _, _}}=send("wrt2", {File, {notfound, Offset, Data}}),
    Ret.
read(File, Size) ->
    {File, {Ret, _, Data}}=send("rea", {File, {notfound, Size, <<>>}}),
    {Ret, Data}.
read(File, Size, Offset) ->
    {File, {Ret, _, _, Data}}=send("rea2", {File, {notfound, Size, Offset, <<>>}}),
    {Ret, Data}.
rename(Src, Dst) -> {Src, Dst, Ret}=send("mv", {Src, Dst, notfound}), Ret.
	
close(File) ->
    ok.
    %~ {File, Ret}=send("clo", {File, notfound}),
	%~ case Ret of
	%~ ok -> filesystem!{workerclosed, File};
	%~ _ -> ok
	%~ end,
	%~ Ret.

%~ isopen(File) ->
	%~ {File, Ret}=send("isopn", {File, false}),
	%~ Ret.
	
%~ findorphans([]) -> ok;
%~ findorphans([File|Files]) ->
	%~ case isopen(File) of
	%~ false -> close(File);
	%~ true -> ok
	%~ end, findorphans(Files).


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


%servidor de archivos
server(St) ->
    receive
    {ring, Pid, Cmd, Acc} ->%mensaje del anillo
        {NwSt, Ans} = procesar(Cmd, Acc, St),
        Pid!Ans,%envia la respuesta al siguiente worker
        server(NwSt);
    dosanitycheck -> server(St);
    {access_state, Func, Pid} -> Pid!{access_state, Func(St)}, server(St);
    {clientopened, File} -> server(fsstate:open_file_client(File, St));
    {clientclosed, File} -> server(fsstate:close_file_client(File, St));
    %dosanitycheck -> spawn(fun() -> lsd(), findorphans(maps:keys(St#fsstate.openfiles)) end), server(St);
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
    case access_state(fun(St) -> fsstate:is_locked(File, St) end) of
    true -> bad;%alreadylocked!
    false ->
        MyId=access_state(fun(St) -> St#fsstate.id end),
        {File, {MyId, Why, Res}} = send("lock", {File, {MyId, Why, ok}}),
        Res
    end.
unlock(File) -> send("unlock", File).
del(File) ->
    case access_state(fun(St) -> fsstate:why_locked(File, St) end) of
    open -> isopen;
    notlocked ->
        case lock(File, for_delete) of
        ok -> {File, Res} = send("del", {File, notfound}), Res;
        bad -> tryagain
        end;
    _ -> tryagain   %some operation is in progress now
    end.
stat(File) -> {File, Ret}=send("stat", {File, notfound}), Ret.

create(File) -> 
    case access_state(fun(St) -> fsstate:why_locked(File, St) end) of
    open -> isopen;
    notlocked ->
        case lock(File, for_create) of
        ok -> 
            Ans = case exists(File) of
                  notfound -> access_state(fun(St) -> fsstate:create_file(File, St) end), ok;
                  _ -> alreadyexists
                  end,
            unlock(File),
            Ans;
        bad -> tryagain
        end;
    _ -> tryagain   %some operation is in progress now
    end.

open(File) ->
    case access_state(fun(St) -> fsstate:why_locked(File, St) end) of
    open -> alreadyopen;
    notlocked ->
        case lock(File, open) of
        ok ->
            {File, Res} = send("opn", {File, notfound}),
            case Res of
            ok -> filesystem!{clientopened, File};
            _-> unlock(File)
            end,
            Res;
        bad -> tryagain
        end;
    _ -> tryagain   %some operation is in progress now
    end.

%procesar recibe el mensaje que llega y el estado. Devuelve el nuevo estado y el mensaje procesado
procesar("lsd", Acc, St) ->
    LocalFiles=fsstate:list_local_files(St),
    false=lists:any(fun (File) -> lists:member(File, Acc) end, LocalFiles),
    {St, Acc++LocalFiles};
procesar("exists", {File, notfound}, St) ->
    case fsstate:exists_locally(File, St) of
    true -> {St, {File, ok}};
    false -> {St, {File, notfound}}
    end;
procesar("count", N, St) -> {St, N+1};
procesar("lock", {File, {Id, Why, ok}}, St) ->
    case fsstate:is_locked(File, St) of
    false -> {fsstate:lock_file(File, Id, Why, St), {File, {Id, Why, ok}}};
    true ->
        case fsstate:who_locked(File, St)<Id of
        true -> {fsstate:lock_file(File, Id, Why, St), {File, {Id, Why, ok}}};
        false -> {St, {File, {Id, Why, bad}}}
        end
    end;
procesar("unlock", File, St) -> {fsstate:unlock_file(File, St), File};
procesar("del", {File, Acc}, St) ->
    NwSt = fsstate:unlock_file(File, St),
    case fsstate:delete_file(File, St) of
    ok -> {NwSt, {File, ok}};
    {error, enoent} -> {NwSt, {File, Acc}};
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
        
%mensajes ya listos reenviarlos directamente
procesar(Str, Acc, St) when Str=="lsd"; Str=="del"; Str=="exists"; Str=="stat"; Str=="lock"; Str=="opn" ->
    {St, Acc};
procesar(Cmd, Acc, _) -> exit(?ERROR("Unknown Command: ~p", [{Cmd, Acc}])).
