-module(fs).
-include_lib("kernel/include/file.hrl").
-include("logging.hrl").
-export([server/1, lsd/0, stat/1, del/1, exist/1, create/1, open/1, write/2, write/3, read/2, read/3, close/1, rename/2]).


-define(SETUP_TIME, 1000).

procesar(Paq) ->
    worker!{send, Paq},
    F=(fun(F) ->
        receive 
        reset -> F(F)
        after ?SETUP_TIME -> procesar(Paq)
        end
    end),
    receive
    reset -> F(F);
    {ans, Ans} -> Ans
    end.
procesar(Cmd, Args) ->
    worker!{makepaq, self(), Cmd, Args},
    receive Paq -> Paq end, procesar(Paq).
    
lsd() -> procesar("lsd", []).
del(File) -> {File, Ret}=procesar("del", {File, notfound}), Ret.
exist(File) -> {File, Ret}=procesar("exist", {File, notfound}), Ret.
stat(File) -> {File, Ret}=procesar("stat", {File, notfound}), Ret.
create(File) ->
    N=procesar("count", 0),
    {File, Ret}=procesar("cre", {File, N, 1, create}),
    Ret.
open(File) ->
	{File, Ret}=procesar("opn", {File, {error, notfound}}),
	case Ret of
	ok -> filesystem!{workeropened, File};
	_ -> ok
	end,
	Ret.

write(File, Data) ->
    {File, {Ret, _}}=procesar("wrt", {File, {notfound, Data}}),
    Ret.
write(File, Offset, Data) ->
    {File, {Ret, _, _}}=procesar("wrt2", {File, {notfound, Offset, Data}}),
    Ret.
read(File, Size) ->
    {File, {Ret, _, Data}}=procesar("rea", {File, {notfound, Size, <<>>}}),
    {Ret, Data}.
read(File, Size, Offset) ->
    {File, {Ret, _, _, Data}}=procesar("rea2", {File, {notfound, Size, Offset, <<>>}}),
    {Ret, Data}.
rename(Src, Dst) -> {Src, Dst, Ret}=procesar("mv", {Src, Dst, notfound}), Ret.
	
close(File) ->
    {File, Ret}=procesar("clo", {File, notfound}),
	case Ret of
	ok -> filesystem!{workerclosed, File};
	_ -> ok
	end,
	Ret.

isopen(File) ->
	{File, Ret}=procesar("isopn", {File, false}),
	Ret.
	
findorphans([]) -> ok;
findorphans([File|Files]) ->
	case isopen(File) of
	false -> close(File);
	true -> ok
	end, findorphans(Files).
	
server(Folder) ->
    file:make_dir(Folder),
    code:add_path(filename:absname(".")),%OJO
    ok=file:set_cwd(Folder),
    server(#{}, sets:new()).
server(OFiles, WorkerFiles) ->
    receive
    {ring, Pid, "lsd", Acc} ->
        {ok, List} = file:list_dir("."),
        MyFiles=[File || File <- List, filelib:is_file(File)],
        false=lists:any(fun (E) -> lists:member(E, Acc) end, MyFiles),
        Pid!Acc++MyFiles,
        server(OFiles, WorkerFiles);
    {ring, Pid, "del", {File, notfound}} ->
        case maps:is_key(File, OFiles) of
        true -> Pid!{File, isopen};
        false ->
            case file:delete(File) of
            ok -> Pid!{File, ok};
            {error, enoent} -> Pid!{File, notfound};
            _ -> exit("Can't delete file.")
            end
        end, server(OFiles, WorkerFiles);
    {ring, Pid, "exist", {File, notfound}} ->
        case filelib:is_file(File) of
        true -> Pid!{File, ok};
        false -> Pid!{File, notfound}
        end, server(OFiles, WorkerFiles);
    {ring, Pid, "count", N} ->
        Pid!N+1, server(OFiles, WorkerFiles);
    {ring, Pid, "cre", {File, N, M, create}} ->
        case filelib:is_file(File) of
        true -> Pid!{File, alreadyexist};
        false ->
            case N of
            M -> ok=file:write_file(File,<<>>), Pid!{File, ok};
            _ -> Pid!{File, N, M+1, create}
            end
        end, server(OFiles, WorkerFiles);
    {ring, Pid, "stat", {File, notfound}} ->
        case file:read_file_info(File) of
        {ok, FileInfo} -> Pid!{File, {FileInfo#file_info.size, FileInfo#file_info.atime, FileInfo#file_info.mtime, FileInfo#file_info.ctime}};
        {error, enoent} -> Pid!{File, notfound}
        end, server(OFiles, WorkerFiles);
    {ring, Pid, "opn", {File, {error, notfound}}} ->
        case filelib:is_file(File) of
        true ->
            case maps:is_key(File, OFiles) of
            true -> Pid!{File, {error, alreadyopen}}, server(OFiles, WorkerFiles);
            false ->
                case file:open(File, [read,append,raw,binary]) of
                {ok, FD} -> Pid!{File, ok}, server(OFiles#{File => FD}, WorkerFiles);
                _ -> exit("Can't open file.")
                end
            end;
        false -> Pid!{File, {error, notfound}}, server(OFiles, WorkerFiles)
        end;
    {ring, Pid, "clo", {File, notfound}} ->
        case filelib:is_file(File) of
        true ->
            case maps:get(File, OFiles, nokey) of
            nokey -> Pid!{File, notopen}, server(OFiles, WorkerFiles);
            FD -> 
                file:close(FD),
                Pid!{File, ok}, server(maps:remove(File, OFiles), WorkerFiles)
            end;
        false -> Pid!{File, notfound}, server(OFiles, WorkerFiles)
        end;
    {ring, Pid, "wrt", {File, {notfound, Data}}} ->
        case filelib:is_file(File) of
        true ->
            case maps:get(File, OFiles, nokey) of
            nokey -> Pid!{File, {notopen, <<>>}};
            FD -> 
                case file:write(FD, Data) of
                ok -> Pid!{File, {ok, <<>>}};
                {error, enospc} ->  Pid!{File, {nospaceleft, <<>>}};
                _ -> exit("Can't write file")
                end
            end;
        false -> Pid!{File, {notfound, Data}}
        end, server(OFiles, WorkerFiles);
    {ring, Pid, "wrt2", {File, {notfound, Offset, Data}}} ->
        case filelib:is_file(File) of
        true ->
            case maps:get(File, OFiles, nokey) of
            nokey -> 
                case file:open(File, [read,append,raw,binary]) of
                {ok, FD} ->
					file:position(FD, Offset),
					case file:write(FD, Data) of
					ok -> Pid!{File, {ok, 0, <<>>}};
					{error, enospc} ->  Pid!{File, {nospaceleft, 0, <<>>}};
					_ -> exit("Can't write file")
					end,
					file:close(FD);
                _ -> exit("Can't open file.")
                end;
            _ -> Pid!{File, {alreadyopen, 0, <<>>}}
            end;
        false -> Pid!{File, {notfound, Offset, Data}}
        end, server(OFiles, WorkerFiles);
    {ring, Pid, "rea", {File, {notfound, Size, <<>>}}} ->
        case filelib:is_file(File) of
        true ->
            case maps:get(File, OFiles, nokey) of
            nokey -> Pid!{File, {notopen, 0, <<>>}};
            FD -> 
                case file:read(FD, Size) of
                {ok, Data} -> Pid!{File, {ok, byte_size(Data), Data}};
                eof -> Pid!{File, {ok, 0, <<>>}};
                _ -> exit("Can't read file")
                end
            end;
        false -> Pid!{File, {notfound, Size, <<>>}}
        end, server(OFiles, WorkerFiles);
    {ring, Pid, "rea2", {File, {notfound, Size, Offset, <<>>}}} ->
        case filelib:is_file(File) of
        true ->
            case maps:get(File, OFiles, nokey) of
            nokey -> 
                case file:open(File, [read,append,raw,binary]) of
                {ok, FD} ->
					file:position(FD, Offset),
					case file:read(FD, Size) of
					{ok, Data} -> Pid!{File, {ok, byte_size(Data), Offset, Data}};
					eof -> Pid!{File, {ok, 0, 0, <<>>}};
					_ -> exit("Can't read file")
					end,
					file:close(FD);
                _ -> exit("Can't open file.")
                end;
            _ -> Pid!{File, {alreadyopen, 0, 0, <<>>}}
            end;
        false -> Pid!{File, {notfound, Size, Offset, <<>>}}
        end, server(OFiles, WorkerFiles);
    {ring, Pid, "mv", {Src, Dst, notfound}} ->
        case filelib:is_file(Src) of
        true ->
            file:delete(Dst),
			case file:rename(Src, Dst) of
			ok -> Pid!{Src, Dst, ok};
			{error, _} -> Pid!{Src, Dst, invalidname}
			end;
        false -> Pid!{Src, Dst, notfound}
        end, server(OFiles, WorkerFiles);
    {ring, Pid, "mv", {Src, Dst, Acc}} ->
		false=filelib:is_file(Src),
        file:delete(Dst),
		Pid!{Src, Dst, Acc},
		server(OFiles, WorkerFiles);
    {ring, Pid, "isopn", {File, Acc}} ->
        case sets:is_element(File, WorkerFiles) of
        true -> Pid!{File, true};
        false -> Pid!{File, Acc}
        end, server(OFiles, WorkerFiles);
    {ring, Pid, Cmd, {File, Acc}} ->
        case lists:member(Cmd, ["del", "exist", "cre", "opn", "wrt", "wrt2", "rea", "rea2", "clo", "stat"]) of
        true ->
            false=filelib:is_file(File), Pid!{File, Acc};
        _ -> exit(io_lib:format("Unknown Msg: ~p", {ring, Pid, Cmd, {File, Acc}}))
        end, server(OFiles, WorkerFiles);
    {workeropened, File} -> server(OFiles, sets:add_element(File,WorkerFiles));
    {workerclosed, File} -> server(OFiles, sets:del_element(File,WorkerFiles));
    dosanitycheck -> spawn(fun() -> lsd(), findorphans(maps:keys(OFiles)) end), server(OFiles, WorkerFiles);
    Msg -> exit(io_lib:format("Unknown Msg: ~p", Msg))
    end.

