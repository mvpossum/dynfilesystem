-module(cliente).
-include("logging.hrl").
-export([handler/1]).


getanswer(badcmd) ->"Eh?";
getanswer(alreadyexist) ->"ERROR 1 EFILEXIST";
getanswer(notfound) ->"ERROR 2 ENOTFOUND";
getanswer(isopen) ->"ERROR 3 EFILEOPEN";
getanswer(badfd) ->"ERROR 4 EBADFILEDSCP";
getanswer(notopen) ->"ERROR 5 ENOTOPEN";
getanswer(alreadyopen) ->"ERROR 6 EALREADYOPEN";
getanswer(nospaceleft) ->"ERROR 7 ENOSPACELEFT";
getanswer(invalidname) ->"ERROR 8 EINVALIDNAME";
getanswer(ok) ->"OK".

    
cleanup(OFiles) ->
    [fs:close(V) || V <- maps:values(OFiles)],
    worker!{client_closed, self()}.

addFile(File, OFiles) ->
    Id=random:uniform(999),
    case maps:is_key(Id, OFiles) of
    true -> addFile(File, OFiles);
    false -> {Id, OFiles#{Id => File}}
    end.


iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).
 
handler(S) ->
    random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
    gen_tcp:send(S, "OK ID 0"),
    handler(S, #{}).
handler(S, OFiles) ->
    receive
    {tcp, S, B} ->
        ?INFO("Received ~p from client.", [B]),
        case cmd:parse(B) of
        ["LSD"] -> 
            List=fs:lsd(),
            gen_tcp:send(S, string:join(["OK"|List], " ")), 
            handler(S, OFiles);
        ["STAT", File] -> 
            case fs:stat(File) of
            notfound -> gen_tcp:send(S, getanswer(notfound));
            {Size, Atime, Mtime, Ctime} ->
                gen_tcp:send(S, ["OK SIZE ",integer_to_list(Size)," ACCESS ",iso_8601_fmt(Atime)," MODIFY ",iso_8601_fmt(Mtime)," CREATE ",iso_8601_fmt(Ctime)])
            end, handler(S, OFiles);
        ["DEL", File] ->
            gen_tcp:send(S, getanswer(fs:del(File))), 
            handler(S, OFiles);
        ["CRE", File] ->
            gen_tcp:send(S, getanswer(fs:create(File))), 
            handler(S, OFiles);
        ["OPN", File] ->
            case fs:open(File) of
            ok ->
                {Id, NOFiles}=addFile(File, OFiles),
                gen_tcp:send(S, "OK FD "++integer_to_list(Id)),
                handler(S, NOFiles);
            {error, Reason} -> gen_tcp:send(S, getanswer(Reason)), handler(S, OFiles)
            end;
        ["WRT", "FD", FD, "SIZE", Size, Data] ->
            case maps:is_key(FD, OFiles) of
            false -> gen_tcp:send(S, getanswer(badfd));
            true ->
                DataCrop=case Size<byte_size(Data) of
                true -> binary:part(Data, 0, Size);
                false -> Data
                end,
                gen_tcp:send(S, getanswer(fs:write(maps:get(FD, OFiles), DataCrop)))
            end, handler(S, OFiles);
        ["WRT2", Name, Size, Offset, Data] ->
			DataCrop=case Size < byte_size(Data) of
			true -> binary:part(Data, 0, Size);
			false -> Data
			end,
			gen_tcp:send(S, getanswer(fs:write(Name, Offset, DataCrop))),
            handler(S, OFiles);
        ["REA", "FD", FD, "SIZE", Size] ->
            case maps:is_key(FD, OFiles) of
            false -> gen_tcp:send(S, getanswer(badfd));
            true ->
                 case fs:read(maps:get(FD, OFiles), Size) of
                 {ok, Data} ->
					Data2=case byte_size(Data) of 0 ->  []; _ -> [<<" ">>,Data] end,
					Ans=[<<"OK SIZE ">>,cmd:toBin(byte_size(Data), int), Data2],
					gen_tcp:send(S, Ans);
                 {Error, _} -> gen_tcp:send(S, getanswer(Error))
                 end
            end, handler(S, OFiles);
        ["REA2", Name, Size, Offset] ->
			 case fs:read(Name, Size, Offset) of
			 {ok, Data} ->
				Data2=case byte_size(Data) of 0 ->  []; _ -> [<<" ">>,Data] end,
				Ans=[<<"OK SIZE ">>,cmd:toBin(byte_size(Data), int), Data2],
				gen_tcp:send(S, Ans);
			 {Error, _} -> gen_tcp:send(S, getanswer(Error))
			 end, handler(S, OFiles);
        ["MV", Src, Dst] ->
			 gen_tcp:send(S, getanswer(fs:rename(Src, Dst))),
			 handler(S, OFiles);
        ["CLO", "FD", FD] ->
            case maps:is_key(FD, OFiles) of
            false -> gen_tcp:send(S, getanswer(badfd)), handler(S, OFiles);
            true ->
                gen_tcp:send(S, getanswer(fs:close(maps:get(FD, OFiles)))),
                handler(S, maps:remove(FD,OFiles))
            end;
        ["BYE"] -> gen_tcp:send(S, getanswer(ok)), gen_tcp:close(S), cleanup(OFiles);
        _ -> gen_tcp:send(S, getanswer(badcmd)), handler(S, OFiles)
        end;
    reset -> handler(S, OFiles);
    {tcp_closed, _} -> ?INFO("Client closed"), cleanup(OFiles)
    end.
