-module(client).
-include("logging.hrl").
-include("clientstate.hrl").
-export([start/1]).

start(St) ->
    clientstate:send("OK ID 0", St),
    spawn(fun() -> handler(St) end).

handler(St) ->
    S = St#cstate.sock,
    receive
    {tcp, S, B} ->
        ?INFO("Received ~p from client.", [B]),
        case cmd:parse(B) of
        ["BYE"] ->
            clientstate:send(getanswer(ok), St),
            clientstate:cleanup(St);
        OtherMsg -> 
            {NwSt, Ans} = handleCommand(OtherMsg, St),
            clientstate:send(Ans, St), handler(NwSt)
        end;
    reset -> handler(St);
    {tcp_closed, _} -> ?INFO("Client closed"), clientstate:cleanup(St)
    end.

getanswer(ok) -> "OK";
getanswer(badcmd) -> "Eh?";
getanswer(alreadyexists) -> "ERROR 1 EFILEXIST";
getanswer(notfound) -> "ERROR 2 ENOTFOUND";
getanswer(isopen) -> "ERROR 3 EFILEOPEN";
getanswer(badfd) -> "ERROR 4 EBADFILEDSCP";
getanswer(notopen) -> "ERROR 5 ENOTOPEN";
getanswer(alreadyopen) -> "ERROR 6 EALREADYOPEN";
getanswer(nospaceleft) -> "ERROR 7 ENOSPACELEFT";
getanswer(invalidname) -> "ERROR 8 EINVALIDNAME";
getanswer(tryagain) -> "ERROR 9 ETRYAGAIN".

handleCommand(["LSD"], St) ->
    List = fs:lsd(),
    {St, string:join(["OK"|List], " ")};
handleCommand(["STAT", File], St) ->
    case fs:stat(File) of
    notfound -> getanswer(notfound);
    {Size, Atime, Mtime, Ctime} ->
        {St, ["OK SIZE ",integer_to_list(Size)," ACCESS ",iso_8601_fmt(Atime)," MODIFY ",iso_8601_fmt(Mtime)," CREATE ",iso_8601_fmt(Ctime)]}
    end;
handleCommand(["DEL", File], St) ->
    {St, getanswer(fs:del(File))};
handleCommand(["CRE", File], St) ->
    {St, getanswer(fs:create(File))};
handleCommand(["OPN", File], St) ->
    case fs:open(File) of
    ok ->
        {NwSt, FD}=clientstate:open_file(File, St),
        {NwSt, "OK FD "++integer_to_list(FD)};
    Reason -> {St, getanswer(Reason)}
    end;
handleCommand(["WRT", "FD", FD, "SIZE", Size, Data], St) ->
    case clientstate:get_file(FD, St) of
    notopen -> {St, getanswer(badfd)};
    File -> 
        DataCrop=crop_extra_data(Data, Size),
        {St, getanswer(fs:write(File, DataCrop))}
    end;
handleCommand(["WRT2", "FD", FD, "SIZE", Size, "OFFSET", Offset, Data], St) ->
    case clientstate:get_file(FD, St) of
    notopen -> {St, getanswer(badfd)};
    File -> 
        DataCrop=crop_extra_data(Data, Size),
        {St, getanswer(fs:write(File, Offset, DataCrop))}
    end;
handleCommand(["REA", "FD", FD, "SIZE", Size], St) ->
    case clientstate:get_file(FD, St) of
    notopen -> {St, getanswer(badfd)};
    File -> 
         case fs:read(File, Size) of
         {ok, Data} ->
            Data2=prepend_if_not_empty(<<" ">>, Data),
            {St, [<<"OK SIZE ">>,cmd:toBin(byte_size(Data), int), Data2]};
         {Error, _} -> {St, getanswer(Error)}
         end
    end;
handleCommand(["REA2", "FD", FD, "OFFSET", Offset, "SIZE", Size], St) ->
    case clientstate:get_file(FD, St) of
    notopen -> {St, getanswer(badfd)};
    File -> 
         case fs:read(File, Offset, Size) of
         {ok, Data} ->
            Data2=prepend_if_not_empty(<<" ">>, Data),
            {St, [<<"OK SIZE ">>,cmd:toBin(byte_size(Data), int), Data2]};
         {Error, _} -> {St, getanswer(Error)}
         end
    end;
handleCommand(["MV", Src, Dst], St) ->
    {St, getanswer(fs:rename(Src, Dst))};
handleCommand(["CLO", "FD", FD], St) ->
    case clientstate:get_file(FD, St) of
    notopen -> {St, getanswer(badfd)};
    File -> {clientstate:close_file(FD, St), getanswer(fs:close(File))}
    end;
handleCommand(_, St) -> {St, getanswer(badcmd)}.


%util functions
iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).
crop_extra_data(Data, Size) ->
    case Size < byte_size(Data) of
    true -> binary:part(Data, 0, Size);
    false -> Data
    end.
prepend_if_not_empty(ToAdd, Data) ->
    case byte_size(Data) of
    0 -> [];
    _ -> [ToAdd,Data]
    end.

