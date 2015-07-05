-module(worker).
-compile(export_all).

-define(PORT,41581).
-define(MULTICAST,{224, 0, 0, 251}).
-define(SHARE,"DEFAULT").
-define(TIMEOUT, 1000).
-define(INTERVAL, 500).

%~ -define(DEBUG,1).
-include("debug.erl").

getPort(nocon)-> "X";
getPort(S) -> case inet:peername(S) of {ok,{_,Port}}->Port; X->X end.

joinRing(Ip, Port, MyShare, MyId, MyPort, AN) ->
    {ok, Prv}=gen_tcp:connect(Ip, Port, [binary, {packet, 4}]),
    ?DF("Sending WORK~n"),
    gen_tcp:send(Prv, cmd:make(["WORK", MyPort])),
    receive
    {tcp, Nxt, <<"OKPRV">>} -> gen_tcp:send(Prv, cmd:make(["OKWORK"])), worker(Prv, Nxt, MyShare, MyId, MyPort, AN, false);
    {tcp_close, Prv} -> worker(nocon, nocon, MyShare, MyId, MyPort, AN, true)
    after ?TIMEOUT -> worker(nocon, nocon, MyShare, MyId, MyPort, AN, true)
    end.

addWtoRing(Port, NxtN, Prv, Nxt, MyShare, MyId, MyPort, AN, Leader) ->
    {ok,{Ip,_}}=inet:peername(NxtN),
    SetPrv=cmd:concat(cmd:make(["SETPRV", Ip, Port])),
    gen_tcp:send(Nxt, SetPrv),
    receive
    {tcp, NxtN, <<"OKWORK">>} ->
            gen_tcp:close(Nxt),
            worker(Prv, NxtN, MyShare, MyId, MyPort, AN, Leader);
    {tcp_close, NxtN} -> worker(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
    {tcp, _, SetPrv} ->
        {ok,PrvN}=gen_tcp:connect(Ip, Port, [binary, {active,true}, {packet, 4}]),
        gen_tcp:send(PrvN, cmd:make(["OKPRV"])),
        receive
        {tcp, NxtN, <<"OKWORK">>} ->
                gen_tcp:close(Prv), gen_tcp:close(Nxt),
                worker(PrvN, NxtN, MyShare, MyId, MyPort, AN, Leader);
        {tcp_close, NxtN} -> worker(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader)
        after ?TIMEOUT -> worker(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader)
        end
    after ?TIMEOUT -> worker(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader)
    end.

worker(nocon, nocon, MyShare, MyId, MyPort, AN, _) ->
    {ok, S}=gen_tcp:connect("localhost", MyPort, [binary, {active,true}, {packet, 4}]),
    AN!enable,
    worker(S, S, MyShare, MyId, MyPort, AN, true);
worker(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader) ->
    receive
    {udp,_,_,_,B} ->
        ?DF("UDP~p~n", [B]),
        ?DF("~p - ~p - ~p, leader=~p~n", [getPort(Prv), MyPort, getPort(Nxt), Leader]),
        case cmd:parse(B) of
        ["SERVER", MyShare, Id, Ip, Port] when Leader, Id>MyId ->
            AN!disable,
            gen_tcp:close(Prv), gen_tcp:close(Nxt),
            joinRing(Ip, Port, MyShare, MyId, MyPort, AN);
        ["SERVER", MyShare, MyId, _, _] when Leader, Nxt/=nocon ->
            gen_tcp:send(Nxt, cmd:make(["NWORK",MyId,1])),
             worker(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
        _ -> worker(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader)
        end;
    {tcp, S, B} ->
        ?DF("TCP~p~n", [B]),
        case cmd:parse(B) of
        ["WORK",Port] ->
            addWtoRing(Port, S, Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
        ["SETPRV",Ip,Port] ->
            {ok,PrvN}=gen_tcp:connect(Ip, Port, [binary, {active,true}, {packet, 4}]),
            gen_tcp:send(PrvN, cmd:make(["OKPRV"])),
            gen_tcp:close(Prv),
            worker(PrvN, Nxt, MyShare, MyId, MyPort, AN, Leader);
        ["NWORK",MyId,N] ->
            io:format("Hay ~p workers.~n", [N]),
            worker(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
        ["NWORK",Id,N] ->
            gen_tcp:send(Nxt, cmd:make(["NWORK",Id,N+1])),
            worker(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
        _ ->
            self()!{tcp, S, B},
            ?DF("Unhandled TCP-MSG: ~p~n", [B]),
            worker(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader)
        end;
    {tcp_closed, Nxt}-> ?DF("Ring broken!~n"), gen_tcp:close(Prv), worker(nocon, nocon, MyShare, MyId, MyPort, AN, true);
    {tcp_closed, Prv} -> ?DF("Ring broken!~n"), gen_tcp:close(Nxt), worker(nocon, nocon, MyShare, MyId, MyPort, AN, true);
    {tcp_closed, S} -> ?DF("Unexpected close ~p~n", [S]),
    worker(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader)
    %~ Msg ->
        %~ io:format("Unknown MSG: ~p~n", [Msg]),
        %~ worker(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader, State)
    end.

acceptAll(LS, Pid) ->
    case gen_tcp:accept(LS) of
    {ok, NS} ->  gen_udp:controlling_process(NS,Pid); 
    {error, Reason}-> io:format("~p~n", [Reason])
    end,
    acceptAll(LS, Pid).
    
start(MyShare, MyPort) ->
    MyId=erlang:phash2({self(), MyPort, now()})+1,
    {ok, LS} = gen_tcp:listen(MyPort,[binary, {packet, 4}]),
    spawn(?MODULE, acceptAll, [LS, self()]),
    {ok,UDP} = gen_udp:open(?PORT, [binary, {reuseaddr,true}, {active,true},  {ip, ?MULTICAST}, {add_membership, {?MULTICAST, {0,0,0,0}}}]),
    AN=spawn(?MODULE, announcer, [UDP, MyShare, MyId, MyPort, false]),
    worker(nocon, nocon, MyShare, MyId, MyPort, AN, true).

getip() -> {ok,[{Ip,_,_}|_]}=inet:getif(), Ip.

announcer(UDP, MyShare, MyId, MyPort, Enabled) ->
    receive
    enable -> announcer(UDP, MyShare, MyId, MyPort, true);
    disable -> announcer(UDP, MyShare, MyId, MyPort, false)
    after ?INTERVAL ->
        case Enabled of
        true -> gen_udp:send(UDP, ?MULTICAST, ?PORT,  cmd:make(["SERVER", MyShare, MyId, getip(), MyPort]));
        false -> ok
        end,
        announcer(UDP, MyShare, MyId, MyPort, Enabled)
    end.
    
s() -> start(?SHARE, ?PORT).
s([PortS]) -> s(PortS, ?SHARE);
s([PortS, MyShare]) -> s(PortS, MyShare);
s(PortS) -> s(PortS, ?SHARE).
s(PortS, MyShare) ->
    Port = try list_to_integer(PortS) catch _:_ -> usage() end,
    start(MyShare, Port).
usage() ->
    io:format(
"usage: worker [Port [Share]]
(default is Port="++integer_to_list(?PORT)++", Share="++?SHARE++")\n"),
    halt(1).
