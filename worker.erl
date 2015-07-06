-module(worker).
-c(ring).
%~ -define(DEBUG,1).
-include("debug.hrl").
-compile(export_all).

-define(PORT,41581).
-define(MULTICAST,{224, 0, 0, 251}).
-define(SHARE,"DEFAULT").
-define(INTERVAL, 500).

getPort(nocon)-> "X";
getPort(S) -> case inet:peername(S) of {ok,{_,Port}}->Port; X->X end.

server(nocon, nocon, MyShare, MyId, MyPort, AN, _) ->
    {ok, S}=gen_tcp:connect("localhost", MyPort, [binary, {active,true}, {packet, 4}]),
    AN!enable,
    server(S, S, MyShare, MyId, MyPort, AN, true);
server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader) ->
    receive
    {udp,_,_,_,B} ->
        ?DF("UDP~p~n", [B]),
        ?DF("~p - ~p - ~p, leader=~p~n", [getPort(Prv), MyPort, getPort(Nxt), Leader]),
        case cmd:parse(B) of
        ["SERVER", MyShare, Id, Ip, Port] when Leader, Id>MyId ->
            AN!disable,
            gen_tcp:close(Prv), gen_tcp:close(Nxt),
            ring:join(Ip, Port, MyShare, MyId, MyPort, AN);
        ["SERVER", MyShare, MyId, _, _] when Leader, Nxt/=nocon ->
            gen_tcp:send(Nxt, cmd:make(["NWORK",MyId,1])),
             worker:server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
        _ -> server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader)
        end;
    {tcp, S, B} ->
        ?DF("TCP~p~n", [B]),
        case cmd:parse(B) of 
        ["WORK",Port] -> ring:addWorker(S, Port, Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
        ["SETPRV",Ip,Port] -> ring:setPrv(Ip, Port, Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
        ["NWORK",MyId,N] ->
            io:format("Hay ~p workers.~n", [N]),
            server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
        ["NWORK",Id,N] ->
            gen_tcp:send(Nxt, cmd:make(["NWORK",Id,N+1])),
            server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
        _ -> io:format("Unhandled TCP-MSG: ~p~n", [B]), halt(1)
        end;
    {tcp_closed, Nxt}-> ?DF("Ring broken!~n"), gen_tcp:close(Prv), server(nocon, nocon, MyShare, MyId, MyPort, AN, true);
    {tcp_closed, Prv} -> ?DF("Ring broken!~n"), gen_tcp:close(Nxt), server(nocon, nocon, MyShare, MyId, MyPort, AN, true);
    {tcp_closed, S} -> io:format("Unexpected close ~p~n", [S]), server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
    Msg -> io:format("Unhandled MSG: ~p~n", [Msg]), halt(1)
    end.

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

acceptAll(LS, Pid) ->
    case gen_tcp:accept(LS) of
    {ok, NS} ->  gen_udp:controlling_process(NS,Pid); 
    {error, Reason}-> io:format("Can't accept client: ~p~n", [Reason])
    end,
    acceptAll(LS, Pid).
    
start(MyShare, MyPort) ->
    MyId=erlang:phash2({self(), MyPort, now()})+1,
    {ok, LS} = gen_tcp:listen(MyPort,[binary, {packet, 4}]),
    spawn(?MODULE, acceptAll, [LS, self()]),
    {ok,UDP} = gen_udp:open(?PORT, [binary, {reuseaddr,true}, {active,true},  {ip, ?MULTICAST}, {add_membership, {?MULTICAST, {0,0,0,0}}}]),
    AN=spawn(?MODULE, announcer, [UDP, MyShare, MyId, MyPort, false]),
    server(nocon, nocon, MyShare, MyId, MyPort, AN, true).

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
