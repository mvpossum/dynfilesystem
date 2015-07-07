-module(worker).
-define(DEBUG,1).
-include("debug.hrl").
-compile(export_all).

-define(UDPPORT,41581).
-define(DEFPORT,41581).
-define(MULTICAST,{224, 0, 0, 251}).
-define(DEFSHARE,"DEFAULT").
-define(BROADCAST_INTERVAL, 500).

%~ getPort(nocon)-> "X";
%~ getPort(S) -> case inet:peername(S) of {ok,{_,Port}}->Port; X->X end.

server(nocon, nocon, MyShare, MyId, MyPort, AN, _) ->
    ring:create(MyShare, MyId, MyPort, AN);
server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader) ->
    receive
    {udp,_,_,_,B} ->
        %~ ?DF("UDP~p", [B]),
        %~ ?DF("~p - ~p - ~p, leader=~p", [getPort(Prv), MyPort, getPort(Nxt), Leader]),
        case cmd:parse(B) of
        ["SERVER", MyShare, Id, Ip, Port] when Leader, Id>MyId ->
            ring:join(Ip, Port, Prv, Nxt, MyShare, MyId, MyPort, AN);
        ["SERVER", _, MyId, _, _] when Leader, Nxt/=nocon ->
             gen_tcp:send(Nxt, cmd:make(["NWORK",MyId,1])),
             server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
        _ -> server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader)
        end;
    {tcp, S, B} ->
        %~ ?DF("TCP~p", [B]),
        case cmd:parse(B) of
        ["WORK",Port] -> ring:addWorker(S, Port, Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
        ["SETPRV",Ip,Port] -> ring:setPrv(Ip, Port, Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
        ["NWORK",MyId,N] ->
            io:format("Hay ~p workers.~n", [N]),
            server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
        ["NWORK",Id,N] ->
            gen_tcp:send(Nxt, cmd:make(["NWORK",Id,N+1])),
            server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
        _ -> io:format("Unhandled TCP-MSG: ~p", [B]), halt(1)
        end;
    {tcp_closed, Nxt}-> gen_tcp:close(Prv), server(nocon, nocon, MyShare, MyId, MyPort, AN, true);
    {tcp_closed, Prv}-> gen_tcp:close(Nxt), server(nocon, nocon, MyShare, MyId, MyPort, AN, true);
    {tcp_closed, _} -> server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
    Msg -> io:format("Unhandled MSG: ~p", [Msg]), halt(1)
    end.

getip() -> {ok,[{Ip,_,_}|_]}=inet:getif(), Ip.

announcer(UDP, MyShare, MyId, MyPort, Enabled) ->
    receive
    enable -> announcer(UDP, MyShare, MyId, MyPort, true);
    disable -> announcer(UDP, MyShare, MyId, MyPort, false)
    after ?BROADCAST_INTERVAL ->
        case Enabled of
        true -> ok=gen_udp:send(UDP, ?MULTICAST, ?UDPPORT,  cmd:make(["SERVER", MyShare, MyId, getip(), MyPort]));
        false -> ok
        end,
        announcer(UDP, MyShare, MyId, MyPort, Enabled)
    end.

acceptAll(LS, Pid) ->
    case gen_tcp:accept(LS) of
    {ok, NS} -> gen_udp:controlling_process(NS,Pid); 
    {error, Reason} -> io:format("Can't accept client: ~p", [Reason])
    end,
    acceptAll(LS, Pid).
    
start() -> start(?DEFPORT, ?DEFSHARE).
start([MyPortS]) -> start(MyPortS, ?DEFSHARE);
start([MyPortS, MyShare]) -> start(MyPortS, MyShare);
start(MyPortS) -> start(MyPortS, ?DEFSHARE).
start(MyPortS, MyShare) when is_list(MyPortS) ->
    MyPort = try list_to_integer(MyPortS) catch _:_ -> usage() end,
    start(MyPort, MyShare);
    
start(MyPort, MyShare) when is_integer(MyPort)->
    MyId=erlang:phash2({self(), MyPort, now()})+1,
    {ok, LS} = gen_tcp:listen(MyPort,[binary, {packet, 4}]),
    spawn(?MODULE, acceptAll, [LS, self()]),
    {ok,UDP} = gen_udp:open(?UDPPORT, [binary, {reuseaddr,true}, {active,true},  {ip, ?MULTICAST}, {add_membership, {?MULTICAST, {0,0,0,0}}}]),
    AN=spawn(?MODULE, announcer, [UDP, MyShare, MyId, MyPort, false]),
    server(nocon, nocon, MyShare, MyId, MyPort, AN, true);

start(_,_) -> usage().
usage() ->
    io:format(
"usage: worker [Port [Share]]
(default is Port="++integer_to_list(?DEFPORT)++", Share="++?DEFSHARE++")\n"),
    halt(1).
