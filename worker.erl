-module(worker).
-include("logging.hrl").
-export([start/0, start/1, start/2]).

-define(UDPPORT,41581).
-define(MULTICAST,{224, 0, 0, 251}).
-define(DEFPORT,41581).
-define(DEFSHARE,"DEFAULT").
-define(BROADCAST_INTERVAL, 1000).
-define(SETUP_TIME, 1000).
-define(PACKET_TYPE, {packet, 4}).
-define(DEFFOLDER, "server").

-define(DOCONTINUE, server(Prv, Nxt, MyShare, MyId, MyPort, Leader, Clients, NPaq, StartTime) ).
-define(DORESET, server(MyShare, MyId, MyPort, Clients, NPaq) ).
server(MyShare, MyId, MyPort, Clients, NPaq) ->
    [Client!reset || Client <- Clients],
    cache!cleanup,
    {ok, Prv, Nxt}=ring:create(MyPort),
    ?INFO("Ring created"), 
	filesystem!dosanitycheck,
    server(Prv, Nxt, MyShare, MyId, MyPort, true, Clients, NPaq,  erlang:monotonic_time()).
    
server(Prv, Nxt, MyShare, MyId, MyPort, Leader, Clients, NPaq, StartTime) ->
    receive
    {udp,_,_,_,B} ->
        %~ ?DF("UDP~p", [B]),
        case cmd:parse(B) of
        ["SERVER", MyShare, Id, Ip, Port] when Leader, Id>MyId ->
            case ring:join(Ip, Port, Prv, Nxt, MyPort) of
            continue -> ?DOCONTINUE;
            {ok, PrvN, NxtN} ->
				?INFO("Joined to server ~p at port ~p", [Id, Port]),
				server(PrvN, NxtN, MyShare, MyId, MyPort, false, Clients, NPaq, erlang:monotonic_time())
            end;
        _ -> ?DOCONTINUE
        end;
    {tcp, Prv, B} ->
        %~ ?DF("TCP~p", [B]),
        case cmd:parse(B) of
        ["SETPRV",Ip,Port] ->
            case ring:setPrv(Ip, Port, Prv) of
                continue -> ?DOCONTINUE;
                {ok,PrvN} -> server(PrvN, Nxt, MyShare, MyId, MyPort, Leader, Clients, NPaq, erlang:monotonic_time())
            end;
        ["FS", MyId, _, Cmd, {Client, Args}] ->
            spawn(fun() ->
                filesystem!{ring, self(), Cmd, Args},
                receive NArgs -> Client!{ans, NArgs} end
            end), ?DOCONTINUE;
        ["FS", Id, N, Cmd, {Client, Args}] ->
            spawn(fun() ->
                cache!{get, self(), Id, N}, receive Cache -> Cache end,
                case Cache of
                {ok, Ans} -> worker!{send, Ans};
                nocached ->
                    filesystem!{ring, self(), Cmd, Args},
                    receive NArgs ->
                        NAns=cmd:make(["FS", Id, N, Cmd, {Client, NArgs}]),
                        cache!{put, Id, N, NAns},
                        worker!{send, NAns},
                        worker!goon
                    end
                end
            end),
            receive goon -> ?DOCONTINUE end;
        _ -> ?ERROR("Unhandled TCP-MSG: ~p", [B]), halt(1)
        end;
    {makepaq, Client, Cmd, Args} ->
        Client!cmd:make(["FS", MyId, NPaq, Cmd, {Client, Args}]),
        server(Prv, Nxt, MyShare, MyId, MyPort, Leader, Clients, NPaq+1, StartTime);
    {send, Data} ->
        TimeElapsed=erlang:convert_time_unit(erlang:monotonic_time()-StartTime, native, milli_seconds),
        case TimeElapsed>=?SETUP_TIME of
        true ->
            case gen_tcp:send(Nxt, Data) of
            ok -> ok;
            _ -> worker!{send, Data}
            end, ?DOCONTINUE;
        false ->
            spawn(fun () ->
                receive after max(?SETUP_TIME-TimeElapsed+10, 0) ->
                    worker!{send, Data}
                end
            end), ?DOCONTINUE
        end;
    {tcp, Nxt, _} -> ?ERROR("Received msg from Nxt"), halt(1);
    {tcp, S, B} ->
        case cmd:parse(B) of
        ["WORK", Port] ->
            case ring:addWorker(S, Port, Prv, Nxt) of
                continue -> ?DOCONTINUE;
                reset -> ?DORESET;
                {ok, PrvN, NxtN} ->  ?INFO("Worker accepted"), server(PrvN, NxtN, MyShare, MyId, MyPort, Leader, Clients, NPaq, erlang:monotonic_time())
            end;
        ["CON"] ->
            Pid=spawn(fun() -> cliente:handler(S) end),
            gen_tcp:controlling_process(S,Pid),
            ?INFO("Accepted client"),
            server(Prv, Nxt, MyShare, MyId, MyPort, Leader, [Pid|Clients], NPaq, StartTime);
        %Messages received from old rings goes here:
        _ -> gen_tcp:close(S), ?DOCONTINUE
        end;
    {client_closed, Pid} -> ?INFO("Client disconnected"), server(Prv, Nxt, MyShare, MyId, MyPort, Leader, Clients--[Pid], NPaq, StartTime);
    {tcp_closed, Nxt} -> gen_tcp:close(Prv), ?DORESET;
    {tcp_closed, Prv} -> gen_tcp:close(Nxt), ?DORESET;
    {tcp_closed, _} -> ?DOCONTINUE;
    {'EXIT',_,Reason} -> ?ERROR("~p", [Reason]), halt(1);
    Msg -> ?INFO("Unhandled MSG: ~p", [Msg]), halt(1)
    end.

getip() -> {ok,[{Ip,_,_}|_]}=inet:getif(), Ip.

announce(UDP, MyShare, MyId, MyPort, Enabled) ->
    receive
    enable -> announce(UDP, MyShare, MyId, MyPort, true);
    disable -> announce(UDP, MyShare, MyId, MyPort, false)
    after ?BROADCAST_INTERVAL ->
        case Enabled of
        true -> gen_udp:send(UDP, ?MULTICAST, ?UDPPORT,  cmd:make(["SERVER", MyShare, MyId, getip(), MyPort]));
        false -> ok
        end,
        announce(UDP, MyShare, MyId, MyPort, Enabled)
    end.

acceptAll(LS) ->
    case gen_tcp:accept(LS) of
    {ok, NS} -> gen_udp:controlling_process(NS,whereis(worker)); 
    {error, Reason} -> ?INFO("Can't accept client: ~p", [Reason])
    end,
    acceptAll(LS).
    
start() -> start(0, ?DEFFOLDER, ?DEFSHARE).
start([MyPortS]) -> start(MyPortS, ?DEFFOLDER, ?DEFSHARE);
start([MyPortS, Folder]) -> start(MyPortS, Folder, ?DEFSHARE).
start(MyPortS, Folder, MyShare) when is_list(MyPortS) ->
    MyPort = try list_to_integer(MyPortS) catch _:_ -> usage() end,
    start(MyPort, Folder, MyShare);
start(MyPort, Folder, MyShare) when is_integer(MyPort)->
    register(worker, self()),
    MyId=erlang:phash2({MyPort, getip(), os:system_time(milli_seconds)})+1,
    register(filesystem, spawn_link(fun() -> fs:server(Folder) end)),
    register(cache, spawn_link(fun() -> cache:server() end)),
    {ok, LS} = gen_tcp:listen(MyPort,[binary, ?PACKET_TYPE]),
    {ok, Port} = inet:port(LS),
    spawn_link(fun() -> acceptAll(LS) end),
    {ok,UDP} = gen_udp:open(?UDPPORT, [binary, {reuseaddr,true}, {active,true},  {ip, ?MULTICAST}, {add_membership, {?MULTICAST, {0,0,0,0}}}]),
    register(announcer, spawn_link(fun() -> announce(UDP, MyShare, MyId, Port, false) end)),
    ?INFO("Starting worker at port ~p with id ~p", [Port, MyId]),
    server(MyShare, MyId, Port, [], 0).

start(_,_) -> usage().
usage() ->
    io:format(
"usage: worker [Folder] [Port]
(default Folder is "++?DEFFOLDER++")
"),
    halt(1).
