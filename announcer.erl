-module(announcer).
-include("logging.hrl").
-include("workerstate.hrl").
-export([start/2]).

-define(UDPPORT,41581).
-define(MULTICAST,{224, 0, 0, 251}).
-define(BROADCAST_INTERVAL, 1000).
-define(CHANCE_CHANGE_ANNOUNCER, 0.2).

%retorna la ip de la primer interfaz encontrada
getip() -> {ok,[{Ip,_,_}|_]}=inet:getif(), Ip.

start(Port, WorkerPid) ->
    {ok,UDP} = gen_udp:open(?UDPPORT, [binary, {reuseaddr,true},
                            {active,true},  {ip, ?MULTICAST},
                            {add_membership, {?MULTICAST, {0,0,0,0}}}]),
    gen_udp:controlling_process(UDP, WorkerPid),%worker receives the messages
    Pid = spawn(fun() -> handler(UDP, undefined, Port, false) end), register(announcer, Pid), Pid.
    
%anuncia al worker
handler(UDP, Id, MyPort, Enabled) ->
    receive
    {enable, NewId} -> handler(UDP, NewId, MyPort, true);
    disable -> handler(UDP, Id, MyPort, false)
    after ?BROADCAST_INTERVAL ->
        case Enabled of
        true ->
            case random:uniform() =< ?CHANCE_CHANGE_ANNOUNCER of
            true ->
                worker!{pass_announce, Id},
                handler(UDP, undefined, MyPort, false);
            false ->
                gen_udp:send(UDP, ?MULTICAST, ?UDPPORT,  cmd:make(["SERVER", Id, getip(), MyPort])),
                handler(UDP, Id, MyPort, Enabled)
            end;
        false -> handler(UDP, Id, MyPort, Enabled)
        end
    end.
