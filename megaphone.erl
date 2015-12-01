-module(megaphone).
-include("logging.hrl").
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
    Pid = spawn(fun() -> handler(UDP, undefined, Port, false) end), register(megaphone, Pid), Pid.
    
%anuncia al worker
handler(UDP, Id, MyPort, Enabled) ->
    receive
    {enable, NewId} -> handler(UDP, NewId, MyPort, true);
    disable -> handler(UDP, Id, MyPort, false);
    {is_enabled, Pid} -> Pid!{is_enabled, Enabled}, handler(UDP, Id, MyPort, Enabled)
    after ?BROADCAST_INTERVAL ->
        case Enabled of
        true -> gen_udp:send(UDP, ?MULTICAST, ?UDPPORT, cmd:make(["SERVER", Id, getip(), MyPort]));
        false -> ok
        end, handler(UDP, Id, MyPort, Enabled)
    end.
