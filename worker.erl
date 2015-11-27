-module(worker).
-include("logging.hrl").
-include("workerstate.hrl").
-export([prepare_socket/1, start/1, send_to_ring/2]).

-define(SETUP_TIME, 1000).
-define(PACKET_TYPE, {packet, 4}).

prepare_socket(MyPort) ->
    {ok, LS} = gen_tcp:listen(MyPort, [binary, ?PACKET_TYPE]),
    {ok, Port} = inet:port(LS),
    spawn_link(fun() -> acceptAll(LS) end),
    Port.
    
start(St) -> Pid = spawn(fun() -> restart(St) end), register(worker, Pid), Pid.

%prepara/reinicia el worker
restart(St) ->
    [Client!reset || Client <- St#wstate.clients],%avisa a los clientes que se reinicio el anillo
    cache!cleanup,
    {ok, Prv, Nxt}=ring:create(St#wstate.id, St#wstate.port),
    ?INFO("Ring created"), 
	filesystem!dosanitycheck,
    handler(workerstate:reset_start_time(workerstate:set_prv(Prv, workerstate:set_nxt(Nxt, workerstate:set_leader(St))))).

handle_announcements(["SERVER", Id, Ip, Port], St) when St#wstate.isleader, Id>St#wstate.id ->
    case ring:join(Ip, Port, St#wstate.prv, St#wstate.nxt, St#wstate.port) of
    continue -> handler(St);
    {ok, PrvN, NxtN} ->
        ?INFO("Joined to server ~p at port ~p", [Id, Port]),
        handler(workerstate:reset_start_time(workerstate:set_prv(PrvN, workerstate:set_nxt(NxtN, workerstate:unset_leader(St)))))
    end;
handle_announcements(_, St) -> handler(St).

%recibe todos los mensajes del worker
handler(St) ->
    {Prv, Nxt} = {St#wstate.prv, St#wstate.nxt},
    receive
    {udp,_,_,_,B} ->
        %~ ?DF("UDP~p", [B]),
        handle_announcements(cmd:parse(B), St);
    {tcp, Prv, B} ->
        %~ ?DF("TCP~p", [cmd:parse(B)]),
        handlePrv(cmd:parse(B), St);
    {tcp, Nxt, _} -> exit(?ERROR("Received msg from Nxt"));
    {tcp, S, B} -> % sender desconocido
        newclient_handler(S, cmd:parse(B), St);
    {send, Data} -> send_data(Data, St), handler(St);
    {make_ring_packet, Client, Cmd, Args} ->
        Client!cmd:make(["RING", St#wstate.id, St#wstate.numpaq, Cmd, {Client, Args}]),
        handler(workerstate:increment_numpaq(St));
    {pass_announce, Id} -> send_data(cmd:make(["ANNOUNCE", Id]), St), handler(St);
    {client_closed, Pid} -> ?INFO("Client disconnected"), handler(workerstate:remove_client(Pid, St));
    {tcp_closed, Nxt} -> gen_tcp:close(Prv), restart(St);
    {tcp_closed, Prv} -> gen_tcp:close(Nxt), restart(St);
    {tcp_closed, _} -> handler(St);
    Msg -> exit(?ERROR("Unhandled MSG: ~p", [Msg]))
    end.

newclient_handler(S, ["WORK", Port], St) ->%es un worker que quiere unirse
    case ring:addWorker(S, Port, St#wstate.prv, St#wstate.nxt) of
    continue -> handler(St);
    reset -> restart(St);
    {ok, PrvN, NxtN} -> 
        ?INFO("Worker connected"),
        handler(workerstate:reset_start_time(workerstate:set_prv(PrvN, workerstate:set_nxt(NxtN, St))))
    end;
newclient_handler(S, ["CON"], St) ->
    Pid=client:start(clientstate:create(S)),
    gen_tcp:controlling_process(S, Pid),
    ?INFO("Client connected"),
    handler(workerstate:add_client(Pid, St));
%aqui llegan los mensajes de anillos ya destruidos(descartados):
%o de clientes que ponen mal el primer comando
newclient_handler(S, _, St) ->
    gen_tcp:close(S), handler(St).
    
handlePrv(["SETPRV", Ip, Port], St) ->
    case ring:setPrv(Ip, Port, St#wstate.prv) of
    continue -> handler(St);
    {ok,PrvN} ->  handler(workerstate:reset_start_time(workerstate:set_prv(PrvN, St)))
    end;
handlePrv(["RING", Id, N, Cmd, {Client, Args}], St) -> %se recibio un mensaje del fs
    spawn(fun() -> %procesarlo y volverlo a mandar
        if
        Id==St#wstate.id ->
            filesystem!{ring, self(), Cmd, Args},
            receive NArgs -> Client!{ans, NArgs} end;
        true ->
            cache!{get, self(), Id, N},
            receive
            {ok, Ans} -> worker!{send, Ans};
            nocached ->
                filesystem!{ring, self(), Cmd, Args},
                receive NArgs ->
                    NAns=cmd:make(["RING", Id, N, Cmd, {Client, NArgs}]),
                    cache!{put, Id, N, NAns},
                    worker!{send, NAns}
                end
            end
        end
    end), handler(St);
handlePrv(["ANNOUNCE", Id], St) ->
    announcer!{enable, Id}, handler(St);
handlePrv({error, B}, _) -> exit(?ERROR("Badformed TCP-MSG: ~p", [B])).

%Solicita al anillo que cree un paquete y lo envie
send_to_ring(Cmd, Args) ->
    worker!{make_ring_packet, self(), Cmd, Args},
    receive Paq -> Paq end, send(Paq).
%Envia un paquete y espera su respuesta (reenvia si se reinicio)
send(Paq) ->
    worker!{send, Paq},
    receive
    {ans, Ans} -> Ans;
    reset -> timer:apply_after(?SETUP_TIME, ?MODULE, send, [Paq])
    end.
send_data(Data, St) ->
    ElapsedTime=elapsed_since_ms(St#wstate.start_time),
    case ElapsedTime>=?SETUP_TIME of%chequea si el anillo esta esable
    true ->
        case gen_tcp:send(St#wstate.nxt, Data) of
        ok -> ok;
        _ -> timer:send_after(?SETUP_TIME, {send, Data}) %worker may have been disconected
        end;
    false -> %espera un rato antes de mandar
        timer:send_after(max(?SETUP_TIME-ElapsedTime+10, 0), {send, Data})
    end.

elapsed_since_ms(TimeStamp) ->
    erlang:convert_time_unit(erlang:monotonic_time()-TimeStamp, native, milli_seconds).

%acepta todas las conexiones y las manda al worker
acceptAll(LS) ->
    case gen_tcp:accept(LS) of
    {ok, NS} -> gen_udp:controlling_process(NS, whereis(worker)); 
    {error, Reason} -> ?INFO("Can't accept client: ~p", [Reason])
    end,
    acceptAll(LS).
