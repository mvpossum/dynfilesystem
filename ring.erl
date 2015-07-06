-module(ring).
%~ -define(DEBUG,1).
-include("debug.hrl").
-export([join/6, addWorker/9, setPrv/9]).


-define(TIMEOUT, 1000).

join(Ip, Port, MyShare, MyId, MyPort, AN) ->
    {ok, Prv}=gen_tcp:connect(Ip, Port, [binary, {packet, 4}]),
    ?DF("Sending WORK~n"),
    gen_tcp:send(Prv, cmd:make(["WORK", MyPort])),
    receive
    {tcp, Nxt, <<"OKPRV">>} -> gen_tcp:send(Prv, cmd:make(["OKWORK"])), worker:server(Prv, Nxt, MyShare, MyId, MyPort, AN, false);
    {tcp_close, Prv} -> worker:server(nocon, nocon, MyShare, MyId, MyPort, AN, true)
    after ?TIMEOUT -> worker:server(nocon, nocon, MyShare, MyId, MyPort, AN, true)
    end.

addWorker(NxtN, Port, Prv, Nxt, MyShare, MyId, MyPort, AN, Leader) ->
    {ok,{Ip,_}}=inet:peername(NxtN),
    SetPrv=cmd:concat(cmd:make(["SETPRV", Ip, Port])),
    gen_tcp:send(Nxt, SetPrv),
    receive
    {tcp, NxtN, <<"OKWORK">>} ->
            gen_tcp:close(Nxt),
            worker:server(Prv, NxtN, MyShare, MyId, MyPort, AN, Leader);
    {tcp, _, SetPrv} ->
        {ok,PrvN}=gen_tcp:connect(Ip, Port, [binary, {active,true}, {packet, 4}]),
        gen_tcp:send(PrvN, cmd:make(["OKPRV"])),
        receive
        {tcp, NxtN, <<"OKWORK">>} ->
                gen_tcp:close(Prv), gen_tcp:close(Nxt),
                worker:server(PrvN, NxtN, MyShare, MyId, MyPort, AN, Leader);
        {tcp_close, NxtN} -> worker:server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader)
        after ?TIMEOUT -> worker:server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader)
        end;
    {tcp_close, NxtN} -> worker:server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader)
    after ?TIMEOUT -> worker:server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader)
    end.

setPrv(Ip, Port, Prv, Nxt, MyShare, MyId, MyPort, AN, Leader) ->
    {ok,PrvN}=gen_tcp:connect(Ip, Port, [binary, {active,true}, {packet, 4}]),
    gen_tcp:send(PrvN, cmd:make(["OKPRV"])),
    gen_tcp:close(Prv),
    worker:server(PrvN, Nxt, MyShare, MyId, MyPort, AN, Leader).


