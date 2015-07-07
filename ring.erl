-module(ring).
-define(DEBUG,1).
-include("debug.hrl").
-export([create/4, addWorker/9, join/8, setPrv/9]).

-define(TIMEOUT,1000).

create(MyShare, MyId, MyPort, AN) ->
    {ok, Prv}=gen_tcp:connect("localhost", MyPort, [binary, {active,true}, {packet, 4}]),
    ok=gen_tcp:send(Prv, cmd:make(["IMNXT"])),
    receive {tcp, Nxt, <<"IMNXT">>} -> ok end,
    AN!enable,
    worker:server(Prv, Nxt, MyShare, MyId, MyPort, AN, true).

addWorker(NxtN, Port, Prv, Nxt, MyShare, MyId, MyPort, AN, Leader) ->
    case inet:peername(NxtN) of
    {ok,{Ip,_}} ->
        SetPrv=cmd:concat(cmd:make(["SETPRV", Ip, Port])),
        gen_tcp:send(Nxt, SetPrv),
        receive
        {tcp, NxtN, <<"OKWORK">>} ->
            gen_tcp:close(Nxt),
            worker:server(Prv, NxtN, MyShare, MyId, MyPort, AN, Leader);
        {tcp, Prv, SetPrv} ->
            {ok,PrvN}=gen_tcp:connect(Ip, Port, [binary, {active,true}, {packet, 4}]),
            gen_tcp:send(PrvN, cmd:make(["OKPRV"])),
            receive
            {tcp, NxtN, <<"OKWORK">>} ->
                lists:foreach(fun gen_tcp:close/1, [Prv, Nxt]),
                worker:server(PrvN, NxtN, MyShare, MyId, MyPort, AN, Leader);
            {tcp_close, NxtN} ->
                gen_tcp:close(PrvN), worker:server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader); 
            {tcp_close, PrvN} ->
                gen_tcp:close(NxtN), worker:server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
            {tcp_close, Prv} ->
                lists:foreach(fun gen_tcp:close/1, [Nxt, PrvN, NxtN]),
                worker:server(nocon, nocon, MyShare, MyId, MyPort, AN, Leader);
            {tcp_close, Nxt} ->
                lists:foreach(fun gen_tcp:close/1, [Prv, PrvN, NxtN]),
                worker:server(nocon, nocon, MyShare, MyId, MyPort, AN, Leader)
            after ?TIMEOUT ->
                lists:foreach(fun gen_tcp:close/1, [PrvN, NxtN]),
                worker:server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader)
            end;
        {tcp_close, NxtN} -> worker:server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader);
        {tcp_close, Prv} ->
            lists:foreach(fun gen_tcp:close/1, [Nxt, NxtN]),
            worker:server(nocon, nocon, MyShare, MyId, MyPort, AN, Leader);
        {tcp_close, Nxt} ->
            lists:foreach(fun gen_tcp:close/1, [Prv, NxtN]),
            worker:server(nocon, nocon, MyShare, MyId, MyPort, AN, Leader)
        after ?TIMEOUT -> gen_tcp:close(NxtN), worker:server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader)
        end;
    {error, _} -> gen_tcp:close(NxtN), worker:server(Prv, Nxt, MyShare, MyId, MyPort, AN, Leader)
    end.

join(Ip, Port, Prv, Nxt, MyShare, MyId, MyPort, AN) ->
    case gen_tcp:connect(Ip, Port, [binary, {packet, 4}]) of
    {ok, PrvN} ->
        AN!disable,
        gen_tcp:close(Prv), gen_tcp:close(Nxt),
        gen_tcp:send(PrvN, cmd:make(["WORK", MyPort])),
        receive
        {tcp, NxtN, <<"OKPRV">>} ->
            ok=gen_tcp:send(PrvN, cmd:make(["OKWORK"])),
            worker:server(PrvN, NxtN, MyShare, MyId, MyPort, AN, false);
        {tcp_close, PrvN} -> worker:server(nocon, nocon, MyShare, MyId, MyPort, AN, true)
        after ?TIMEOUT -> worker:server(nocon, nocon, MyShare, MyId, MyPort, AN, true)
        end;
    {error, _} -> worker:server(Prv, Nxt, MyShare, MyId, MyPort, AN, true)
    end.

setPrv(Ip, Port, Prv, Nxt, MyShare, MyId, MyPort, AN, Leader) ->
    case gen_tcp:connect(Ip, Port, [binary, {packet, 4}]) of
    {ok,PrvN} ->
        ok=gen_tcp:send(PrvN, cmd:make(["OKPRV"])),
        gen_tcp:close(Prv),
        worker:server(PrvN, Nxt, MyShare, MyId, MyPort, AN, Leader);
    {error, _} -> worker:server(Prv, Nxt, MyShare, MyId, MyPort, AN, true)
    end.


