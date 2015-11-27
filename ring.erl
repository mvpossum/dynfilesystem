-module(ring).
-export([create/2, addWorker/4, join/5, setPrv/3]).
-include("logging.hrl").

-define(TIMEOUT,2000).

-define(PACKET_TYPE, {packet, 4}).

create(MyId, MyPort) ->
    {ok, Prv}=gen_tcp:connect("localhost", MyPort, [binary, {active,true}, ?PACKET_TYPE]),
    ok=gen_tcp:send(Prv, cmd:make(["IMNXT"])),
    receive {tcp, Nxt, <<"IMNXT">>} -> ok end,
    announcer!{enable, MyId},
    {ok, Prv, Nxt}.

addWorker(NxtN, Port, Prv, Nxt) ->
    case inet:peername(NxtN) of
    {ok,{Ip,_}} ->
        SetPrv=cmd:flat(cmd:make(["SETPRV", Ip, Port])),
        gen_tcp:send(Nxt, SetPrv),
        receive
        {tcp, NxtN, <<"OKWORK">>} ->
            gen_tcp:close(Nxt),
            {ok, Prv, NxtN};
        {tcp, Prv, SetPrv} ->
            {ok,PrvN}=gen_tcp:connect(Ip, Port, [binary, {active,true}, ?PACKET_TYPE]),
            gen_tcp:send(PrvN, cmd:make(["OKPRV"])),
            receive
            {tcp, NxtN, <<"OKWORK">>} ->
                [gen_tcp:close(Con) || Con <- [Prv, Nxt]],
                {ok, PrvN, NxtN};
            {tcp_close, NxtN} ->
                gen_tcp:close(PrvN), continue; 
            {tcp_close, PrvN} ->
                gen_tcp:close(NxtN), continue;
            {tcp_close, Prv} ->
                [gen_tcp:close(Con) || Con <- [Nxt, PrvN, NxtN]],
                reset;
            {tcp_close, Nxt} ->
                [gen_tcp:close(Con) || Con <- [Prv, PrvN, NxtN]],
                reset
            after ?TIMEOUT ->
                [gen_tcp:close(Con) || Con <- [PrvN, NxtN]],
                continue
            end;
        {tcp_close, NxtN} -> continue;
        {tcp_close, Prv} ->
            [gen_tcp:close(Con) || Con <- [Nxt, NxtN]],
            reset;
        {tcp_close, Nxt} ->
            [gen_tcp:close(Con) || Con <- [Prv, NxtN]],
            reset
        after ?TIMEOUT -> gen_tcp:close(NxtN), continue
        end;
    {error, _} -> gen_tcp:close(NxtN), continue
    end.

join(Ip, Port, Prv, Nxt, MyPort) ->
    case gen_tcp:connect(Ip, Port, [binary, ?PACKET_TYPE]) of
    {ok, PrvN} ->
        gen_tcp:send(PrvN, cmd:make(["WORK", MyPort])),
        receive
        {tcp, NxtN, <<"OKPRV">>} ->
            announcer!disable,
            gen_tcp:close(Prv), gen_tcp:close(Nxt),
            gen_tcp:send(PrvN, cmd:make(["OKWORK"])),
            {ok, PrvN, NxtN};
        {tcp_close, PrvN} -> continue
        after ?TIMEOUT -> gen_tcp:close(PrvN), continue
        end;
    {error, _} -> continue
    end.

setPrv(Ip, Port, Prv) ->
    case gen_tcp:connect(Ip, Port, [binary, ?PACKET_TYPE]) of
    {ok,PrvN} ->
        ok=gen_tcp:send(PrvN, cmd:make(["OKPRV"])),
        gen_tcp:close(Prv),
        {ok,PrvN};
    {error, _} -> continue
    end.
