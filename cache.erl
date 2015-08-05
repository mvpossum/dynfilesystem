-module(cache).
-include("logging.hrl").
-export([server/0,server/1]).

-define(ALIVE_CACHE, 60*60*24).

server() -> server(#{}).
server(Cache) ->
    receive
    {put, Id, N, Val} ->
        server(Cache#{Id => {N, erlang:monotonic_time(), Val}});
    {get, From, Id, N} ->
        case maps:get(Id, Cache, nokey) of
        {N, _, Val} -> From!{ok, Val};
        _ -> From!nocached
        end, server(Cache);
    cleanup ->
		Pred = fun(_,{_, Time, _}) -> ?ALIVE_CACHE>=erlang:convert_time_unit(erlang:monotonic_time()-Time, native, seconds) end,
        server(maps:filter(Pred,Cache))
    end.
