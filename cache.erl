-module(cache).
-include("logging.hrl").
-export([start/0]).

-define(ALIVE_CACHE, 60*2).%2 mins

start() -> Pid = spawn(fun() -> handler(#{}) end), register(cache, Pid), Pid.
handler(Cache) ->
    receive
    {put, Id, N, Val} ->
        handler(Cache#{Id => {N, erlang:monotonic_time(), Val}});
    {get, From, Id, N} ->
        case maps:get(Id, Cache, nokey) of
        {N, _, Val} -> From!{ok, Val};
        _ -> From!nocached
        end, handler(Cache);
    cleanup ->
		Pred = fun(_,{_, CreatedTime, _}) -> ?ALIVE_CACHE>=elapsed_since(CreatedTime) end,
        handler(maps:filter(Pred,Cache))
    end.

elapsed_since(TimeStamp) ->
    erlang:convert_time_unit(erlang:monotonic_time()-TimeStamp, native, seconds).
