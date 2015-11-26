-module(clientstate).
-include("clientstate.hrl").
-export([create/1, send/2, cleanup/1, get_file/2, open_file/2, close_file/2]).

create(Sock) ->
    random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
    gen_tcp:send(Sock, "OK ID 0"),
    #cstate{sock=Sock, openfiles=#{}}.


send(What, St) -> gen_tcp:send(St#cstate.sock, What).

cleanup(St) ->
    gen_tcp:close(St#cstate.sock),
    [fs:close(V) || V <- maps:values(St#cstate.openfiles)],
    worker!{client_closed, self()}.
get_file(File, St) ->
    maps:get(File, St#cstate.openfiles, notopen).


get_unique_desc(OFiles) ->%crea un nuevo numero de descriptor
    FD = random:uniform(900+maps:size(OFiles)),%poca prob de colision
    case maps:is_key(FD, OFiles) of
    true -> get_unique_desc(OFiles);%already exists(collision), repeat
    false -> FD
    end.
open_file(File, St) ->
    FD = get_unique_desc(St#cstate.openfiles),
    {St#cstate{openfiles = (St#cstate.openfiles)#{FD => File}}, FD}.
close_file(FD, St) ->
    St#cstate{openfiles = maps:remove(FD, St#cstate.openfiles)}.
