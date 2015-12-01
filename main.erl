-module(main).
-include("logging.hrl").
-export([start/0, start/1, start/2]).

-define(DEFPORT,41581).
-define(DEFFOLDER, "server").

generate_id(Port, Folder) ->
    erlang:phash2({Port, Folder, os:system_time(milli_seconds)}).
    
%main entry: inicia todos los subsistemas
start(MyPort, Folder) when is_integer(MyPort) ->
    %this seed is used to generate random file descriptors
    random:seed(erlang:phash2(node()), erlang:monotonic_time(), erlang:unique_integer()),
    
    Port = worker:prepare_socket(MyPort),
    MyId = generate_id(Port, Folder),
    link(cache:start()),
    link(fs:start(fsstate:create(MyId, Folder))),
    Worker=worker:start(workerstate:create(MyId, Port)),
    link(Worker),
    link(megaphone:start(Port, Worker)),
    ?INFO("Started worker at port ~p with id ~p, using storage folder ~p", [Port, MyId, Folder]),
    
    %prints periodically how many workers are in the ring:
    %~ timer:apply_interval(4000, erlang, apply, [fun() -> ?DF("~p workers.", [fs:count()]) end, []]),
    
    %error messages coming from the links are trapped here:
    receive {'EXIT',_,Reason} -> exit(?ERROR("~p", [Reason])) end;
    
%formas alternativas de invocar el programa   
start(MyPortS, Folder) when is_list(MyPortS) ->%transformar a entero
    MyPort = try list_to_integer(MyPortS) catch _:_ -> usage() end,
    start(MyPort, Folder).
    
start() -> start(0, ?DEFFOLDER).
start([MyPortS]) -> start(MyPortS, ?DEFFOLDER);
start([MyPortS, Folder]) -> start(MyPortS, Folder);
start(_) -> usage().
    
usage() ->
    io:format(
"usage: worker [Folder] [Port]
(default Folder is "++?DEFFOLDER++")
"), exit(1).
