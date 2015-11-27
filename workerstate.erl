-module(workerstate).
-include("workerstate.hrl").
-include("logging.hrl").
-export([create/2, set_leader/1, unset_leader/1, set_prv/2, set_nxt/2, increment_numpaq/1, reset_start_time/1, add_client/2, remove_client/2]).

create(Id, Port) ->
    #wstate{id=Id, port=Port, clients=[], numpaq=0}.

set_leader(St) ->
    St#wstate{isleader = true}.
unset_leader(St) ->  
    St#wstate{isleader = false}.  

set_prv(Prv, St) ->
    St#wstate{prv = Prv}.
set_nxt(Nxt, St) ->
    St#wstate{nxt = Nxt}.

increment_numpaq(St) ->
    St#wstate{numpaq = St#wstate.numpaq+1}.
    
reset_start_time(St) ->
    St#wstate{start_time = erlang:monotonic_time()}.


add_client(Client, St) ->
    St#wstate{clients = [Client|St#wstate.clients]}.
remove_client(Client, St) ->
    St#wstate{clients = St#wstate.clients--[Client]}.
    
