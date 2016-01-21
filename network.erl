%%% These are functions to start a peer network and run a supervisor

-module(network).
-export([create_supervisor/1, supervisor_start/1]).
-include("p2p.hrl").

%%% Spawns a bunch of nodes to make up a p2p network
%%% Be careful because if you create_peers too fast they won't
%%% have time to pull down a Known_peers list from their Peer
%%% and you'll end up with poor connectivity
create_network(N) ->
	Ids = peer:create_peer(nil),
	timer:sleep(100),
	create_network(N - 1, [Ids]).
create_network(0, Results) ->
	Results;
create_network(N, Results) ->
	Random_Id = lists:nth(rand:uniform(length(Results)), Results),
	Ids = peer:create_peer(Random_Id#peer.server_pid),
	timer:sleep(100),
	create_network(N - 1, [Ids | Results]).

%%% Creates a network with a supervisor 
create_supervisor(N) -> 
	Ids_list = create_network(N),
	register(supervisor, spawn(?MODULE, supervisor_start, [Ids_list])),
	io:format("Network created and supervisor registered as supervisor.~n").

supervisor_start(Ids_list) ->
	lists:foreach(fun(Ids) -> link(Ids#peer.server_pid), link(Ids#peer.client_pid) end, Ids_list),
	supervisor_loop(Ids_list).
supervisor_loop(Ids_list) ->
	        receive
			die ->
				exit(die)
		end,
		supervisor_loop(Ids_list).
