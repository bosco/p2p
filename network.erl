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

is_connected(Peer1, Peer2) ->
	Peer1#peer.client_pid ! {find_peer, self(), Peer2#peer.id},
	receive
		Result when Result == Peer2 ->
			1;
		_ ->
			0
	end.

peer_status(Peer, Ids_list) ->
	Peer#peer.client_pid ! {peer_count, self()},
	receive
		Peer_count ->
		io:format("~p has ~p peers in its tree.~n", [Peer#peer.id, Peer_count])
	end,
	Total_connections = lists:foldl(fun(Peer2, Sum) -> is_connected(Peer, Peer2) + Sum end, 0, Ids_list),
	Peers_in_network = length(Ids_list),
	Percentage = trunc(Total_connections / Peers_in_network * 100),
	io:format("~p is connected to ~p/~p peers ~p%~n", [Peer#peer.id, Total_connections, Peers_in_network, Percentage]). 

supervisor_start(Ids_list) ->
	lists:foreach(fun(Ids) -> link(Ids#peer.server_pid), link(Ids#peer.client_pid) end, Ids_list),
	supervisor_loop(Ids_list).
supervisor_loop(Ids_list) ->
	        receive
			die ->
				exit(die);
			test ->
				lists:foreach(fun(Peer) -> peer_status(Peer, Ids_list) end, Ids_list)
		end,
		supervisor_loop(Ids_list).
