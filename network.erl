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

%%% We do this traceroute ourselves rather than using closest_in_network so that
%%% our info doesn't end up being added to the server's peer list
trace_route(_, _, 0) ->
	%%io:format("Maximum hops exceeded.~n", []),
	0;
trace_route(Peer, Id, _) when Peer#peer.id == Id ->
	%%io:format("Found ~p~n", [Id]),
	1;
trace_route(Peer, Id, Hops) ->
	Peer#peer.client_pid ! {find_peer, self(), Id},
	receive
		Result when Result == Peer ->
			%%io:format("This is as close as we get.~n", []),
			0;
		Result ->
			%%io:format("[~p] ~p -> ~p~n", [Hops, Peer#peer.id, Result#peer.id]),
			trace_route(Result, Id, Hops - 1) 
	end.

peer_status(Peer, Ids_list) ->
	%% If we get_peers from the server, it adds our info to the peer list
	%% hence we get_peers from the client
	Peer#peer.client_pid ! {get_peers, self()},
	receive
		Peers ->
			Peers_list = gb_trees:to_list(Peers),
			Peer_count = length(Peers_list),
			io:format("~p has ~p peers in its tree.~n", [Peer#peer.id, Peer_count])
	end,
	Total_connections = lists:foldl(fun(Peer2, Sum) -> trace_route(Peer, Peer2#peer.id, 8) + Sum end, 0, Ids_list),
	Peers_in_network = length(Ids_list),
	Percentage = trunc(Total_connections / Peers_in_network * 100),
	io:format("~p is connected to ~p/~p peers ~p%~n", [Peer#peer.id, Total_connections, Peers_in_network, Percentage]),
	Percentage.

%%% This makes a conversion table between our ids and the TGF ids
%%% TGF Ids start at one
make_id_to_tgfid_list(Ids_list) ->
	make_id_to_tgfid_list(Ids_list, [], 1).
make_id_to_tgfid_list([], List, _) ->
	List;
make_id_to_tgfid_list([Ids|Tail], List, TGF_id) ->
	New_list = lists:keystore(Ids#peer.id, 2, List, {TGF_id, Ids#peer.id}),
	make_id_to_tgfid_list(Tail, New_list, TGF_id + 1).

%%% Takes a Peers tree, looks up the Ids and makes a TGF connection
%%% for each one
convert_peers_to_connections_list(Id, Peers, TGFid_list) ->
	convert_peers_to_connections_list(Id, gb_trees:to_list(Peers), TGFid_list, []).
convert_peers_to_connections_list(_, [], _, List) ->
	List;
convert_peers_to_connections_list(Id, [{Peer_id, _}|Tail], TGFid_list, List) ->
	{TGFid_1, Id} = lists:keyfind(Id, 2, TGFid_list),
	{TGFid_2, Peer_id} = lists:keyfind(Peer_id, 2, TGFid_list),
	convert_peers_to_connections_list(Id, Tail, TGFid_list, [{TGFid_1, TGFid_2}|List]).

%%% Grabs a Peers tree from every peer, converts it to a TGF connection list
%%% and appends all those TGF connection lists together
get_network_connections(Ids_list, TGFid_list) ->
	get_network_connections(Ids_list, TGFid_list, []).
get_network_connections([], _, Connections) ->
	Connections;
get_network_connections([Ids|Tail], TGFid_list, Connections) ->
	Ids#peer.client_pid ! {get_peers, self()},
	receive
		Peers ->
			New_connections = convert_peers_to_connections_list(Ids#peer.id, Peers, TGFid_list),
			get_network_connections(Tail, TGFid_list, lists:append(Connections, New_connections))
	end.

output_tgf(Ids_list, Filename) ->
	TGFid_list = make_id_to_tgfid_list(Ids_list),
	Connections = get_network_connections(Ids_list, TGFid_list),
	{ok, Stream} = file:open(Filename, write),
	lists:foreach(fun({TGFid, Id}) -> io:format(Stream, "~p ~p~n", [TGFid, Id]) end, TGFid_list),
	io:format(Stream, "#~n", []),
	lists:foreach(fun({Id1, Id2}) -> io:format(Stream, "~p ~p~n", [Id1, Id2]) end, Connections),
	file:close(Stream).

refresh_peers([]) ->
	done;
refresh_peers([Peer|Tail]) ->
	Peer#peer.client_pid ! {refresh, self()},
	receive
		done ->
			refresh_peers(Tail)
	end.

time_lapse(Ids_list, Steps) ->
	time_lapse(Ids_list, Steps, 1).
time_lapse(_, 0, _) ->
	done;
time_lapse(Ids_list, Step, Filenum) ->
	refresh_peers(Ids_list),
	Filename = lists:concat(["time-lapse-", Filenum, ".tgf"]),
	output_tgf(Ids_list, Filename),	
	time_lapse(Ids_list, Step - 1, Filenum + 1).

supervisor_start(Ids_list) ->
	lists:foreach(fun(Ids) -> link(Ids#peer.server_pid), link(Ids#peer.client_pid) end, Ids_list),
	supervisor_loop(Ids_list).
supervisor_loop(Ids_list) ->
	        receive
			die ->
				exit(die);
			net_status ->
				Percentage_sum = lists:foldl(fun(Peer, Sum) -> peer_status(Peer, Ids_list) + Sum end, 0, Ids_list),
				io:format("Connectivity percentage ~p%~n", [trunc(Percentage_sum / length(Ids_list))]);
			refresh_peers ->
				refresh_peers(Ids_list);
			{output_tgf, Filename} ->
				io:format("Writing network structure to ~p~n", [Filename]),
				output_tgf(Ids_list, Filename);
			{time_lapse, Steps} ->
				io:format("Doing ~p step time-lapse~n", [Steps]),
				time_lapse(Ids_list, Steps)
		end,
		supervisor_loop(Ids_list).
