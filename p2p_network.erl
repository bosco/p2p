-module(p2p_network).
-export([create_supervisor/1, supervisor_loop/1]).
-include("p2p.hrl").

%%% Spawns a bunch of nodes to make up a p2p network
%%% Be careful because if you create_peers too fast they won't
%%% have time to pull down a Known_peers list from their Peer
%%% and you'll end up with poor connectivity
create_network(N) ->
	Ids = p2p:create_peer(),
	timer:sleep(100),
	create_network(N - 1, [Ids]).
create_network(0, Results) ->
	Results;
create_network(N, Results) ->
	Random_Id = lists:nth(rand:uniform(length(Results)), Results),
	Ids = p2p:create_peer(Random_Id#ids.server_pid),
	timer:sleep(100),
	create_network(N - 1, [Ids | Results]).

%%% Creates a network with a supervisor 
create_supervisor(N) ->
	Ids = create_network(N),
	register(supervisor, spawn(?MODULE, supervisor_loop, [Ids])),
	io:format("Network created and supervisor registered as supervisor.~n").

%%% This makes a conversion table between our ids and the TGF ids
%%% TGF Ids start at one
make_id_to_tgfid_list(Ids_list) ->
	make_id_to_tgfid_list(Ids_list, [], 1).
make_id_to_tgfid_list([], List, _) ->
	List;
make_id_to_tgfid_list([Ids|Tail], List, TGF_id) ->
	New_list = lists:keystore(Ids#ids.id, 2, List, {TGF_id, Ids#ids.id}),
	make_id_to_tgfid_list(Tail, New_list, TGF_id + 1).

%%% Takes a peer list, looks up the Ids and makes a TGF connection
%%% for each one
convert_peer_list_to_connections_list(Id, Peer_list, TGFid_list) ->
	convert_peer_list_to_connections_list(Id, Peer_list, TGFid_list, []).
convert_peer_list_to_connections_list(_, [], _, List) ->
	List;
convert_peer_list_to_connections_list(Id, [{Peer_id, _}|Tail], TGFid_list, List) ->
	{TGFid_1, Id} = lists:keyfind(Id, 2, TGFid_list),
	{TGFid_2, Peer_id} = lists:keyfind(Peer_id, 2, TGFid_list),
	convert_peer_list_to_connections_list(Id, Tail, TGFid_list, [{TGFid_1, TGFid_2}|List]).

%%% Grabs a peer list from every peer, converts it to a TGF connection list
%%% and appends all those TGF connection lists together
get_network_connections(Ids_list, TGFid_list) ->
	get_network_connections(Ids_list, TGFid_list, []).
get_network_connections([], _, Connections) ->
	Connections;
get_network_connections([Ids|Tail], TGFid_list, Connections) ->
	Known_peers =  p2p:client_get_known_peers(Ids#ids.server_pid, Ids#ids.id),
	New_connections = convert_peer_list_to_connections_list(Ids#ids.id, Known_peers, TGFid_list),
	get_network_connections(Tail, TGFid_list, lists:append(Connections, New_connections)).
	
%%%dump_peers([]) ->
%%%	done;
%%%dump_peers([{Id, Server_pid, Client_pid}|Tail]) ->
%%%	Known_peers = client_get_known_peers(Server_pid, Id),
%%%	{ok, Stream} = file:open("/tmp/test.txt", write),
%%%	io:format(Stream, "~p~n", [Known_peers]),
%%%	file:close(Stream),
%%%	dump_peers(Tail).

supervisor_loop(Ids_list) ->
	receive
		output_tgf ->
			io:format("Making ID to TGFID list...~n", []),
			TGFid_list = make_id_to_tgfid_list(Ids_list),
			io:format("Making connections list...~n", []),
			Connections = get_network_connections(Ids_list, TGFid_list),
			{ok, Stream} = file:open("output.tgf", write),
			lists:foreach(fun({TGFid, Id}) -> io:format(Stream, "~p ~p~n", [TGFid, Id]) end, TGFid_list),
			io:format(Stream, "#~n", []),
			lists:foreach(fun({Id1, Id2}) -> io:format(Stream, "~p ~p~n", [Id1, Id2]) end, Connections),
			file:close(Stream)
	end,
	supervisor_loop(Ids_list).
