-module(p2p).

-export([create_peer/0, create_peer/1, server_loop/3, client_loop/3, create_network/1, create_supervisor/1, supervisor_loop/1]).

%%% Based on: https://www.youtube.com/watch?v=kXyVqk3EbwE

%%% Gives us a random number we can use in our request strings
%%% to make sure servers are responding to us
create_cookie() -> 
	rand:uniform(4294967296).

%%% Gives a random number between 1 and 2**32
%%% there has to be a nicer way to do this, like
%%% with a constant or something
create_id() ->
	rand:uniform(4294967296).

%%% Tells you the distance between two peers. Uses XOR like kademlia
calculate_distance(Peer1_id, Peer2_id) ->
	Peer1_id bxor Peer2_id.

%%% Add the distance from ourselves to a peerl list so we can print it out
add_distance_to_peer_list([], _, New_list) ->
	New_list;
add_distance_to_peer_list([{Peer_id, Peer_pid} | Tail], Id, New_list) ->
	add_distance_to_peer_list(Tail, Id, [{Peer_id, Peer_pid, calculate_distance(Peer_id, Id)} | New_list]).

%%% Prints out a nice ordered list of known peers and their distance
print_peer_list(Peers, Id) ->
	Sorted_list = lists:keysort(3, add_distance_to_peer_list(Peers, Id, [])),
	io:format("=== Peers ===~n{ID, Erlang PID, Distance}~n", []),
	lists:foreach(fun(Peer) -> io:format("~p~n", [Peer]) end, Sorted_list).  
%%% Adds a peer to the known peers list, keystore will NOT allow duplicate IDs, will NOT allow more than 32 peers
add_known_peer({Peer_id, Peer_pid}, Known_peers) ->
	if
	       	length(Known_peers) < 32 -> %% This should be a constant
			lists:keystore(Peer_id, 1, Known_peers, {Peer_id, Peer_pid});
		true ->
			Known_peers
	end.

%%% Same thing as the above function, but it does it for an array of Peers
add_known_peers([], Known_peers) ->
	Known_peers;
add_known_peers([Peer | Tail], Known_peers) ->
	add_known_peers(Tail, add_known_peer(Peer, Known_peers)).

%%% This gives you the ID and PID of the closest peer we can find in our Known_peers
%%% this could probably be in a tree, making searching faster
find_closest_peer_local(Peer1_id, [{Peer2_id, Peer2_pid} | Tail]) ->
	find_closest_peer_local(Peer1_id, {Peer2_id, Peer2_pid}, calculate_distance(Peer1_id, Peer2_id), Tail).
find_closest_peer_local(_, Closest_peer, _, []) ->
	Closest_peer;
find_closest_peer_local(Peer1_id, Closest_peer, Smallest_distance, [{Peer2_id, Peer2_pid} | Tail]) ->
	Distance = calculate_distance(Peer1_id, Peer2_id),
	if Distance < Smallest_distance ->
		   find_closest_peer_local(Peer1_id, {Peer2_id, Peer2_pid}, Distance, Tail);
	true ->
		   find_closest_peer_local(Peer1_id, Closest_peer, Smallest_distance, Tail)
	end.

%%% This gives you a list of the best peer IDs we could have in our Known_ids table like kademlia
calc_optimal_peer_ids(Id) ->
	calc_optimal_peer_ids(Id, 1, [Id]). %% The first entry is always ourselves
calc_optimal_peer_ids(_, 2147483648, Optimal_peer_ids) -> %% Stop when we have 32 entries (there has to be a nicer way to do this)
	Optimal_peer_ids;
calc_optimal_peer_ids(Id, Distance, Optimal_peer_ids) ->
	calc_optimal_peer_ids(Id, Distance * 2, [Id bxor Distance | Optimal_peer_ids]). %% Double the distance each time (would a shift me more efficient?)

%%% This recursively queries peers to find a certain ID until the peer returns
%%% itself as the closest
client_find_closest({Id, Server_pid}, Search_id, {Peer_id, Peer_pid}) ->
	Cookie = create_cookie(),
	Peer_pid ! {find_closest, self(), {Id, Server_pid}, Search_id, Cookie},
	receive
		{{Result_id, Result_pid}, Cookie} ->
			if
				Result_id == Peer_id -> %% Bad things could happen here with bogus input
					{Result_id, Result_pid};
				true ->
%%%					io:format("Digging deeper~n", []),
					client_find_closest({Id, Server_pid}, Search_id, {Result_id, Result_pid})
			end
	after 2000 -> %% should be a constant
		io:format("~p Timed out looking for closest peer.~n", [self()]),
		{Peer_id, Peer_pid} %% this will cause issues when a peer legitimately dies
	end.

%%% This reaches out to the closest remote peer we have in our Known hosts and asks it what its
%%% closest peer to the Peer_id is
find_closest_peer_remote(Peer1_id, Known_peers, {Id, Server_pid}) ->
	{Peer2_id, Peer2_pid} = find_closest_peer_local(Peer1_id, Known_peers),
	if
		Peer2_id == Id -> %%% Don't remote search for yourself
			{Peer2_id, Peer2_pid};
		true ->
			client_find_closest({Id, Server_pid}, Peer1_id, {Peer2_id, Peer2_pid})
	end.

%%% This just generates a list, using find_closest_peer_remote
make_new_known_peers([], _, _, New_known_peers) ->
	New_known_peers;
make_new_known_peers([Peer_id | Optimal_peer_ids], Known_peers, {Id, Server_pid}, New_known_peers) ->
	{Closest_peer_id, Closest_peer_pid} = find_closest_peer_remote(Peer_id, Known_peers, {Id, Server_pid}), 
	Result = lists:keystore(Closest_peer_id, 1, New_known_peers, {Closest_peer_id, Closest_peer_pid}),
	make_new_known_peers(Optimal_peer_ids, Known_peers, {Id, Server_pid}, Result).
	
%%% Reach out to a peer to get their Known_peers list
client_get_known_peers(Peer_pid, Id) ->
	Cookie = create_cookie(),
	Peer_pid ! {get_known_peers, self(), Cookie},
	receive %% Needs timeout condition
		{Received_peers, Cookie} ->
			Received_peers
	end.

%%% This adds any other peers that we know about to open spots in Known_peers
%%% they may not be optimal, but we might as well use the knowledge
%%% add_known_peer will automatically prevent duplicates and keep our
%%% Known_peers length to 32
pad_peer_list([], New_list) ->
	New_list;
pad_peer_list([Head|Tail], New_list) ->
	pad_peer_list(Tail, add_known_peer(Head, New_list)).


%%% Go through the peers we have looking for optimal peers
refresh_known_peers(Known_peers, {Id, Server_pid}) ->
	Optimal_peer_ids = calc_optimal_peer_ids(Id),
	New_known_peers = make_new_known_peers(Optimal_peer_ids, Known_peers, {Id, Server_pid}, []),
	Padded_new_known_peers = pad_peer_list(New_known_peers, Known_peers),
%%%	io:format("~p has ~p peers in its list~n", [Id, length(Padded_new_known_peers)]),
	Padded_new_known_peers.

%%% Creates the first peer in our p2p network
create_peer() ->
	%%% Start up the server process ASAP because we can get queries right after the join
	Our_cookie = create_cookie(),
	Id = create_id(),
	io:format("Initial ID is ~p~n", [Id]),
	Server_pid = spawn(p2p, server_loop, [[], Id, Our_cookie]),

	%%% This first peer in our list is going to be ourselves
	Initial_peers = add_known_peer({Id, Server_pid}, []),
	Server_pid ! {update_known_peers, Initial_peers, Our_cookie},

	%%% Run the client functions in its own process
	Client_pid = spawn(p2p, client_loop, [Id, Our_cookie, Server_pid]),

	{Id, Server_pid, Client_pid}.

%%% Creates every other peer in our p2p network
create_peer(Peer_pid) ->
	%%% Start up the server process ASAP because we can get queries right after the join
	Our_cookie = create_cookie(),
	Id = create_id(),
	io:format("My ID is ~p~n", [Id]),
	Server_pid = spawn(p2p, server_loop, [[], Id, Our_cookie]),

	%%% This first peer in our list is going to be ourselves
	Initial_peers = add_known_peer({Id, Server_pid}, []),
	Server_pid ! {update_known_peers, Initial_peers, Our_cookie},

	%%% Ok, now tell the peer we're joining the network
	Peer_pid ! {join, Server_pid, Id},

	%%% query a peer for their peers
	Received_peers = client_get_known_peers(Peer_pid, Id),

	%%% add them to our list
	Known_peers = add_known_peers(Received_peers, Initial_peers),

	%%% lastly we look for optimal peers and put those in the list
	New_known_peers = refresh_known_peers(Known_peers, {Id, Server_pid}),

	%%% Push our new and improved list out to the sever process
	Server_pid ! {update_known_peers, New_known_peers, Our_cookie},

	%%% Run the client functions in its own process
	Client_pid = spawn(p2p, client_loop, [Id, Our_cookie, Server_pid]),

	{Id, Server_pid, Client_pid}.

%%% Spawns a bunch of nodes to make up a p2p network
%%% Be careful because if you create_peers too fast they won't
%%% have time to pull down a Known_peers list from their Peer
%%% and you'll end up with poor connectivity
create_network(N) ->
	{Id, Server_pid, Client_pid} = create_peer(),
	io:format("create_peer() returned {~p, ~p, ~p}~n", [Id, Server_pid, Client_pid]),
	timer:sleep(100),
	create_network(N - 1, [{Id, Server_pid, Client_pid}]).
create_network(0, Results) ->
	Results;
create_network(N, Results) ->
	{_, Random_server_pid, _} = lists:nth(rand:uniform(length(Results)), Results),
	{Id, Server_pid, Client_pid} = create_peer(Random_server_pid),
	io:format("create_peer(~p) returned {~p, ~p, ~p}~n", [Random_server_pid, Id, Server_pid, Client_pid]),
	timer:sleep(100),
	create_network(N - 1, [{Id, Server_pid, Client_pid} | Results]).

%%% Creates a network and collects stats 
create_supervisor(N) ->
	Ids = create_network(N),
	spawn(p2p, supervisor_loop, [Ids]).

%%% This makes a conversion table between our ids and the TGF ids
%%% TGF Ids start at one
make_id_to_tgfid_list(Ids) ->
	make_id_to_tgfid_list(Ids, [], 1).
make_id_to_tgfid_list([], List, _) ->
	List;
make_id_to_tgfid_list([{Id, Server_pid, Client_pid}|Tail], List, TGF_id) ->
	New_list = lists:keystore(Id, 2, List, {TGF_id, Id}),
	make_id_to_tgfid_list(Tail, New_list, TGF_id + 1).

%%% Takes a peer list, looks up the Ids and makes a TGF connection
%%% for each one
convert_peer_list_to_connections_list(Id, Peer_list, TGFid_list) ->
	convert_peer_list_to_connections_list(Id, Peer_list, TGFid_list, []).
convert_peer_list_to_connections_list(_, [], _, List) ->
	List;
convert_peer_list_to_connections_list(Id, [{Peer_id, Peer_pid}|Tail], TGFid_list, List) ->
	{TGFid_1, Id} = lists:keyfind(Id, 2, TGFid_list),
	{TGFid_2, Peer_id} = lists:keyfind(Peer_id, 2, TGFid_list),
	convert_peer_list_to_connections_list(Id, Tail, TGFid_list, [{TGFid_1, TGFid_2}|List]).
	
dump_peers([]) ->
	done;
dump_peers([{Id, Server_pid, Client_pid}|Tail]) ->
	Known_peers = client_get_known_peers(Server_pid, Id),
	{ok, Stream} = file:open("/tmp/test.txt", write),
	io:format(Stream, "~p~n", [Known_peers]),
	file:close(Stream),
	dump_peers(Tail).

supervisor_loop(Ids) ->
	receive
		dump_peers ->
			io:format("Dumping peers to the filesystem~n", []),
			TGFid_list = make_id_to_tgfid_list(Ids),
			io:format("Conversion list ~p~n", [TGFid_list]),
			{Id, Server_pid, Client_pid} = lists:nth(2, Ids),
			Known_peers =  client_get_known_peers(Server_pid, Id),
			io:format("Connections list ~p~n", [convert_peer_list_to_connections_list(Id, Known_peers, TGFid_list)]) 
	end,
	supervisor_loop(Ids).

client_loop(Id, Our_cookie, Server_pid) ->
	receive
		{get_pid_of, Peer_id} ->
			Known_peers =  client_get_known_peers(Server_pid, Id),
			Peer = find_closest_peer_remote(Peer_id, Known_peers, {Id, Server_pid}),
			io:format("The closest peer to ~p is ~p~n", [Peer_id, Peer]),
			New_known_peers = Known_peers
	after 100 -> %% Should be a constant
		Known_peers =  client_get_known_peers(Server_pid, Id),
		New_known_peers = refresh_known_peers(Known_peers, {Id, Server_pid})
	end,
	Server_pid ! {update_known_peers, New_known_peers, Our_cookie},
	client_loop(Id, Our_cookie, Server_pid).

server_loop(Known_peers, Id, Our_cookie) ->
	receive
		{join, Peer_pid, Peer_id}  ->
			%% Should we check this ID for collisions?
			New_known_peers = add_known_peer({Peer_id, Peer_pid}, Known_peers);
		{leave, Peer_pid} ->
			New_known_peers = Known_peers;
		{get_known_peers, Peer_pid, Cookie} ->
			Peer_pid ! {Known_peers, Cookie},	
			New_known_peers = Known_peers;
		{find_closest, Peer_pid, {Peer_id, Server_pid}, Search_id, Cookie} ->
			Result = find_closest_peer_local(Search_id, Known_peers),
			Peer_pid ! {Result, Cookie},
			New_known_peers = add_known_peer({Peer_id, Server_pid}, Known_peers);
		{update_known_peers, Received_peers, Our_cookie} -> %% this should only be called by our client process
			New_known_peers = Received_peers
	end,
	server_loop(New_known_peers, Id, Our_cookie).
