-module(p2p).
-export([create_peer/0, create_peer/1, server_loop/3, client_loop/3, client_get_known_peers/2]).
-include("p2p.hrl").

%%% Based on: https://www.youtube.com/watch?v=kXyVqk3EbwE

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
	find_closest_peer_local(Peer1_id, {Peer2_id, Peer2_pid}, calc:distance(Peer1_id, Peer2_id), Tail).
find_closest_peer_local(_, Closest_peer, _, []) ->
	Closest_peer;
find_closest_peer_local(Peer1_id, Closest_peer, Smallest_distance, [{Peer2_id, Peer2_pid} | Tail]) ->
	Distance = calc:distance(Peer1_id, Peer2_id),
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
	Cookie = calc:create_cookie(),
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
	Cookie = calc:create_cookie(),
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
	Our_cookie = calc:create_cookie(),
	Id = calc:create_id(),
	io:format("Initial ID is ~p~n", [Id]),
	Server_pid = spawn(p2p, server_loop, [[], Id, Our_cookie]),

	%%% This first peer in our list is going to be ourselves
	Initial_peers = add_known_peer({Id, Server_pid}, []),
	Server_pid ! {update_known_peers, Initial_peers, Our_cookie},

	%%% Run the client functions in its own process
	Client_pid = spawn(p2p, client_loop, [Id, Our_cookie, Server_pid]),

	#ids{id=Id, server_pid=Server_pid, client_pid=Client_pid}.

%%% Creates every other peer in our p2p network
create_peer(Peer_pid) ->
	%%% Start up the server process ASAP because we can get queries right after the join
	Our_cookie = calc:create_cookie(),
	Id = calc:create_id(),
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

	#ids{id=Id, server_pid=Server_pid, client_pid=Client_pid}.

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
