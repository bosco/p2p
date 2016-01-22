%%% All our general stuff dealing with making peers, maintaining peers lists, etc.

-module(peer).
-export([create_peer/1, add_peer/2, closest_peer/2, closest_peer_in_network/3, create_peers/2]).
-include("p2p.hrl").

%%% Adds a peer to our local peers tree
%%% This will not allow you to add duplicate peers
%%% It also will not allow you to have more than ?id_size peers
add_peer(_, {Size, Tree}) when Size >= ?id_size ->
	{Size, Tree};
add_peer(Peer, Peers) ->
	gb_trees:enter(Peer#peer.id, {Peer#peer.server_pid, Peer#peer.client_pid}, Peers).

%%% Looks through our local peers tree to find the closest match
closest_peer(Search_key, {_, Tree}) ->
	closest_peer(Search_key, Tree);
closest_peer(Search_key, {Key, {Server_pid, Client_pid}, nil, _}) when Search_key < Key ->
	#peer{id=Key, server_pid=Server_pid, client_pid=Client_pid};
closest_peer(Search_key, {Key, {Server_pid, Client_pid}, _, nil}) when Search_key >= Key ->
	#peer{id=Key, server_pid=Server_pid, client_pid=Client_pid};
closest_peer(Search_key, {Key, _, Smaller, _}) when Search_key < Key ->
	closest_peer(Search_key, Smaller); 
closest_peer(Search_key, {Key, _, _, Bigger}) when Search_key >= Key ->
	closest_peer(Search_key, Bigger).

%%% Finds the closest peer in the whole network
%%% Peer is who we are (servers want to know this to add us)
%%% Peers is our peer tree
%%% Id is the Id we're looking for
closest_peer_in_network(Peer, _, Id) when Peer#peer.id == Id ->
	Peer; %% they searched for themselves
closest_peer_in_network(Peer, Peers, Id) ->
	%% Start in our tree
	Result = closest_peer(Id, Peers),
	if
		Result#peer.id == Id -> %% We found it locally
			Result;
		true ->
			recursive_find_closest(Peer, Id, nil, Result, ?max_hops)
	end.

recursive_find_closest(_, _, _, Result, 0) ->
	Result; %% They've exceeded the max hops (avoids loops)
recursive_find_closest(_, _, Result, Result, _) ->
	Result; %% This result and the previous one match
recursive_find_closest(Result, _, _, Result, _) ->
	Result; %% Don't query yourself
recursive_find_closest(Peer, Id, _, Result, Remaining_hops) ->
	New_result = client:find_closest(Peer, Result#peer.server_pid, Id),
	recursive_find_closest(Peer, Id, Result, New_result, Remaining_hops - 1).

%%% Creates a peers tree using another peers tree
%%% 1. find the best peers in the network using the old tree
%%% 2. put them in the new tree
%%% 3. put any remaining peers from the old tree in the new tree 
%%% Peer is us (needed for client:find_closest)
%%% Peers is our old Peer tree
%%% returns a new Peer tree
create_peers(Peer, Peers) ->
	Best_peers = calc:best_peers(Peer#peer.id),
	Best_peers_in_network = lists:map(fun(Id) -> closest_peer_in_network(Peer, Peers, Id) end, Best_peers),
	New_peers = lists:foldl(fun(Network_peer, New_peers) -> add_peer(Network_peer, New_peers) end, gb_trees:empty(), Best_peers_in_network),
	%% Convert our Peers tree to a list of peer records
	Peers_list = lists:foldl(fun({Id, {Server_id, Client_id}}, Peers_list) -> [#peer{id=Id, server_pid=Server_id, client_pid=Client_id}|Peers_list] end, [], gb_trees:to_list(Peers)),
	lists:foldl(fun(Old_peer, New_peers2) -> add_peer(Old_peer, New_peers2) end, New_peers, Peers_list).

%%% Creates the first peer in our p2p network
create_peer(Remote_pid) ->
	Secret_cookie = calc:cookie(),
	Id = calc:id(),
	Empty_peers = gb_trees:empty(),

	%%% Start up the server process ASAP because we can get queries right after the join
	%%% we have a secret_cookie with the server process so only we can update its
	%%% peer list also we can't add ourselves as a peer until we get the Server_pid back
	Server_pid = spawn(server, server_loop, [Empty_peers, Secret_cookie]),

	%%% Start up the client process too, because we need its PID to add ourselves as a peer
	Client_pid = spawn(client, client_loop, [Id, Secret_cookie, Server_pid]),

	%%% This first peer in our tree is going to be ourselves
	Peers = add_peer(#peer{id=Id, server_pid=Server_pid, client_pid=Client_pid}, Empty_peers),
	Server_pid ! #update_peers{cookie=Secret_cookie, peers=Peers},

	Peer = #peer{id=Id, server_pid=Server_pid, client_pid=Client_pid},

	%%% Bootstrap a Peers tree unless we're the first peer
	if
		Remote_pid /= nil ->
			Remote_peers =  client:get_peers(Peer, Remote_pid),
			New_peers = peer:create_peers(Peer, Remote_peers),
			Server_pid ! #update_peers{cookie=Secret_cookie, peers=New_peers},
			Peer;
		true ->
			Peer
	end.

