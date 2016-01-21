%%% All our general stuff dealing with making peers, maintaining peers lists, etc.

-module(peer).
-export([create_peer/0, add_peer/2, closest_peer/2, closest_peer_in_network/3]).
-include("p2p.hrl").

%%% Adds a peer to our local peers tree
%%% This will not allow you to add duplicate peers
%%% It also will not allow you to have more than 32 peers
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
	recursive_find_closest(Peer, Id, nil, Result).

recursive_find_closest(_, _, Result, Result) ->
	Result;
recursive_find_closest(Peer, Id, _, Result) ->
	New_Result = client:find_closest(Peer, Result#peer.server_pid, Id),
	recursive_find_closest(Peer, Id, Result, New_Result).

%%% Creates the first peer in our p2p network
create_peer() ->
	Secret_cookie = calc:cookie(),
	Id = calc:id(),
	Empty_peers = gb_trees:empty(),

	%%% Start up the server process ASAP because we can get queries right after the join
	%%% we have a secret_cookie with the server process so only we can update its
	%%% peer list also we can't add ourselves as a peer until we get the Server_pid back
	Server_pid = spawn(server, server_loop, [Empty_peers, Secret_cookie]),

	%%% Start up the client process too, because we need it's PID to add ourselves as a peer
	Client_pid = spawn(client, client_loop, [Id, Secret_cookie, Server_pid]),

	%%% This first peer in our tree is going to be ourselves
	Peers = add_peer(#peer{id=Id, server_pid=Server_pid, client_pid=Client_pid}, Empty_peers),
	Server_pid ! #update_peers{cookie=Secret_cookie, peers=Peers},

	#peer{id=Id, server_pid=Server_pid, client_pid=Client_pid}.
