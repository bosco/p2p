%%% All our client functions go in here, including the main loop

-module(client).
-export([client_loop/3, find_closest/3, get_peers/2]).
-include("p2p.hrl").

find_closest(Peer, Pid, Id) ->
	Cookie = calc:cookie(),
	Pid ! #find_closest{cookie=Cookie, mypid=self(), peer=Peer, id=Id},
	receive
		#find_closest_response{cookie=Cookie, result=Result} ->
			Result
	after ?client_timeout ->
		io:format("~p client_find_closest(~p, ~p) timed out~n", [self(), Peer, Pid])
	end.

get_peers(Peer, Pid) ->
	Cookie = calc:cookie(),
	Pid ! #get_peers{cookie=Cookie, mypid=self(), peer=Peer},
	receive
		#get_peers_response{cookie=Cookie, peers=Peers} ->
			Peers
	after ?client_timeout ->
		io:format("~p client_get_peers(~p, ~p) timed out~n", [self(), Peer, Pid])
	end.

client_loop(Id, Secret_cookie, Server_pid) ->
	Peer = #peer{id=Id, server_pid=Server_pid, client_pid=self()},
	client_loop(Peer, Secret_cookie).
client_loop(Peer, Secret_cookie) ->
	receive
		{get_peers, Pid} ->
			Peers =  get_peers(Peer, Peer#peer.server_pid),
			Pid ! Peers;
		{find_peer, Pid, Id} ->
			Peers =  get_peers(Peer, Peer#peer.server_pid),
%%%			Pid ! peer:closest_peer_in_network(Peer, Peers, Id)
			Closest_peer = peer:closest_peer(Id, Peers),
			Pid ! Closest_peer;
		{refresh, Pid} ->
			Peers =  get_peers(Peer, Peer#peer.server_pid),
			New_peers = peer:create_peers(Peer, Peers),
			Peer#peer.server_pid ! #update_peers{cookie=Secret_cookie, peers=New_peers},
			Pid ! done
	after ?refresh ->
		Peers =  get_peers(Peer, Peer#peer.server_pid),
		New_peers = peer:create_peers(Peer, Peers),
		Peer#peer.server_pid ! #update_peers{cookie=Secret_cookie, peers=New_peers}
	end,
	client_loop(Peer, Secret_cookie).
