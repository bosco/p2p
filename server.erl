%%% Anything that just the server does goes in here

-module(server).
-export([server_loop/2]).
-include("p2p.hrl").

server_loop(Peers, Secret_cookie) ->
	receive
		#get_peers{mypid=Pid, cookie=Cookie, peer=Peer} ->
			Pid ! #get_peers_response{peers=Peers, cookie=Cookie},
			New_peers = peer:add_peer(Peer, Peers);
		#find_closest{mypid=Pid, cookie=Cookie, peer=Peer, id=Id} ->
			Result = peer:closest_peer(Id, Peers),
			Pid ! #find_closest_response{result=Result, cookie=Cookie},
			New_peers = peer:add_peer(Peer, Peers);
		#update_peers{peers=Received_peers, cookie=Secret_cookie} ->
			New_peers = Received_peers
	end,
	server_loop(New_peers, Secret_cookie).
