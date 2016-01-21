%%% All our client functions go in here, including the main loop

-module(client).
-export([client_loop/3, find_closest/3]).
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
	register(server, Server_pid),
	Peer = #peer{id=Id, server_pid=Server_pid, client_pid=self()},
	client_loop(Peer, Secret_cookie).
client_loop(Peer, Secret_cookie) ->
	receive
	after ?refresh ->
		Peers =  get_peers(Peer, server),

		Test = peer:closest_peer_in_network(Peer, Peers, calc:id()),
		io:format("~p~n", [Test]),

		New_peers = Peers,
		server ! #update_peers{cookie=Secret_cookie, peers=New_peers}
	end,
	client_loop(Peer, Secret_cookie).
