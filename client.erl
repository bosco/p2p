%%% All our client functions go in here, including the main loop

-module(client).
-export([client_loop/3]).
-include("p2p.hrl").

client_get_peers(Peer, Pid) ->
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
		Peers =  client_get_peers(Peer, server),
		io:format("~p~n", [Peers]),
		New_peers = Peers,
		server ! #update_peers{cookie=Secret_cookie, peers=New_peers}
	end,
	client_loop(Peer, Secret_cookie).
