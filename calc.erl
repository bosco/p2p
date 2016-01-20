%%% This file has general calculation functions in it

-module(calc).
-export([cookie/0, id/0, best_peers/1, distance/2]).
-include("p2p.hrl").

%%% Gives us a random number we can use in our request strings
%%% to make sure servers are responding to us
cookie() -> 
	rand:uniform(?max_cookie).

%%% Gives a random SHA1 hash converted to a 160 bit integer
id() ->
	<<Id:?id_size/integer>> = crypto:hash(sha, crypto:rand_bytes(20)),
	Id.

%%% Tells you the distance between two peers. Uses XOR like kademlia
distance(Peer1_id, Peer2_id) ->
	Peer1_id bxor Peer2_id.

%%% This gives you a list of the best peer IDs we could have in our Peers tree
%%% your Peer tree should be the size of your hash in bits
best_peers(Id) ->
	best_peers(Id, 1, [Id], ?id_size - 1). %% The first entry is always ourselves
best_peers(_, _, Ids, 0) -> 
	Ids;
best_peers(Id, Distance, Ids, N) ->
	best_peers(Id, Distance * 2, [Id bxor Distance | Ids], N - 1).
