%%% This file has general calculation functions in it

-module(calc).
-export([create_cookie/0, create_id/0, distance/2]).
-include("p2p.hrl").

%%% Gives us a random number we can use in our request strings
%%% to make sure servers are responding to us
create_cookie() -> 
	rand:uniform(?max_id).

%%% Gives a random number between 1 and 2**32
%%% there has to be a nicer way to do this, like
%%% with a constant or something
create_id() ->
	rand:uniform(?max_id).

%%% Tells you the distance between two peers. Uses XOR like kademlia
distance(Peer1_id, Peer2_id) ->
	Peer1_id bxor Peer2_id.
