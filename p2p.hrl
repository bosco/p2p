%%% Constants

%%% the maximum size of our cookies is 2**32
-define(max_cookie, 4294967296).

%%% the size of our id in bits (SHA1 is 160 bits)
-define(id_size, 160).

%%% this is how often we rebuild our Peers tree
-define(refresh, 10000).

%%% how long to wait for responses to our client requests
-define(client_timeout, 2000).

%%% Records

%%% Data structures
-record(peer,{id, server_pid, client_pid}).

%%% Protocl structures
-record(update_peers,{cookie, peers}).
-record(get_peers,{cookie, mypid, peer=#peer{}}).
-record(get_peers_response,{cookie, peers}).
-record(find_closest,{cookie, mypid, peer=#peer{}, id}).
-record(find_closest_response,{cookie, result}).
