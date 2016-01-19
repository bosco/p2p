%%% Constants

%%% This is the maximum size of our ids and our cookies 2**32
-define(max_id, 4294967296).

%%% Records

-record(ids,{id, server_pid, client_pid}).
