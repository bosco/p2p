# p2p
I wanted to learn me some erlang for great good, so I wrote this p2p network simulator. It starts up a bunch of processes to simulate peers on a network. Each peer has two processes: a client process that handles peer discovery (as well as some requestions from the supervisor) and a server process that answers information requests. There is also a network supervisor which can be used to gather statistics about the network.

## Data Structures

### \#peer
This record is used in a whole bunch of stuff. Here are its elements:
id - An SHA1 hash that is unique to the peer
server\_pid - The PID of the server process associated with this peer
client\_pid - The PID of the client process associated with this peer

### Peers
This is a gb\_tree that contains all the peers a peer knows about. The key is the peer id, and the value is {server\_pid, client\_pid}. A tree works nicely for this because we spend a lot of time searching for the peer *closest* to a certain id.

## Protocol
### get\_peers
### find\_closest
### update\_peers

### Peer discovery
