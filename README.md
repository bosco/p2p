# p2p
![16 peer network](https://raw.githubusercontent.com/bosco/p2p/master/16_peer_network.png)

I wanted to learn me some erlang for great good, so I wrote this p2p network simulator. It is based on [this awesome video.](https://www.youtube.com/watch?v=kXyVqk3EbwE) It starts up a bunch of processes to simulate peers on a network. Each peer has two processes: a client process that handles peer discovery (as well as some requests from the supervisor) and a server process that answers information requests. There is also a network supervisor which can be used to gather statistics about the network.

## Data Structures

### \#peer
This record is used in a whole bunch of stuff. Here are its elements:

**id** - An SHA1 hash that is unique to the peer

**server\_pid** - The PID of the server process associated with this peer

**client\_pid** - The PID of the client process associated with this peer

### Peers
This is a gb\_tree that contains all the peers a peer knows about. The key is the peer id, and the value is {server\_pid, client\_pid}. A tree works nicely for this because we spend a lot of time searching for the peer *closest* to a certain id. The tree size is limited to 160 peers.

## Protocol
### get\_peers
This takes a \#peer record with the client information, a pid to respond to, and a cookie from the client. It responds with a tree of the peers the server currently knows about and the cookie the client sent us. The client's peer information is added to the server's tree of known peers.
### find\_closest
This takes a \#peer record with the client information, a pid to resond to, a cookie from the client, and an id to look form. The server searches through its of peers and responds with the peer closest to the id the client was looking for as well as the cookie the client sent us. The client's peer information is added to the server's tree of known peers.
### update\_peers
This can ONLY be called by our client process wich has our secret cookie. It replaces our old known peers tree with a new one. It returns nothing.

## Peer discovery
When a peer is created it gives itself a 160 bit SHA1 hash. It also reaches out to another peer to download its peers tree. Using that peers tree a new peer searches for other peers closest to it (falling off by a factor of two each time). If there is extra space in the tree the peer puts any remaining peers in the tree it downloaded into its own tree.

To find an ID a peer starts by looking in its own tree and finding the closest ID it can. It then reaches out to that peer and asks it for the closest peer it has to that ID. So on and so forth until it either finds what it was looking for, ends up getting stopped at a peer that thinks it is the closest to that ID, or reaches the maximum number of hops (this is to avoid routing loops).

### Supervisor
In the network.erl file the create\_supervisor/1 function will build a p2p network where each peer starts off by being randomly connected to another peer. The supervisor process is linked to all other processes and has some cool commands:

**die** - Kills itself

**net\_status** - Reaches out to every peer to find how many peers it knows about and how many peers in the network it can connect to. Also gives you an average of the percentage of other peers each peer can connect to.

**refresh_peers** - Reaches out to each peer and tells it to go through its discovery procedure. ?refresh, defined in p2p.hrl, sets the timeout for peers to do this automatically. As of this writing it's set to one day, but it changes all the time as I test stuff.

**output_tgf** - Takes a filename as input and creates a Trivial Graph Format file for the network with all the nodes and their current connections. You can then use yEd to get a visual representation of the network.

**time_lapse** - Refreshes all the peers and then outputs a TGF over and over for the amount of steps specified. Surprisingly boring since peers seem to learn about other peers pretty quickly. 

### Usage

```
1> network:create_supervisor(16).
Network created and supervisor registered as supervisor.
ok
2> supervisor ! net_status.
net_status
1103758995791841315871195529108936648184935609178 has 12 peers in its tree.
1103758995791841315871195529108936648184935609178 is connected to 16/16 peers 100%
[...]
Connectivity percentage 95%
3> supervisor ! refresh_peers.
refresh_peers
4> supervisor ! net_status.
[...]
Connectivity percentage 100%
5> supervisor ! {output_tgf, "16_peer_network.tgf"}.
Writing network structure to "16_peer_network.tgf"
{output_tgf,"16_peer_network.tgf"}
```
