-module(network).
-export([start/0]).

-define(SEND_PORT, 5676).
-define(RECV_PORT, 5675).
-define(SEEK_PERIOD, 5000).


start() ->
    spawn(fun() -> listen_for_connections() end),
    spawn(fun() -> broadcast_own_name() end).
		  
		  
listen_for_connections() ->
     %Open up an udp connection at some port on your computer, sets which port should receive messages
    {ok, RecvSocket} = gen_udp:open(?RECV_PORT, [list, {reuseaddr,true}, {active,false}]),
    listen_for_connections(RecvSocket).

listen_for_connections(RecvSocket) ->
    %Using udp to try and receive the adress port and the Nodename of the other computer broadcasting stuff
    {ok, {_Adress, ?SEND_PORT, NodeName}} = gen_udp:recv(RecvSocket, 0),
    %When the nodename is received, the Node is added to the cluster (if it has not been added before)
    Node = list_to_atom(NodeName),
    case is_in_cluster(Node) of
	true ->
        %If it is in the cluster, continue the process of listening to lifesigns
	    listen_for_connections(RecvSocket);
	false ->
        %If it is not in the cluster, let it join the party!
        net_adm:ping(Node),
        %Then, continue the search for new mates.
	    listen_for_connections(RecvSocket)
    end.

is_in_cluster(Node) ->
    %Make a complete list of the cluster, don't forget to count yourself!
    NodeList = [node()|nodes()],
    %Take advantage of BIF that checks if element is in list    
    lists:member(Node, NodeList).


broadcast_own_name() ->
    {ok, Socket} = gen_udp:open(?SEND_PORT, [list, {reuseaddr,true}, {active,true}, {broadcast, true}]),
    broadcast_own_name(Socket).

broadcast_own_name(SendSocket) ->
    %Sending your own name as a list (why not just send the atom? I dont understand)
    %to all computers inn the entire world (or more realisticly, all the computers your router allows)
    ok = gen_udp:send(SendSocket, {255,255,255,255}, ?RECV_PORT, atom_to_list(node())),
    timer:sleep(?SEEK_PERIOD),
    broadcast_own_name(SendSocket).