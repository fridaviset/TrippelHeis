-module(connector).
-export([start/0]).

-define(RECEIVE_PORT, 20066).
-define(SEND_PORT, 20067).

start() ->
	node_init(),
	spawn(fun listen/0),
	spawn(fun broadcast/0).

node_init() ->
	os:cmd("epmd -daemon"),
	timer:sleep(100),
	{_ok, [{IPtuple, _Broadcast, _Self} | _Disregard]} = inet:getif(),
  IPadress=inet:ntoa(IPtuple),
  NodeName = "elevator@"++IPadress,
	net_kernel:start([list_to_atom(NodeName), longnames]),
	erlang:set_cookie(node(), 'key_lime_pie').

listen() ->
	{ok, ReceiveSocket} = gen_udp:open(?RECEIVE_PORT, [list, {active, false}]),
	listen(ReceiveSocket).
listen(ReceiveSocket) ->
	{ok, {_Address, _Port, NodeName}} = gen_udp:recv(ReceiveSocket, 0),
	Node = list_to_atom(NodeName),
	case lists:member(Node, [node()|nodes()]) of
		true ->
			listen(ReceiveSocket);
		false ->
			net_adm:ping(Node),
			listen(ReceiveSocket)
		end.

broadcast() ->
	{ok, SendSocket} = gen_udp:open(?SEND_PORT, [list, {active, true}, {broadcast, true}]),
	broadcast(SendSocket).
broadcast(SendSocket) ->
	ok = gen_udp:send(SendSocket, {255,255,255,255}, ?RECEIVE_PORT, atom_to_list(node())),
	timer:sleep(5000),
	broadcast(SendSocket).
