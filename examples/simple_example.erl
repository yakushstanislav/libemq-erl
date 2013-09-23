-module(simple_example).

-export([start/0]).

-include_lib("emq/include/emq.hrl").

-define(PRINT(Text), io:format(Text, [])).
-define(PRINT(Format, Args), io:format(Format, Args)).

-define(HOST, {127, 0, 0, 1}).
-define(PORT, 7851).

basic() ->
	?PRINT("Auth~n"),
	ok = emq:auth(test, "eagle", "eagle"),

	?PRINT("Ping~n"),
	ok = emq:ping(test),

	?PRINT("Stat~n"),
	{ok, Stat} = emq:stat(test),

	?PRINT("[Status]~n"),
	?PRINT("~p~n", [Stat]).

user_management() ->
	?PRINT("User create~n"),
	ok = emq:user_create(test, "first.user", "password", ?EMQ_QUEUE_PERM),

	?PRINT("User create~n"),
	ok = emq:user_create(test, "user_2", "password", ?EMQ_QUEUE_PERM),

	?PRINT("User list~n"),
	{ok, Users} = emq:user_list(test),
	?PRINT("~p~n", [Users]),

	?PRINT("User rename~n"),
	ok = emq:user_rename(test, "user_2", "user"),

	?PRINT("Set perm~n"),
	ok = emq:user_set_perm(test, "user", ?EMQ_QUEUE_PERM bor ?EMQ_ADMIN_PERM),

	?PRINT("User delete~n"),
	ok = emq:user_delete(test, "first.user"),

	?PRINT("User delete~n"),
	ok = emq:user_delete(test, "user").

queue_management() ->
	?PRINT("Queue create~n"),
	ok = emq:queue_create(test, ".queue_1", ?EMQ_MAX_MSG, ?EMQ_MAX_MSG_SIZE, []),

	?PRINT("Queue create~n"),
	ok = emq:queue_create(test, "queue-2", ?EMQ_MAX_MSG, ?EMQ_MAX_MSG_SIZE, []),

	?PRINT("Queue declare~n"),
	ok = emq:queue_declare(test, ".queue_1"),

	?PRINT("Queue declare~n"),
	ok = emq:queue_declare(test, "queue-2"),

	?PRINT("Queue \".queue_1\" exist: ~p~n", [emq:queue_exist(test, ".queue_1")]),

	?PRINT("Queue \"not-exist-queue\" exist: ~p~n", [emq:queue_exist(test, "not-exist-queue")]),

	?PRINT("Queue list~n"),
	{ok, Queues} = emq:queue_list(test),
	?PRINT("~p~n", [Queues]),

	?PRINT("Queue rename~n"),
	ok = emq:queue_rename(test, ".queue_1", ".queue_test"),

	Message = list_to_binary("test message"),

	?PRINT("Queue push~n"),
	ok = emq:queue_push(test, ".queue_test", Message),
	?PRINT("Queue push~n"),
	ok = emq:queue_push(test, "queue-2", Message),
	?PRINT("Queue push~n"),
	ok = emq:queue_push(test, ".queue_test", Message),
	?PRINT("Queue push~n"),
	ok = emq:queue_push(test, "queue-2", Message),
	?PRINT("Queue push~n"),
	ok = emq:queue_push(test, ".queue_test", Message),
	?PRINT("Queue push~n"),
	ok = emq:queue_push(test, "queue-2", Message),

	?PRINT("Queue \".queue_test\" size: ~w~n", [emq:queue_size(test, ".queue_test")]),

	?PRINT("Get message ~p~n", [emq:queue_get(test, ".queue_test")]),
	?PRINT("Get message ~p~n", [emq:queue_get(test, ".queue_test")]),
	?PRINT("Get message ~p~n", [emq:queue_get(test, ".queue_test")]),

	?PRINT("Queue \".queue_test\" size: ~w~n", [emq:queue_size(test, ".queue_test")]),

	?PRINT("Queue \"queue-2\" size: ~w~n", [emq:queue_size(test, "queue-2")]),

	?PRINT("Pop message ~p~n", [emq:queue_pop(test, "queue-2")]),
	?PRINT("Pop message ~p~n", [emq:queue_pop(test, "queue-2")]),
	?PRINT("Pop message ~p~n", [emq:queue_pop(test, "queue-2")]),

	?PRINT("Queue \"queue-2\" size: ~w~n", [emq:queue_size(test, "queue-2")]),

	?PRINT("Queue purge~n"),
	ok = emq:queue_purge(test, ".queue_test"),

	?PRINT("Queue \".queue_test\" size: ~w~n", [emq:queue_size(test, ".queue_test")]),

	?PRINT("Queue delete~n"),
	ok = emq:queue_delete(test, ".queue_test"),

	?PRINT("Queue delete~n"),
	ok = emq:queue_delete(test, "queue-2").

route_management() ->
	?PRINT("Route create~n"),
	ok = emq:route_create(test, ".route_1", []),

	?PRINT("Queue create~n"),
	ok = emq:queue_create(test, ".queue_1", ?EMQ_MAX_MSG, ?EMQ_MAX_MSG_SIZE, []),

	?PRINT("Queue create~n"),
	ok = emq:queue_create(test, ".queue_2", ?EMQ_MAX_MSG, ?EMQ_MAX_MSG_SIZE, []),

	?PRINT("Route \".route_1\" exist: ~p~n", [emq:route_exist(test, ".route_1")]),

	?PRINT("Route \"not-exist-route\" exist: ~p~n", [emq:route_exist(test, "not-exist-route")]),

	?PRINT("Route list~n"),
	{ok, Routes} = emq:route_list(test),
	?PRINT("~p~n", [Routes]),

	?PRINT("Route bind~n"),
	ok = emq:route_bind(test, ".route_1", ".queue_1", "key1"),

	?PRINT("Route bind~n"),
	ok = emq:route_bind(test, ".route_1", ".queue_2", "key1"),

	?PRINT("Route bind~n"),
	ok = emq:route_bind(test, ".route_1", ".queue_2", "key2"),

	?PRINT("Route keys~n"),
	{ok, RouteKeys} = emq:route_keys(test, ".route_1"),
	?PRINT("~p~n", [RouteKeys]),

	?PRINT("Route rename~n"),
	ok = emq:route_rename(test, ".route_1", ".route_test"),

	Message = list_to_binary("test message"),

	?PRINT("Route push~n"),
	ok = emq:route_push(test, ".route_test", "key1", Message),
	?PRINT("Route push~n"),
	ok = emq:route_push(test, ".route_test", "key2", Message),
	?PRINT("Route push~n"),
	ok = emq:route_push(test, ".route_test", "key1", Message),
	?PRINT("Route push~n"),
	ok = emq:route_push(test, ".route_test", "key2", Message),
	?PRINT("Route push~n"),
	ok = emq:route_push(test, ".route_test", "key1", Message),
	?PRINT("Route push~n"),
	ok = emq:route_push(test, ".route_test", "key2", Message),

	?PRINT("Queue \".queue_1\" size: ~w~n", [emq:queue_size(test, ".queue_1")]),

	?PRINT("Queue \".queue_2\" size: ~w~n", [emq:queue_size(test, ".queue_2")]),

	?PRINT("Route unbind~n"),
	ok = emq:route_unbind(test, ".route_test", ".queue_1", "key1"),

	?PRINT("Route unbind~n"),
	ok = emq:route_unbind(test, ".route_test", ".queue_2", "key1"),

	?PRINT("Route unbind~n"),
	ok = emq:route_unbind(test, ".route_test", ".queue_2", "key2"),

	?PRINT("Route delete~n"),
	ok = emq:route_delete(test, ".route_test"),

	?PRINT("Queue delete~n"),
	ok = emq:queue_delete(test, ".queue_1"),

	?PRINT("Queue delete~n"),
	ok = emq:queue_delete(test, ".queue_2").

channel_management() ->
	?PRINT("Channel create~n"),
	ok = emq:channel_create(test, ".channel_1", []),

	?PRINT("Channel create~n"),
	ok = emq:channel_create(test, ".channel_2", []),

	?PRINT("Channel \".channel_1\" exist: ~p~n", [emq:channel_exist(test, ".channel_1")]),

	?PRINT("Channel \".channel_2\" exist: ~p~n", [emq:channel_exist(test, ".channel_2")]),

	?PRINT("Channel list~n"),
	{ok, Channels} = emq:channel_list(test),
	?PRINT("~p~n", [Channels]),

	?PRINT("Channel delete~n"),
	ok = emq:channel_delete(test, ".channel_1"),

	?PRINT("Channel delete~n"),
	ok = emq:channel_delete(test, ".channel_2").

start() ->
	?PRINT("This is a simple example of using libemq-erl~n"),

	emq:connect(test, ?HOST, ?PORT),
	?PRINT("Connected to ~w:~w~n", [?HOST, ?PORT]),

	basic(),
	user_management(),
	queue_management(),
	route_management(),
	channel_management(),

	emq:disconnect(test).
