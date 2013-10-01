%% Copyright (c) 2013, Stanislav Yakush(st.yakush@yandex.ru)
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%    * Redistributions of source code must retain the above copyright
%%      notice, this list of conditions and the following disclaimer.
%%    * Redistributions in binary form must reproduce the above copyright
%%      notice, this list of conditions and the following disclaimer in the
%%      documentation and/or other materials provided with the distribution.
%%    * Neither the name of the libemq-erl nor the
%%      names of its contributors may be used to endorse or promote products
%%      derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(emq_packet_encoder).

-export([encode/1]).

-include("helper.hrl").
-include("protocol.hrl").
-include("emq.hrl").

encode_request_header(Cmd, Noack, Bodylen) ->
	<<?UINT16(?EMQ_PROTOCOL_REQ), ?UINT8(Cmd), ?UINT8(Noack), ?UINT32(Bodylen)>>.

encode_string(Str, Pad) ->
	<<?STRING(Str), 0:((8 * Pad) - (length(Str) * 8))>>.

encode_save_mode(sync) -> 0;
encode_save_mode(async) -> 1.

encode_flush_flags([], Value) ->
	Value;

encode_flush_flags([all|T], _Value) ->
	Flags = ?EMQ_PROTOCOL_FLUSH_USER bor ?EMQ_PROTOCOL_FLUSH_QUEUE bor
		?EMQ_PROTOCOL_FLUSH_ROUTE bor ?EMQ_PROTOCOL_FLUSH_CHANNEL,
	encode_flush_flags(T, Flags);

encode_flush_flags([user|T], Value) ->
	encode_flush_flags(T, Value bor ?EMQ_PROTOCOL_FLUSH_USER);

encode_flush_flags([queue|T], Value) ->
	encode_flush_flags(T, Value bor ?EMQ_PROTOCOL_FLUSH_QUEUE);

encode_flush_flags([route|T], Value) ->
	encode_flush_flags(T, Value bor ?EMQ_PROTOCOL_FLUSH_ROUTE);

encode_flush_flags([channel|T], Value) ->
	encode_flush_flags(T, Value bor ?EMQ_PROTOCOL_FLUSH_CHANNEL).

encode_flush_flags(Flags) ->
	encode_flush_flags(Flags, 0).

encode_queue_flags([], Value) ->
	Value;

encode_queue_flags([autodelete|T], Value) ->
	encode_queue_flags(T, Value bor ?EMQ_QUEUE_AUTODELETE);

encode_queue_flags([force_push|T], Value) ->
	encode_queue_flags(T, Value bor ?EMQ_QUEUE_FORCE_PUSH);

encode_queue_flags([round_robin|T], Value) ->
	encode_queue_flags(T, Value bor ?EMQ_QUEUE_ROUND_ROBIN);

encode_queue_flags([durable|T], Value) ->
	encode_queue_flags(T, Value bor ?EMQ_QUEUE_DURABLE).

encode_queue_flags(Flags) ->
	encode_queue_flags(Flags, ?EMQ_QUEUE_NONE).

encode_queue_subscribe_flags(msg) -> 0;
encode_queue_subscribe_flags(notify) -> 1.

encode_route_flags([], Value) ->
	Value;

encode_route_flags([autodelete|T], Value) ->
	encode_route_flags(T, Value bor ?EMQ_ROUTE_AUTODELETE);

encode_route_flags([round_robin|T], Value) ->
	encode_route_flags(T, Value bor ?EMQ_ROUTE_ROUND_ROBIN);

encode_route_flags([durable|T], Value) ->
	encode_route_flags(T, Value bor ?EMQ_ROUTE_DURABLE).

encode_route_flags(Flags) ->
	encode_route_flags(Flags, ?EMQ_ROUTE_NONE).

encode_channel_flags([], Value) ->
	Value;

encode_channel_flags([autodelete|T], Value) ->
	encode_channel_flags(T, Value bor ?EMQ_CHANNEL_AUTODELETE);

encode_channel_flags([round_robin|T], Value) ->
	encode_channel_flags(T, Value bor ?EMQ_CHANNEL_ROUND_ROBIN);

encode_channel_flags([durable|T], Value) ->
	encode_channel_flags(T, Value bor ?EMQ_CHANNEL_DURABLE).

encode_channel_flags(Flags) ->
	encode_channel_flags(Flags, ?EMQ_CHANNEL_NONE).

encode({auth, {Username, Password}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_AUTH, 0, 64),
	ReqUsername = encode_string(Username, 32),
	ReqPassword = encode_string(Password, 32),
	<<ReqHeader/binary, ReqUsername/binary, ReqPassword/binary>>;

encode(ping) ->
	encode_request_header(?EMQ_PROTOCOL_CMD_PING, 0, 0);

encode(stat) ->
	encode_request_header(?EMQ_PROTOCOL_CMD_STAT, 0, 0);

encode({save, {Mode}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_SAVE, 0, 1),
	ReqMode = encode_save_mode(Mode),
	<<ReqHeader/binary, ?UINT8(ReqMode)>>;

encode({flush, {Flags}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_FLUSH, 0, 4),
	ReqFlags = encode_flush_flags(Flags),
	<<ReqHeader/binary, ?UINT32(ReqFlags)>>;

encode({user_create, {Username, Password, Perm}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_USER_CREATE, 0, 72),
	ReqUsername = encode_string(Username, 32),
	ReqPassword = encode_string(Password, 32),
	<<ReqHeader/binary, ReqUsername/binary, ReqPassword/binary, ?UINT64(Perm)>>;

encode(user_list) ->
	encode_request_header(?EMQ_PROTOCOL_CMD_USER_LIST, 0, 0);

encode({user_rename, {From, To}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_USER_RENAME, 0, 64),
	ReqFrom = encode_string(From, 32),
	ReqTo = encode_string(To, 32),
	<<ReqHeader/binary, ReqFrom/binary, ReqTo/binary>>;

encode({user_set_perm, {Username, Perm}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_USER_SET_PERM, 0, 40),
	ReqUsername = encode_string(Username, 32),
	<<ReqHeader/binary, ReqUsername/binary, ?UINT64(Perm)>>;

encode({user_delete, {Username}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_USER_DELETE, 0, 32),
	ReqUsername = encode_string(Username, 32),
	<<ReqHeader/binary, ReqUsername/binary>>;

encode({queue_create, {Name, MaxMsg, MaxMsgSize, Flags}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_QUEUE_CREATE, 0, 76),
	ReqName = encode_string(Name, 64),
	ReqFlags = encode_queue_flags(Flags),
	<<ReqHeader/binary, ReqName/binary, ?UINT32(MaxMsg), ?UINT32(MaxMsgSize), ?UINT32(ReqFlags)>>;

encode({queue_declare, {Name}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_QUEUE_DECLARE, 0, 64),
	ReqName = encode_string(Name, 64),
	<<ReqHeader/binary, ReqName/binary>>;

encode({queue_exist, {Name}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_QUEUE_EXIST, 0, 64),
	ReqName = encode_string(Name, 64),
	<<ReqHeader/binary, ReqName/binary>>;

encode(queue_list) ->
	encode_request_header(?EMQ_PROTOCOL_CMD_QUEUE_LIST, 0, 0);

encode({queue_rename, {From, To}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_QUEUE_RENAME, 0, 128),
	ReqFrom = encode_string(From, 64),
	ReqTo = encode_string(To, 64),
	<<ReqHeader/binary, ReqFrom/binary, ReqTo/binary>>;

encode({queue_size, {Name}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_QUEUE_SIZE, 0, 64),
	ReqName = encode_string(Name, 64),
	<<ReqHeader/binary, ReqName/binary>>;

encode({queue_push, {Name, Msg, Expire}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_QUEUE_PUSH, 0, 68 + size(Msg)),
	ReqName = encode_string(Name, 64),
	<<ReqHeader/binary, ReqName/binary, ?UINT32(Expire), Msg/binary>>;

encode({queue_get, {Name}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_QUEUE_GET, 0, 64),
	ReqName = encode_string(Name, 64),
	<<ReqHeader/binary, ReqName/binary>>;

encode({queue_pop, {Name, Timeout}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_QUEUE_POP, 0, 68),
	ReqName = encode_string(Name, 64),
	<<ReqHeader/binary, ReqName/binary, ?UINT32(Timeout)>>;

encode({queue_confirm, {Name, Tag}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_QUEUE_CONFIRM, 0, 72),
	ReqName = encode_string(Name, 64),
	<<ReqHeader/binary, ReqName/binary, ?UINT64(Tag)>>;

encode({queue_subscribe, {Name, Flags}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_QUEUE_SUBSCRIBE, 0, 68),
	ReqName = encode_string(Name, 64),
	ReqFlags = encode_queue_subscribe_flags(Flags),
	<<ReqHeader/binary, ReqName/binary, ?UINT32(ReqFlags)>>;

encode({queue_unsubscribe, {Name}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_QUEUE_UNSUBSCRIBE, 0, 64),
	ReqName = encode_string(Name, 64),
	<<ReqHeader/binary, ReqName/binary>>;

encode({queue_purge, {Name}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_QUEUE_PURGE, 0, 64),
	ReqName = encode_string(Name, 64),
	<<ReqHeader/binary, ReqName/binary>>;

encode({queue_delete, {Name}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_QUEUE_DELETE, 0, 64),
	ReqName = encode_string(Name, 64),
	<<ReqHeader/binary, ReqName/binary>>;

encode({route_create, {Name, Flags}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_ROUTE_CREATE, 0, 68),
	ReqName = encode_string(Name, 64),
	ReqFlags = encode_route_flags(Flags),
	<<ReqHeader/binary, ReqName/binary, ?UINT32(ReqFlags)>>;

encode({route_exist, {Name}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_ROUTE_EXIST, 0, 64),
	ReqName = encode_string(Name, 64),
	<<ReqHeader/binary, ReqName/binary>>;

encode(route_list) ->
	encode_request_header(?EMQ_PROTOCOL_CMD_ROUTE_LIST, 0, 0);

encode({route_keys, {Name}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_ROUTE_KEYS, 0, 64),
	ReqName = encode_string(Name, 64),
	<<ReqHeader/binary, ReqName/binary>>;

encode({route_rename, {From, To}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_ROUTE_RENAME, 0, 128),
	ReqFrom = encode_string(From, 64),
	ReqTo = encode_string(To, 64),
	<<ReqHeader/binary, ReqFrom/binary, ReqTo/binary>>;

encode({route_bind, {Name, Queue, Key}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_ROUTE_BIND, 0, 160),
	ReqName = encode_string(Name, 64),
	ReqQueue = encode_string(Queue, 64),
	ReqKey = encode_string(Key, 32),
	<<ReqHeader/binary, ReqName/binary, ReqQueue/binary, ReqKey/binary>>;

encode({route_unbind, {Name, Queue, Key}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_ROUTE_UNBIND, 0, 160),
	ReqName = encode_string(Name, 64),
	ReqQueue = encode_string(Queue, 64),
	ReqKey = encode_string(Key, 32),
	<<ReqHeader/binary, ReqName/binary, ReqQueue/binary, ReqKey/binary>>;

encode({route_push, {Name, Key, Msg, Expire}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_ROUTE_PUSH, 0, 100 + size(Msg)),
	ReqName = encode_string(Name, 64),
	ReqKey = encode_string(Key, 32),
	<<ReqHeader/binary, ReqName/binary, ReqKey/binary, ?UINT32(Expire), Msg/binary>>;

encode({route_delete, {Name}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_ROUTE_DELETE, 0, 64),
	ReqName = encode_string(Name, 64),
	<<ReqHeader/binary, ReqName/binary>>;

encode({channel_create, {Name, Flags}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_CHANNEL_CREATE, 0, 68),
	ReqName = encode_string(Name, 64),
	ReqFlags = encode_channel_flags(Flags),
	<<ReqHeader/binary, ReqName/binary, ?UINT32(ReqFlags)>>;

encode({channel_exist, {Name}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_CHANNEL_EXIST, 0, 64),
	ReqName = encode_string(Name, 64),
	<<ReqHeader/binary, ReqName/binary>>;

encode(channel_list) ->
	encode_request_header(?EMQ_PROTOCOL_CMD_CHANNEL_LIST, 0, 0);

encode({channel_rename, {From, To}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_CHANNEL_RENAME, 0, 128),
	ReqFrom = encode_string(From, 64),
	ReqTo = encode_string(To, 64),
	<<ReqHeader/binary, ReqFrom/binary, ReqTo/binary>>;

encode({channel_publish, {Name, Topic, Msg}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_CHANNEL_PUBLISH, 0, 96 + size(Msg)),
	ReqName = encode_string(Name, 64),
	ReqTopic = encode_string(Topic, 32),
	<<ReqHeader/binary, ReqName/binary, ReqTopic/binary, Msg/binary>>;

encode({channel_subscribe, {Name, Topic}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_CHANNEL_SUBSCRIBE, 0, 96),
	ReqName = encode_string(Name, 64),
	ReqTopic = encode_string(Topic, 32),
	<<ReqHeader/binary, ReqName/binary, ReqTopic/binary>>;

encode({channel_psubscribe, {Name, Pattern}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_CHANNEL_PSUBSCRIBE, 0, 96),
	ReqName = encode_string(Name, 64),
	ReqPattern = encode_string(Pattern, 32),
	<<ReqHeader/binary, ReqName/binary, ReqPattern/binary>>;

encode({channel_unsubscribe, {Name, Topic}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_CHANNEL_UNSUBSCRIBE, 0, 96),
	ReqName = encode_string(Name, 64),
	ReqTopic = encode_string(Topic, 32),
	<<ReqHeader/binary, ReqName/binary, ReqTopic/binary>>;

encode({channel_punsubscribe, {Name, Pattern}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_CHANNEL_PUNSUBSCRIBE, 0, 96),
	ReqName = encode_string(Name, 64),
	ReqPattern = encode_string(Pattern, 32),
	<<ReqHeader/binary, ReqName/binary, ReqPattern/binary>>;

encode({channel_delete, {Name}}) ->
	ReqHeader = encode_request_header(?EMQ_PROTOCOL_CMD_CHANNEL_DELETE, 0, 64),
	ReqName = encode_string(Name, 64),
	<<ReqHeader/binary, ReqName/binary>>.
