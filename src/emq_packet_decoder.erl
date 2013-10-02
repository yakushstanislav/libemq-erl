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

-module(emq_packet_decoder).

-export([decode_response_header_bodylen/1, decode/1]).

-include("helper.hrl").
-include("protocol.hrl").
-include("emq.hrl").

decode_response_header_bodylen(<<?UINT16(?EMQ_PROTOCOL_RES), ?UINT8(_Cmd), ?UINT8(_Status), ?UINT32(Bodylen)>>) ->
	Bodylen;

decode_response_header_bodylen(<<?UINT16(?EMQ_PROTOCOL_EVENT), ?UINT8(_Cmd), ?UINT8(_Status), ?UINT32(Bodylen)>>) ->
	Bodylen.

decode_response_header(<<?UINT16(_Magic), ?UINT8(Cmd), ?UINT8(Status), ?UINT32(Bodylen), Body/binary>>) ->
	{Cmd, Status, Bodylen, Body}.

decode_response_header_status(Command, Response) ->
	{Command, Status, _Bodylen, _Body} = decode_response_header(Response),
	decode_status(Status).

decode_string(Bin) ->
	Str = binary_to_list(Bin),
	string:substr(Str, 1, string:chr(Str, 0) - 1).

decode_status(Status) ->
	case Status of
		?EMQ_PROTOCOL_STATUS_SUCCESS ->
			ok;
		?EMQ_PROTOCOL_STATUS_ERROR ->
			error;
		?EMQ_PROTOCOL_STATUS_ERROR_PACKET ->
			error_packet;
		?EMQ_PROTOCOL_STATUS_ERROR_COMMAND ->
			error_command;
		?EMQ_PROTOCOL_STATUS_ERROR_ACCESS ->
			error_access;
		?EMQ_PROTOCOL_STATUS_ERROR_MEMORY ->
			error_memory;
		?EMQ_PROTOCOL_STATUS_ERROR_VALUE ->
			error_value;
		?EMQ_PROTOCOL_STATUS_ERROR_NOT_DECLARED ->
			error_not_declared;
		?EMQ_PROTOCOL_STATUS_ERROR_NOT_FOUND ->
			error_not_found;
		?EMQ_PROTOCOL_STATUS_ERROR_NO_DATA ->
			error_no_data
	end.

decode_stat(<<>>) ->
	[];

decode_stat(<<?UINT8(MajorVersion), ?UINT8(MinorVersion), ?UINT8(PatchVersion),
	?UINT32(Uptime), ?FLOAT(UsedCpuSys), ?FLOAT(UsedCpuUser),
	?UINT32(UsedMemory), ?UINT32(UsedMemoryRss), ?FLOAT(FragmentationRatio),
	?UINT32(Clients), ?UINT32(Users), ?UINT32(Queues), ?UINT32(Routes), ?UINT32(Channels),
	?UINT32(_Resv3), ?UINT32(_Resv4)>>) ->
	[{version, {MajorVersion, MinorVersion, PatchVersion}},
	{uptime, Uptime},
	{used_cpu_sys, UsedCpuSys},
	{used_cpu_user, UsedCpuUser},
	{used_memory, UsedMemory},
	{used_memory_rss, UsedMemoryRss},
	{fragmentation_ration, FragmentationRatio},
	{clients, Clients},
	{users, Users},
	{queues, Queues},
	{routes, Routes},
	{channels, Channels}].

decode_user_list(<<>>, List) ->
	List;

decode_user_list(<<Username:32/binary, Password:32/binary, ?UINT64(Perm), Rest/binary>>, List) ->
	decode_user_list(Rest, [#user{username = decode_string(Username), password = decode_string(Password), perm = Perm} | List]).

decode_user_list(Data) ->
	decode_user_list(Data, []).

decode_queue_list(<<>>, List) ->
	List;

decode_queue_list(<<Name:64/binary, ?UINT32(MaxMsg), ?UINT32(MaxMsgSize), ?UINT32(Flags),
	?UINT32(Size), ?UINT32(DeclaredClients), ?UINT32(SubscribedClients), Rest/binary>>, List) ->
	decode_queue_list(Rest, [#queue{name = decode_string(Name), maxMsg = MaxMsg, maxMsgSize = MaxMsgSize, flags = Flags,
		size = Size, declaredClients = DeclaredClients, subscribedClients = SubscribedClients} | List]).

decode_queue_list(Data) ->
	decode_queue_list(Data, []).

decode_route_list(<<>>, List) ->
	List;

decode_route_list(<<Name:64/binary, ?UINT32(Flags), ?UINT32(Keys), Rest/binary>>, List) ->
	decode_route_list(Rest, [#route{name = decode_string(Name), flags = Flags, keys = Keys} | List]).

decode_route_list(Data) ->
	decode_route_list(Data, []).

decode_route_keys(<<>>, List) ->
	List;

decode_route_keys(<<Key:32/binary, Queue:64/binary, Rest/binary>>, List) ->
	decode_route_keys(Rest, [#route_key{key = decode_string(Key), queue = decode_string(Queue)} | List]).

decode_route_keys(Data) ->
	decode_route_keys(Data, []).

decode_channel_list(<<>>, List) ->
	List;

decode_channel_list(<<Name:64/binary, ?UINT32(Flags), ?UINT32(Topics), ?UINT32(Patterns), Rest/binary>>, List) ->
	decode_channel_list(Rest, [#channel{name = decode_string(Name), flags = Flags, topics = Topics, patterns = Patterns} | List]).

decode_channel_list(Data) ->
	decode_channel_list(Data, []).

decode_exist_status(<<>>) ->
	undefined;

decode_exist_status(<<?UINT32(Status)>>) ->
	case Status of
		1 -> exist;
		0 -> not_exist
	end.

decode_queue_size(<<>>) ->
	undefined;

decode_queue_size(<<?UINT32(Size)>>) ->
	Size.

decode_msg(<<>>) ->
	undefined;

decode_msg(<<?UINT64(Tag), Msg/binary>>) ->
	{Tag, Msg}.

decode_event({?EMQ_PROTOCOL_CMD_QUEUE_SUBSCRIBE, ?EMQ_PROTOCOL_EVENT_NOTIFY, _Bodylen, Body}) ->
	{event, {queue, decode_string(Body)}};

decode_event({?EMQ_PROTOCOL_CMD_QUEUE_SUBSCRIBE, ?EMQ_PROTOCOL_EVENT_MESSAGE, _Bodylen, Body}) ->
	<<Queue:64/binary, Msg/binary>> = Body,
	{event, {queue, decode_string(Queue), Msg}};

decode_event({?EMQ_PROTOCOL_CMD_CHANNEL_SUBSCRIBE, ?EMQ_PROTOCOL_EVENT_MESSAGE, _Bodylen, Body}) ->
	<<Channel:64/binary, Topic:32/binary, Msg/binary>> = Body,
	{event, {channel, decode_string(Channel), decode_string(Topic), Msg}};

decode_event({?EMQ_PROTOCOL_CMD_CHANNEL_PSUBSCRIBE, ?EMQ_PROTOCOL_EVENT_MESSAGE, _Bodylen, Body}) ->
	<<Channel:64/binary, Topic:32/binary, Pattern:32/binary, Msg/binary>> = Body,
	{event, {channel, decode_string(Channel), decode_string(Topic), decode_string(Pattern), Msg}}.

decode({auth, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_AUTH, Response);

decode({ping, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_PING, Response);

decode({stat, Response}) ->
	{?EMQ_PROTOCOL_CMD_STAT, Status, _Bodylen, Body} = decode_response_header(Response),
	{decode_status(Status), decode_stat(Body)};

decode({save, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_SAVE, Response);

decode({flush, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_FLUSH, Response);

decode({user_create, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_USER_CREATE, Response);

decode({user_list, Response}) ->
	{?EMQ_PROTOCOL_CMD_USER_LIST, Status, _Bodylen, Body} = decode_response_header(Response),
	{decode_status(Status), decode_user_list(Body)};

decode({user_rename, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_USER_RENAME, Response);

decode({user_set_perm, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_USER_SET_PERM, Response);

decode({user_delete, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_USER_DELETE, Response);

decode({queue_create, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_QUEUE_CREATE, Response);

decode({queue_declare, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_QUEUE_DECLARE, Response);

decode({queue_exist, Response}) ->
	{?EMQ_PROTOCOL_CMD_QUEUE_EXIST, Status, _Bodylen, Body} = decode_response_header(Response),
	{decode_status(Status), decode_exist_status(Body)};

decode({queue_list, Response}) ->
	{?EMQ_PROTOCOL_CMD_QUEUE_LIST, Status, _Bodylen, Body} = decode_response_header(Response),
	{decode_status(Status), decode_queue_list(Body)};

decode({queue_rename, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_QUEUE_RENAME, Response);

decode({queue_size, Response}) ->
	{?EMQ_PROTOCOL_CMD_QUEUE_SIZE, Status, _Bodylen, Body} = decode_response_header(Response),
	{decode_status(Status), decode_queue_size(Body)};

decode({queue_push, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_QUEUE_PUSH, Response);

decode({queue_get, Response}) ->
	{?EMQ_PROTOCOL_CMD_QUEUE_GET, Status, _Bodylen, Body} = decode_response_header(Response),
	{decode_status(Status), decode_msg(Body)};

decode({queue_pop, Response}) ->
	{?EMQ_PROTOCOL_CMD_QUEUE_POP, Status, _Bodylen, Body} = decode_response_header(Response),
	{decode_status(Status), decode_msg(Body)};

decode({queue_confirm, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_QUEUE_CONFIRM, Response);

decode({queue_subscribe, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_QUEUE_SUBSCRIBE, Response);

decode({queue_unsubscribe, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_QUEUE_UNSUBSCRIBE, Response);

decode({queue_purge, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_QUEUE_PURGE, Response);

decode({queue_delete, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_QUEUE_DELETE, Response);

decode({route_create, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_ROUTE_CREATE, Response);

decode({route_exist, Response}) ->
	{?EMQ_PROTOCOL_CMD_ROUTE_EXIST, Status, _Bodylen, Body} = decode_response_header(Response),
	{decode_status(Status), decode_exist_status(Body)};

decode({route_list, Response}) ->
	{?EMQ_PROTOCOL_CMD_ROUTE_LIST, Status, _Bodylen, Body} = decode_response_header(Response),
	{decode_status(Status), decode_route_list(Body)};

decode({route_keys, Response}) ->
	{?EMQ_PROTOCOL_CMD_ROUTE_KEYS, Status, _Bodylen, Body} = decode_response_header(Response),
	{decode_status(Status), decode_route_keys(Body)};

decode({route_rename, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_ROUTE_RENAME, Response);

decode({route_bind, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_ROUTE_BIND, Response);

decode({route_unbind, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_ROUTE_UNBIND, Response);

decode({route_push, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_ROUTE_PUSH, Response);

decode({route_delete, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_ROUTE_DELETE, Response);

decode({channel_create, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_CHANNEL_CREATE, Response);

decode({channel_exist, Response}) ->
	{?EMQ_PROTOCOL_CMD_CHANNEL_EXIST, Status, _Bodylen, Body} = decode_response_header(Response),
	{decode_status(Status), decode_exist_status(Body)};

decode({channel_list, Response}) ->
	{?EMQ_PROTOCOL_CMD_CHANNEL_LIST, Status, _Bodylen, Body} = decode_response_header(Response),
	{decode_status(Status), decode_channel_list(Body)};

decode({channel_rename, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_CHANNEL_RENAME, Response);

decode({channel_publish, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_CHANNEL_PUBLISH, Response);

decode({channel_subscribe, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_CHANNEL_SUBSCRIBE, Response);

decode({channel_psubscribe, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_CHANNEL_PSUBSCRIBE, Response);

decode({channel_unsubscribe, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_CHANNEL_UNSUBSCRIBE, Response);

decode({channel_punsubscribe, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_CHANNEL_PUNSUBSCRIBE, Response);

decode({channel_delete, Response}) ->
	decode_response_header_status(?EMQ_PROTOCOL_CMD_CHANNEL_DELETE, Response);

decode({wait_event, Response}) ->
	decode_event(decode_response_header(Response)).
