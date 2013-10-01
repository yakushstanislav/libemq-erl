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

-module(emq).

-export([connect/1, connect/2, connect/3, disconnect/1]).
-export([auth/3, ping/1, stat/1, save/2, flush/2]).
-export([user_create/4, user_list/1, user_rename/3, user_set_perm/3, user_delete/2]).
-export([queue_create/5, queue_declare/2, queue_exist/2, queue_list/1, queue_rename/3, queue_size/2,
	queue_push/3, queue_push/4, queue_get/2, queue_pop/2, queue_pop/3, queue_confirm/3, queue_subscribe/3,
	queue_unsubscribe/2, queue_purge/2, queue_delete/2]).
-export([route_create/3, route_exist/2, route_list/1, route_keys/2, route_rename/3,
	route_bind/4, route_unbind/4, route_push/4, route_push/5, route_delete/2]).
-export([channel_create/3, channel_exist/2, channel_list/1, channel_rename/3, channel_publish/4,
	channel_subscribe/3, channel_psubscribe/3, channel_unsubscribe/3, channel_punsubscribe/3, channel_delete/2]).

-define(DEFAULT_HOST, {127, 0, 0, 1}).
-define(DEFAULT_PORT, 7851).

-define(is_string(Str, Len), (is_list(Str) and (length(Str) < Len))).

%% Functions to control connection

connect(Server) ->
	connect(Server, ?DEFAULT_HOST, ?DEFAULT_PORT).

connect(Server, Host) ->
	connect(Server, Host, ?DEFAULT_PORT).

connect(Server, Host, Port) ->
	gen_server:start_link({local, Server}, emq_connection, {Host, Port}, []).

disconnect(Server) ->
	gen_server:cast(Server, stop).

%% Basic functions

auth(Server, Username, Password) when ?is_string(Username, 32) and ?is_string(Password, 32) ->
	gen_server:call(Server, {auth, {Username, Password}}).

ping(Server) ->
	gen_server:call(Server, ping).

stat(Server) ->
	gen_server:call(Server, stat).

save(Server, Mode) when (Mode == async) or (Mode == sync) ->
	gen_server:call(Server, {save, {Mode}}).

flush(Server, Flags) when is_list(Flags) ->
	gen_server:call(Server, {flush, {Flags}}).

%% Functions for work with users

user_create(Server, Username, Password, Perm) when ?is_string(Username, 32) and ?is_string(Password, 32) ->
	gen_server:call(Server, {user_create, {Username, Password, Perm}}).

user_list(Server) ->
	gen_server:call(Server, user_list).

user_rename(Server, From, To) when ?is_string(From, 32) and ?is_string(To, 32) ->
	gen_server:call(Server, {user_rename, {From, To}}).

user_set_perm(Server, Username, Perm) when ?is_string(Username, 32) ->
	gen_server:call(Server, {user_set_perm, {Username, Perm}}).

user_delete(Server, Username) when ?is_string(Username, 32) ->
	gen_server:call(Server, {user_delete, {Username}}).

%% Functions for work with queues

queue_create(Server, Name, MaxMsg, MaxMsgSize, Flags) when ?is_string(Name, 64) and is_list(Flags) ->
	gen_server:call(Server, {queue_create, {Name, MaxMsg, MaxMsgSize, Flags}}).

queue_declare(Server, Name) when ?is_string(Name, 64) ->
	gen_server:call(Server, {queue_declare, {Name}}).

queue_exist(Server, Name) when ?is_string(Name, 64) ->
	gen_server:call(Server, {queue_exist, {Name}}).

queue_list(Server) ->
	gen_server:call(Server, queue_list).

queue_rename(Server, From, To) when ?is_string(From, 64) and ?is_string(To, 64) ->
	gen_server:call(Server, {queue_rename, {From, To}}).

queue_size(Server, Name) when ?is_string(Name, 64) ->
	gen_server:call(Server, {queue_size, {Name}}).

queue_push(Server, Name, Msg) when ?is_string(Name, 64) and is_binary(Msg) ->
	gen_server:call(Server, {queue_push, {Name, Msg, 0}}, infinity).

queue_push(Server, Name, Msg, Expire) when ?is_string(Name, 64) and is_binary(Msg) and is_integer(Expire) ->
	gen_server:call(Server, {queue_push, {Name, Msg, Expire}}, infinity).

queue_get(Server, Name) when ?is_string(Name, 64) ->
	gen_server:call(Server, {queue_get, {Name}}, infinity).

queue_pop(Server, Name) when ?is_string(Name, 64) ->
	gen_server:call(Server, {queue_pop, {Name, 0}}, infinity).

queue_pop(Server, Name, Timeout) when ?is_string(Name, 64) and is_integer(Timeout) ->
	gen_server:call(Server, {queue_pop, {Name, Timeout}}, infinity).

queue_confirm(Server, Name, Tag) when ?is_string(Name, 64) ->
	gen_server:call(Server, {queue_confirm, {Name, Tag}}).

queue_subscribe(Server, Name, Flags) when ?is_string(Name, 64) and is_atom(Flags) ->
	gen_server:call(Server, {queue_subscribe, {Name, Flags}}).

queue_unsubscribe(Server, Name) when ?is_string(Name, 64) ->
	gen_server:call(Server, {queue_unsubscribe, {Name}}).

queue_purge(Server, Name) when ?is_string(Name, 64) ->
	gen_server:call(Server, {queue_purge, {Name}}).

queue_delete(Server, Name) when ?is_string(Name, 64) ->
	gen_server:call(Server, {queue_delete, {Name}}).

%% Functions for work with routes

route_create(Server, Name, Flags) when ?is_string(Name, 64) and is_list(Flags) ->
	gen_server:call(Server, {route_create, {Name, Flags}}).

route_exist(Server, Name) when ?is_string(Name, 64) ->
	gen_server:call(Server, {route_exist, {Name}}).

route_list(Server) ->
	gen_server:call(Server, route_list).

route_keys(Server, Name) when ?is_string(Name, 64) ->
	gen_server:call(Server, {route_keys, {Name}}).

route_rename(Server, From, To) when ?is_string(From, 64) and ?is_string(To, 64) ->
	gen_server:call(Server, {route_rename, {From, To}}).

route_bind(Server, Name, Queue, Key) when ?is_string(Name, 64) and ?is_string(Queue, 64) and ?is_string(Key, 32) ->
	gen_server:call(Server, {route_bind, {Name, Queue, Key}}).

route_unbind(Server, Name, Queue, Key) when ?is_string(Name, 64) and ?is_string(Queue, 64) and ?is_string(Key, 32) ->
	gen_server:call(Server, {route_unbind, {Name, Queue, Key}}).

route_push(Server, Name, Key, Msg) when ?is_string(Name, 64) and ?is_string(Key, 32) and is_binary(Msg) ->
	gen_server:call(Server, {route_push, {Name, Key, Msg, 0}}, infinity).

route_push(Server, Name, Key, Msg, Expire) when ?is_string(Name, 64) and ?is_string(Key, 32) and is_binary(Msg) ->
	gen_server:call(Server, {route_push, {Name, Key, Msg, Expire}}, infinity).

route_delete(Server, Name)  when ?is_string(Name, 64) ->
	gen_server:call(Server, {route_delete, {Name}}).

%% Functions for work with channels

channel_create(Server, Name, Flags) when ?is_string(Name, 64) and is_list(Flags) ->
	gen_server:call(Server, {channel_create, {Name, Flags}}).

channel_exist(Server, Name) when ?is_string(Name, 64) ->
	gen_server:call(Server, {channel_exist, {Name}}).

channel_list(Server) ->
	gen_server:call(Server, channel_list).

channel_rename(Server, From, To) when ?is_string(From, 64) and ?is_string(To, 64) ->
	gen_server:call(Server, {channel_rename, {From, To}}).

channel_publish(Server, Name, Topic, Msg) when ?is_string(Name, 64) and ?is_string(Topic, 32) and is_binary(Msg) ->
	gen_server:call(Server, {channel_publish, {Name, Topic, Msg}}, infinity).

channel_subscribe(Server, Name, Topic) when ?is_string(Name, 64) and ?is_string(Topic, 32) ->
	gen_server:call(Server, {channel_subscribe, {Name, Topic}}).

channel_psubscribe(Server, Name, Pattern) when ?is_string(Name, 64) and ?is_string(Pattern, 32) ->
	gen_server:call(Server, {channel_psubscribe, {Name, Pattern}}).

channel_unsubscribe(Server, Name, Topic) when ?is_string(Name, 64) and ?is_string(Topic, 32) ->
	gen_server:call(Server, {channel_unsubscribe, {Name, Topic}}).

channel_punsubscribe(Server, Name, Pattern) when ?is_string(Name, 64) and ?is_string(Pattern, 32) ->
	gen_server:call(Server, {channel_punsubscribe, {Name, Pattern}}).

channel_delete(Server, Name) when ?is_string(Name, 64) ->
	gen_server:call(Server, {channel_delete, {Name}}).
