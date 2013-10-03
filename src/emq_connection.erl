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

-module(emq_connection).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-import(emq_packet_encoder, [encode/1]).
-import(emq_packet_decoder, [decode_response_bodylen/1, decode/1]).

-define(TCP_OPTS, [binary, {packet, 0}, {active, false}, {nodelay, true}]).

-record(state, {socket}).

init({Host, Port}) ->
	case gen_tcp:connect(Host, Port, ?TCP_OPTS) of
		{ok, Socket} ->
			{ok, #state{socket = Socket}};
		Error ->
			{stop, Error}
	end.

handle_call(wait_event, _From, #state{socket = Socket} = State) ->
	Response = receive_response(Socket),
	{reply, decode({wait_event, Response}), State};

handle_call({Command, _Params} = Msg, _From, #state{socket = Socket} = State) ->
	gen_tcp:send(Socket, encode(Msg)),
	Response = receive_response(Socket),
	{reply, decode({Command, Response}), State};

handle_call(Command, _From, #state{socket = Socket} = State) ->
	gen_tcp:send(Socket, encode(Command)),
	Response = receive_response(Socket),
	{reply, decode({Command, Response}), State}.

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(normal, #state{socket = Socket}) ->
	gen_tcp:close(Socket),
	ok.

receive_response(Socket) ->
	{ok, Header} = gen_tcp:recv(Socket, 8),
	Bodylen = decode_response_bodylen(Header),
	Body =
	if
		Bodylen > 0 ->
			{ok, Response} = gen_tcp:recv(Socket, Bodylen),
			Response;
		Bodylen == 0 ->
			<<>>
	end,
	<<Header/binary, Body/binary>>.
