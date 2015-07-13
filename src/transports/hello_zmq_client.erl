% Copyright 2012, Travelping GmbH <info@travelping.com>

% Permission is hereby granted, free of charge, to any person obtaining a
% copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
% DEALINGS IN THE SOFTWARE.

% @private
-module(hello_zmq_client).

-behaviour(hello_client).
-export([init_transport/2, send_request/3, terminate_transport/2, handle_info/2, update_service/2]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("hello.hrl").
-include("hello_log.hrl").

-record(zmq_state, {
    client  :: pid(),
    uri     :: #ex_uri{},
    socket  :: pid()
}).

init_transport(URI, _Options) ->
    {ok, Socket} = ezmq:socket([{type, dealer}, {active, true}]),
    Res = ezmq_connect_url(Socket, URI),
    {Res, #zmq_state{socket = Socket, uri = URI}}.

send_request(Message, Signature, State = #zmq_state{socket = Socket}) ->
    ezmq:send(Socket, [Signature, Message]),
    % TODO:
    %ezmq:send(Socket, [<<>>, Signature, Message]),
    {ok, State}.

terminate_transport(_Reason, #zmq_state{socket = Socket}) ->
    ezmq:close(Socket).

update_service({Host, Port, _Txt}, State = #zmq_state{uri = URI, socket = Socket}) ->
    Protocol = hello_zmq_listener:zmq_protocol(URI),
    R = ezmq:connect(Socket, tcp, Host, Port, [Protocol]),
    ?LOG_DEBUG("ezmq:connect: ~p", [R]),
    {ok, State}.

handle_info({zmq, _Socket, [Signature, Msg]}, State) ->
    {?INCOMING_MSG, {ok, Signature, Msg, State}};
handle_info({zmq, _Socket, [<<>>, Signature, Msg]}, State) ->
    {?INCOMING_MSG, {ok, Signature, Msg, State}}.

%% --------------------------------------------------------------------------------
%% -- helpers
ezmq_connect_url(_Socket, #ex_uri{authority = #ex_uri_authority{port = undefined}}) -> browse;

ezmq_connect_url(Socket, URI = #ex_uri{authority = #ex_uri_authority{host = Host, port = Port}}) ->
    Protocol = hello_zmq_listener:zmq_protocol(URI),
    {ok, IP} = hello_zmq_listener:ezmq_ip(Protocol, Host),
    ezmq:connect(Socket, tcp, IP, Port, [Protocol]).
