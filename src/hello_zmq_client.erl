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
-export([init_transport/2, send_request/2, terminate_transport/2, handle_info/2]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("internal.hrl").

-record(zmq_state, {
    client  :: pid(),
    socket  :: ezmq:socket()
}).

init_transport(URI, _Options) ->
    {ok, Socket} = ezmq:socket([{type, dealer}, {active, true}]),
    ok = ezmq_connect_url(Socket, URI),
    {ok, #zmq_state{socket = Socket}}.

send_request(Message, State = #zmq_state{socket = Socket}) ->
    ezmq:send(Socket, [<<>>, Message]),
    {ok, State}.

terminate_transport(_Reason, #zmq_state{socket = Socket}) ->
    ezmq:close(Socket).

handle_info({zmq, _Socket, [<<>>, Msg]}, State) ->
    {?INCOMING_MSG, {ok, Msg, State}}.

%% --------------------------------------------------------------------------------
%% -- helpers
zmq_protocol(#ex_uri{scheme = "zmq-tcp"})  -> inet;
zmq_protocol(#ex_uri{scheme = "zmq-tcp6"}) -> inet6.

%% use dnssd to resolve port AND host
%% map host to Type and Path to Name
ezmq_connect_url(_Socket, #ex_uri{authority = #ex_uri_authority{host = Host, port = undefined},
				  path = [$/|Path]}) ->
    dnssd:resolve(list_to_binary(Path), <<"_", (list_to_binary(Host))/binary, "._tcp.">>, <<"local.">>),
    ok;

ezmq_connect_url(Socket, URI = #ex_uri{authority = #ex_uri_authority{host = Host, port = Port}}) ->
    Protocol = zmq_protocol(URI),
    case ezmq_ip(Protocol, Host) of
        {ok, IP} ->
            ezmq:connect(Socket, tcp, IP, Port, [Protocol]);
        Other ->
            Other
    end.

ezmq_ip(inet, Host)  -> inet:parse_ipv4_address(Host);
ezmq_ip(inet6, Host) ->
    case re:run(Host, "^\\[(.*)\\]$", [{capture, all, list}]) of
        {match, ["[::1]", IP]} ->
            inet:parse_ipv6_address(IP);
        _ ->
            inet:parse_ipv6_address(Host)
    end.
