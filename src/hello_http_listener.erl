% Copyright (c) 2010-2011 by Travelping GmbH <info@travelping.com>

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
-module(hello_http_listener).
-behaviour(hello_binding).
-export([listener_childspec/2, listener_key/1, binding_key/1]).
-export([unslash/1]).

-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-include("internal.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").

%% --------------------------------------------------------------------------------
%% -- hello_binding callbacks
listener_childspec(ChildID, #binding{ip = IP, port = Port}) ->
    Dispatch = [{'_', [{['...'], ?MODULE, []}]}],

    %% Copied from cowboy.erl because it doesn't provide an API that
    %% allows supervising the listener from the calling application yet.
    Acceptors = 100,
    Transport = cowboy_tcp_transport,
    TransportOpts = [{port, default_port(Port)}, {ip, IP}],
    Protocol = cowboy_http_protocol,
    ProtocolOpts = [{dispatch, Dispatch}],
    Args = [Acceptors, Transport, TransportOpts, Protocol, ProtocolOpts],
    {ChildID, {cowboy_listener_sup, start_link, Args}, permanent, infinity, supervisor, [cowboy_listener_sup]}.

listener_key(#binding{ip = IP, port = Port}) ->
    hello_registry:listener_key(IP, default_port(Port)).

binding_key(#binding{host = Host, port = Port, path = Path}) ->
    {list_to_binary(Host), default_port(Port), unslash(Path)}.

default_port(undefined) -> 80;
default_port(Port)      -> Port.

%% --------------------------------------------------------------------------------
%% -- request handling (callbacks for cowboy_http_handler)
init({tcp, http}, Req, _) ->
    {ok, Req, undefined}.

handle(Req, _State) ->
    {Method, Req1} = cowboy_http_req:method(Req),
    case lists:member(Method, ['PUT', 'POST']) of
        true ->
            {BindingKey, Req2} = get_binding_key(Req1),
            {Peer, Req3} = cowboy_http_req:peer(Req2),
            HandlerPid = hello_binding:start_handler(?MODULE, BindingKey, Peer),
            {Body, Req4} = get_body(Req3),
            hello_binding:incoming_message(HandlerPid, Body),
            Req5 = cowboy_http_req:compact(Req4),
            {ok, Req6} = cowboy_http_req:chunked_reply(200, get_headers(), Req5),
            http_chunked_loop(HandlerPid, Req6);
        false ->
            {ok, Req2} = cowboy_http_req:reply(405, Req1),
            {ok, Req2, undefined}
    end.

terminate(_Req, _State) ->
    ok.

http_chunked_loop(HandlerPid, Request) ->
    receive
        {hello_closed, HandlerPid, _Peer} ->
            {ok, Request, undefined};
        {hello_msg, HandlerPid, _, Message} ->
            cowboy_http_req:chunk(Message, Request),
            http_chunked_loop(HandlerPid, Request)
    end.

get_headers() ->
    {ok, Vsn} = application:get_key(hello, vsn),
    [{'Content-Type', <<"application/json">>},
     {'Server', erlang:list_to_binary("hello/" ++ Vsn)}].

get_binding_key(Req) ->
    {Port, Req2} = cowboy_http_req:port(Req),
    {PathList, Req3} = cowboy_http_req:path_info(Req2),
    {Host, Req4} = cowboy_http_req:raw_host(Req3),
    {{Host, Port, PathList}, Req4}.

get_body(Req) ->
    case cowboy_http_req:body(Req) of
        {ok, Body, Req2} -> {Body, Req2};
        {error, badarg}  -> {<<>>, Req}
    end.

unslash(Path) ->
    case re:split(Path, "/", [{return, binary}]) of
        []            -> [];
        [<<>> | Rest] -> Rest;
        List          -> List
    end.
