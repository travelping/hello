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
-export([lookup_binding/4, unslash/1, default_port/1, server_header/0]).

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

%% --------------------------------------------------------------------------------
%% -- request handling (callbacks for cowboy_http_handler)
init({tcp, http}, Req, _) ->
    {ok, Req, undefined}.

handle(Req, _State) ->
    {Method, Req1} = cowboy_http_req:method(Req),
    case lists:member(Method, ['PUT', 'POST']) of
        true ->
            {Peer, Req2} = cowboy_http_req:peer(Req1),
            {Port, Req3} = cowboy_http_req:port(Req2),
            {PathList, Req4} = cowboy_http_req:path_info(Req3),
            {Host, Req5} = cowboy_http_req:raw_host(Req4),
            case lookup_binding(?MODULE, Host, Port, PathList) of
                {ok, Binding} ->
                    Handler = hello_binding:start_handler(Binding, Peer, self()),
                    {ok, Body, Req6} = cowboy_http_req:body(Req5),
                    hello_binding:incoming_message(Handler, Body),
                    Req7 = cowboy_http_req:compact(Req6),
                    {ok, Req8} = cowboy_http_req:chunked_reply(200, json_headers(), Req7),
                    http_chunked_loop(Handler, Req8);
                {error, not_found} ->
                    {ok, Req6} = cowboy_http_req:reply(404, server_header(), Req5),
                    {ok, Req6, undefined}
            end;
        false ->
            {ok, Req2} = cowboy_http_req:reply(405, server_header(), Req1),
            {ok, Req2, undefined}
    end.

terminate(_Req, _State) ->
    ok.

http_chunked_loop(Handler, Request) ->
    receive
        {hello_closed, Handler, _Peer} ->
            {ok, Request, undefined};
        {hello_msg, Handler, _, Message} ->
            cowboy_http_req:chunk(Message, Request),
            http_chunked_loop(Handler, Request)
    end.

json_headers() ->
    {ok, Vsn} = application:get_key(hello, vsn),
    [{'Content-Type', <<"application/json">>},
     {'Server', erlang:list_to_binary("hello/" ++ Vsn)}].

server_header() ->
    {ok, Vsn} = application:get_key(hello, vsn),
    [{'Server', erlang:list_to_binary("hello/" ++ Vsn)}].

lookup_binding(Module, Host, Port, PathList) ->
    case hello_registry:lookup_binding(Module, {Host, Port, PathList}) of
        {error, not_found} ->
            hello_registry:lookup_binding(Module, {<<"0.0.0.0">>, Port, PathList});
        Result ->
            Result
    end.

unslash(Path) ->
    case re:split(Path, "/", [{return, binary}]) of
        []            -> [];
        [<<>> | Rest] -> Rest;
        List          -> List
    end.

default_port(undefined) -> 80;
default_port(Port)      -> Port.
