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
-export([listener_childspec/2, listener_key/1, binding_key/1, url_for_log/1]).

%% http utils used by other listeners
-export([lookup_binding/4, unslash/1, default_port/1, server_header/0, req_transport_params/1]).
-export([process/3]).

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
    Acceptors = 30,
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

url_for_log(#binding{url = URL}) ->
    list_to_binary(ex_uri:encode(URL)).

%% --------------------------------------------------------------------------------
%% -- request handling (callbacks for cowboy_http_handler)
init({tcp, http}, Req, _) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),
    case lists:member(Method, ['PUT', 'POST']) of
        true ->
            {Port, Req3} = cowboy_req:port(Req1),
            {PathList, Req4} = cowboy_req:path_info(Req3),
            {Host, Req5} = cowboy_req:raw_host(Req4),
            case lookup_binding(?MODULE, Host, Port, PathList) of
                {ok, Binding} ->
		    process(Binding, Req, State);
                {error, not_found} ->
                    {ok, Req6} = cowboy_req:reply(404, server_header(), Req5),
                    {ok, Req6, undefined}
            end;
        false ->
            {ok, Req2} = cowboy_req:reply(405, server_header(), Req1),
            {ok, Req2, undefined}
    end.

terminate(_Req, _State) ->
    ok.

process(Binding, Req, State) ->
    {Peer, Req1} = cowboy_req:peer(Req),
    {TransportParams, Req6} = req_transport_params(Req1),
    Handler = hello_binding:start_handler(Binding, Peer, self(), TransportParams),
    {ok, Body, Req7} = cowboy_req:body(Req6),
    hello_binding:incoming_message(Handler, Body),
    Req8 = cowboy_req:compact(Req7),
    {ok, Req9} = cowboy_req:chunked_reply(200, json_headers(), Req8),
    erlang:display(loop),
    http_chunked_loop(Handler, Req9, State).

http_chunked_loop(Handler, Request, State) ->
    receive
        {hello_closed, Handler, _Peer} ->
            {ok, Request, State};
        {hello_msg, Handler, _, Message} ->
            cowboy_req:chunk(Message, Request),
            http_chunked_loop(Handler, Request, State)
    end.

json_headers() ->
    {ok, Vsn} = application:get_key(hello, vsn),
    [{<<"Content-Type">>, <<"application/json">>},
     {<<"Server">>, erlang:list_to_binary("hello/" ++ Vsn)}].

server_header() ->
    {ok, Vsn} = application:get_key(hello, vsn),
    [{<<"Server">>, erlang:list_to_binary("hello/" ++ Vsn)}].

req_transport_params(Req1) ->
    {{PeerIP, PeerPort}, Req2} = cowboy_req:peer(Req1),
    {ProxyPeerIP, Req3} = cowboy_req:peer_addr(Req2),
    {QSVals, Req4} = cowboy_req:qs_vals(Req3),
    {Cookies, Req5} = cowboy_req:cookies(Req4),
    TransportParams = [{peer_ip, PeerIP},
                       {peer_port, PeerPort},
                       {real_peer_ip, ProxyPeerIP},
                       {query_params, QSVals},
                       {cookie_params, Cookies}],
    {TransportParams, Req5}.

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
