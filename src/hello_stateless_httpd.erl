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
-module(hello_stateless_httpd).
-export([start/5, lookup_service/1]).

-include("internal.hrl").
-define(HANDLER, hello_stateless_http_server).

start("http", Host, undefined, Path, CallbackModule) ->
    start("http", Host, 80, Path, CallbackModule);
start("http", Host, Port, Path, CallbackModule) ->
    %% TODO: handle IP addresses
    SimpleKey = {http, host_key(Host), Port},
    PathKey   = {http, host_key(Host), Port, unslash(Path)},
    case hello_registry:lookup(SimpleKey) of
        {error, not_found} ->
            %% no server running on Host:Port, we need to start a listener
            Dispatch = [{'_', [{['...'], ?HANDLER, []}]}],
            {ok, ListenerPid} = start_listener(Port, Dispatch),
            hello_registry:multi_register([{SimpleKey, []}, {PathKey, CallbackModule}], ListenerPid);
        {ok, ListenerPid, _} ->
            %% listener is already running
            case hello_registry:lookup(PathKey) of
                {error, not_found} ->
                    %% nobody is on that path yet, let's register the Module
                    ok = hello_registry:register(PathKey, CallbackModule, ListenerPid);
                {ok, _Pid, CallbackModule} ->
                    {error, already_started};
                {ok, _Pid, _OtherModule} ->
                    {error, occupied}
            end
    end.

start_listener(Port, Dispatch) ->
    cowboy:start_listener(make_ref(), 100, cowboy_tcp_transport, [{port, Port}], cowboy_http_protocol, [{dispatch, Dispatch}]).

lookup_service(Req) ->
    {Host, Req1}     = cowboy_http_req:raw_host(Req),
    {Port, Req2}     = cowboy_http_req:port(Req1),
    {PathList, Req3} = cowboy_http_req:path_info(Req2),
    PathKey          = {http, Host, Port, PathList},
    case hello_registry:lookup(PathKey) of
        {ok, _Pid, CallbackModule} ->
            {CallbackModule, Req3};
        {error, not_found} ->
            {undefined, Req3}
    end.

unslash(Path) ->
    case re:split(Path, "/", [{return, binary}]) of
        []            -> [];
        [<<>> | Rest] -> Rest;
        List          -> List
    end.

host_key("*")  -> '_';
host_key(Host) -> list_to_binary(Host).
