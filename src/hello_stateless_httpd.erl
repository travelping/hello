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
-export([start/5, lookup_service/2, lookup_service/3]).
-export([unslash/1]).

-include("internal.hrl").
-define(HANDLER, hello_stateless_http_server).

start("http", Host, undefined, Path, CallbackModule) ->
    start("http", Host, 80, Path, CallbackModule);
start("http", Host, Port, Path, CallbackModule) ->
    case Host of
        "*" ->
            start_reg(<<"0.0.0.0">>, {0,0,0,0}, Port, Path, CallbackModule);
        _ ->
            case inet_parse:address(Host) of
                {error, einval} ->
                    start_reg(list_to_binary(Host), {0,0,0,0}, Port, Path, CallbackModule);
                {ok, IPAddr} ->
                    start_reg(list_to_binary(inet_parse:ntoa(IPAddr)), IPAddr, Port, Path, CallbackModule)
            end
    end.

start_reg(Host, IP, Port, Path, CallbackModule) ->
    BindingKey = hello_registry:binding_key(http, Host, Port, unslash(Path)),

    case hello_registry:lookup_listener(IP, Port) of
        {error, not_found} ->
            %% no server running on Host:Port, we need to start a listener
            {ok, ListenerPid} = start_listener(IP, Port),
            ListenerKey = hello_registry:listener_key(IP, Port),
            hello_registry:multi_register([{ListenerKey, ?MODULE}, {BindingKey, CallbackModule}], ListenerPid);
        {ok, ListenerPid, ?MODULE} ->
            %% listener is already running
            case hello_registry:lookup(BindingKey) of
                {error, not_found} ->
                    %% nobody is on that path yet, let's register the Module
                    ok = hello_registry:register(BindingKey, CallbackModule, ListenerPid);
                {ok, _Pid, CallbackModule} ->
                    {error, already_started};
                {ok, _Pid, _OtherModule} ->
                    {error, occupied}
            end;
        {ok, _OtherListener, _OtherModule} ->
            {error, occupied}
    end.

start_listener(IP, Port) ->
    Dispatch = [{'_', [{['...'], ?HANDLER, [IP]}]}],
    cowboy:start_listener(make_ref(), 100, cowboy_tcp_transport, [{port, Port}, {ip, IP}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]).

lookup_service(IP, Req) ->
    {Port, Req2}     = cowboy_http_req:port(Req),
    {PathList, Req3} = cowboy_http_req:path_info(Req2),
    case lookup_service(IP, Port, PathList) of
        undefined ->
            {Host, Req4} = cowboy_http_req:raw_host(Req),
            {lookup_service(Host, Port, PathList), Req4};
        Module ->
            {Module, Req3}
    end.

lookup_service(RegSpec, Port, PathList) ->
    case hello_registry:lookup_binding(http, RegSpec, Port, PathList) of
        {ok, _Pid, CallbackModule} -> CallbackModule;
        {error, not_found}         -> undefined
    end.

unslash(Path) ->
    case re:split(Path, "/", [{return, binary}]) of
        []            -> [];
        [<<>> | Rest] -> Rest;
        List          -> List
    end.
