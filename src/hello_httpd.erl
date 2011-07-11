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
-module(hello_httpd).
-export([start/5, stop/1, lookup_service/1]).

-include("internal.hrl").
-define(HANDLER, hello_stateless_http_server).

start("http", Host, undefined, Path, CallbackModule) ->
    start("http", Host, 80, Path, CallbackModule);
start("http", Host, Port, Path, CallbackModule) ->
    %% TODO: handle IP addresses
    MatchPath = unslash(Path),
    Key = {http, Host, Port, MatchPath},
    case ets:lookup(?HANDLER_TAB, Key) of
        [] ->
            Dispatch   = [{transform_host(Host), [{['...'], ?HANDLER, []}]}],
            ServerName = make_ref(),
            ets:insert(?HANDLER_TAB, {Key, CallbackModule}),
            cowboy:start_listener(ServerName, 100,
                cowboy_tcp_transport, [{port, Port}],
                cowboy_http_protocol, [{dispatch, Dispatch}]);
        [{_Key, CallbackModule}] ->
            {error, already_started};
        [{_Key, _}]->
            {error, occupied}
    end.

stop(ServerName) ->
    cowboy:stop_listener(ServerName).

lookup_service(Req) ->
    {Host, Req1} = cowboy_http_req:raw_host(Req),
    {Port, Req2} = cowboy_http_req:port(Req1),
    {Path, Req3} = cowboy_http_req:path_info(Req2),
    case ets:lookup(?HANDLER_TAB, {http, binary_to_list(Host), Port, Path}) of
        [] ->
            {undefined, Req3};
        [{_, CallbackModule}] ->
            {CallbackModule, Req3}
    end.

unslash(Path) ->
    case re:split(Path, "/", [{return, binary}]) of
        []            -> [];
        [<<>> | Rest] -> Rest;
        List          -> List
    end.

transform_host("*") -> '_';
transform_host(Host) -> re:split(Host, "\\.", [{return,binary}]).
