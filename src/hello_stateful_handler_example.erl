% Copyright (c) 2011 by Travelping GmbH <info@travelping.com>

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

-module(hello_stateful_handler_example).
-export([bind/0]).

-behaviour(hello_stateful_handler).
-export([method_info/1, param_info/2, init/2, handle_request/4, handle_info/3, terminate/3]).

-include("hello.hrl").

bind() ->
    hello:bind_stateful("zmq-ipc://hello-events.ipc", ?MODULE, []).

method_info(_State) ->
    [#rpc_method{name = subscribe},
     #rpc_method{name = ping},
     #rpc_method{name = echo}].

param_info(echo, _State) ->
    [#rpc_param{name = text, type = string}];
param_info(_, _State) ->
    [].

init(_Context, []) ->
    hello_stateful_handler:set_idle_timeout(35000),
    {ok, undefined}.

handle_request(_From, echo, [Text], State) ->
    {reply, {ok, Text}, State};
handle_request(_From, subscribe, _Args, State) ->
    timer:send_interval(1000, {event, timer}),
    {reply, {ok, <<"ok">>}, State};
handle_request(From, ping, _Args, State) ->
    timer:send_after(1000, {event, ping, From}),
    {noreply, State}.

handle_info(Context, {event, timer}, State) ->
    hello_stateful_handler:notify_np(Context, event, [{'type', timer}]),
    {noreply, State};

handle_info(_Context, {event, ping, From}, State) ->
    hello_stateful_handler:reply(From, {ok, <<"ok">>}),
    {noreply, State}.

terminate(_Context, _Reason, _State) ->
    ok.
