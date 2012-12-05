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

%% @private
-module(hello2_stateful_handler_example).
-export([bind/0]).

-behaviour(hello2_stateful_handler).
-export([method_info/1, param_info/2, init/2, handle_request/4, handle_info/3, terminate/3]).

-include("hello2.hrl").

bind() ->
    hello2:bind_stateful("zmq-ipc://hello-events.ipc", ?MODULE, []).

method_info(_State) ->
    [#rpc_method{name = subscribe},
     #rpc_method{name = ping},
     #rpc_method{name = echo}].

param_info(echo, _State) ->
    [#rpc_param{name = text, type = string}];
param_info(_, _State) ->
    [].

init(_Context, []) ->
    hello2_stateful_handler:set_idle_timeout(5000),
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
    Stamp = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(os:timestamp())),
    hello2_stateful_handler:notify_np(Context, event, [{'type', timer}, {'stamp', Stamp}]),
    {noreply, State};

handle_info(_Context, {event, ping, From}, State) ->
    hello2_stateful_handler:reply(From, {ok, <<"ok">>}),
    {noreply, State}.

terminate(Context, _Reason, _State) ->
    hello2_stateful_handler:notify_np(Context, event, [{'type', 'quit'}]).
