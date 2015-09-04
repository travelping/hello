% Copyright 2010-2015, Travelping GmbH <info@travelping.com>

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

% @doc This module provide helpers for metrics

-module (hello_metrics).

-export([start_subscriptions/0, start_subscriptions/1, start_subscriptions/2,
         subscriptions/0, subscriptions/1]).

-export([packet_in/1,
         packet_out/1, 
         ok_request/0, ok_request/1,
         internal_request/0,
         error_request/0,
         response/0,
         handle_request_time/1,
         service/1,
         binding/1,
         listener/1,
         client/1]).

-include("hello_log.hrl").

-type metric() :: exometer_report:metric().
-type datapoint() :: exometer_report:datapoint(). 
-type interval() :: exometer_report:interval(). 
-type extra() :: exometer_report:extra(). 
-type subscription() :: {metric(), datapoint(), interval(), extra()}.
-type subscriptions() :: [subscription()].
-type metrics_type() :: packets | request | response | service | 
                        handler | binding | listener | client.

-export_type([metrics_type/0]).

-define(ALL_TYPES, [packets, request, response, service, handler, binding, listener, client]).
-define(DEFAULT_INTERVAL, 500).
-define(FAST_INTERVAL, trunc(?DEFAULT_INTERVAL / 3)).

-spec start_subscriptions() -> list().
start_subscriptions() ->
    [start_subscriptions(Reporter, ?ALL_TYPES) || {Reporter, _} <- exometer_report:list_reporters()].

-spec start_subscriptions(subscriptions()) -> list().
start_subscriptions(Types) ->
    [start_subscriptions(Reporter, Types) || {Reporter, _} <- exometer_report:list_reporters()].

-spec start_subscriptions(atom(), subscriptions()) -> list(atom()).
start_subscriptions(Reporter, Types) ->
    [exometer_report:subscribe(Reporter, Name, DataPoint, Time, [], true) 
     || {Name, DataPoint, Time} <- subscriptions(Types)].

-spec subscriptions() -> subscriptions().
subscriptions() -> 
    subscriptions(?ALL_TYPES).

-spec subscriptions(Type :: [metrics_type()] | metrics_type()) -> subscriptions().
subscriptions(Types) when is_list(Types) -> 
    lists:flatten([subscriptions(Type) || Type <- Types]);

subscriptions(packets) ->
    [{[hello, packet_in], value, ?DEFAULT_INTERVAL},
     {[hello, packet_in, size], median, ?DEFAULT_INTERVAL},
     {[hello, packet_in, size], min, ?DEFAULT_INTERVAL},
     {[hello, packet_in, size], max, ?DEFAULT_INTERVAL},
     {[hello, packet_in, per_sec], one, ?FAST_INTERVAL},
     {[hello, packet_out], value, ?DEFAULT_INTERVAL},
     {[hello, packet_out, size], median, ?DEFAULT_INTERVAL},
     {[hello, packet_out, size], min, ?DEFAULT_INTERVAL},
     {[hello, packet_out, size], max, ?DEFAULT_INTERVAL},
     {[hello, packet_out, per_sec], one, ?FAST_INTERVAL}];
subscriptions(request)  ->
    [{[hello, request], value, ?DEFAULT_INTERVAL},
     {[hello, request, ok], value, ?DEFAULT_INTERVAL},
     {[hello, request, error], value, ?DEFAULT_INTERVAL},
     {[hello, request, internal], value, ?DEFAULT_INTERVAL},
     {[hello, request, per_sec], one, ?FAST_INTERVAL}];
subscriptions(response) -> 
    [{[hello, response], value, ?DEFAULT_INTERVAL},
     {[hello, response, per_sec], one, ?FAST_INTERVAL}];
subscriptions(handler) -> 
    [{[hello, handle_request_time], max, ?DEFAULT_INTERVAL}, 
     {[hello, handle_request_time], mean, ?DEFAULT_INTERVAL}];
subscriptions(service) -> [{[hello, services], value, ?DEFAULT_INTERVAL}];
subscriptions(binding) -> [{[hello, bindings], value, ?DEFAULT_INTERVAL}];
subscriptions(listener) -> [{[hello, listeners], value, ?DEFAULT_INTERVAL}];
subscriptions(client) -> [{[hello, clients], value, ?DEFAULT_INTERVAL}];

subscriptions(Type) -> 
    ?LOG_DEBUG("Hello metrics received unknown subscription type '~p'.", [Type], [], ?LOGID50), [].


% @private
packet_in(Size) -> packet(packet_in, Size).

% @private
packet_out(Size) -> packet(packet_out, Size).

% @private
ok_request() -> ok_request(1).

% @private
ok_request(Value) -> request(ok, Value).

% @private
internal_request() -> request(internal, 1).

% @private
error_request() -> request(error, 1).

% @private
response() ->
    exometer:update_or_create([hello, response], 1, counter, []),
    exometer:update_or_create([hello, response, per_sec], 1, spiral, [{time_span, 1000}]).

handle_request_time(Time) ->
    exometer:update_or_create([hello, handle_request_time], Time, 
                              histogram, [{truncate, false}]).

% @private
service(Value) -> 
    exometer:update_or_create([hello, services], Value, counter, []).

% @private
binding(Value) -> 
    exometer:update_or_create([hello, bindings], Value, counter, []).

% @private
listener(Value) -> 
    exometer:update_or_create([hello, listeners], Value, counter, []).

% @private
client(Value) -> 
    exometer:update_or_create([hello, clients], Value, counter, []).

%% --------------------------------------------------------------------------------
%% -- Helpers
packet(Type, Size) ->
    exometer:update_or_create([hello, Type], 1, counter, []),
    exometer:update_or_create([hello, Type, size], Size, histogram, []),
    exometer:update_or_create([hello, Type, per_sec], 1, spiral, [{time_span, 1000}]).

request(Type, Value) ->
    exometer:update_or_create([hello, request], Value, counter, []),
    exometer:update_or_create([hello, request, Type], Value, counter, []),
    exometer:update_or_create([hello, request, per_sec], Value, spiral, [{time_span, 1000}]).
