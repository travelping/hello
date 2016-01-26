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

-export([subscribe_client/1, subscribe_server/1,
         unsubscribe_client/1, unsubscribe_server/1,
         start_subscriptions/0, start_subscriptions/1, start_subscriptions/2,
         subscriptions/0, subscriptions/1]).

-export([packet_in/2,
         packet_out/2, 
         ok_request/1, ok_request/2,
         internal_request/1,
         error_request/1,
         handle_request_time/2,
         service/1,
         binding/1,
         listener/1,
         client/1,
         client_ok_request/1,
         client_error_request/1,
         client_internal_request/1,
         client_request_handle_time/2,
         client_ping_pong_latency/2]).

-include("hello_log.hrl").

-type metric() :: exometer_report:metric().
-type datapoint() :: exometer_report:datapoint(). 
-type interval() :: exometer_report:interval(). 
-type extra() :: exometer_report:extra(). 
-type subscription() :: {metric(), datapoint(), interval(), extra()}.
-type subscriptions() :: [subscription()].
-type metrics_type() :: client | server | common.

-export_type([metrics_type/0]).

-define(ALL_TYPES, [client, server, common]).
-define(DEFAULT_INTERVAL, 1000).
-define(DEFAULT_INTERVAL_2, ?DEFAULT_INTERVAL * 2).

-spec subscribe_client(Name :: list()) -> list().
subscribe_client(Name) ->
    {ok, Metrics} = application:get_env(hello, metrics),
    case lists:member(client, Metrics) of
        true -> subscribe_client_(Name);
        false -> []
    end.

% @private
subscribe_client_(Name) ->
    [exometer_report:subscribe(Reporter, Metric, DataPoint, ?DEFAULT_INTERVAL, Tags, true) 
     || {Metric, DataPoint, Tags} <- client_metrics(Name),
        {Reporter, _} <- exometer_report:list_reporters()].

% @private
client_metrics(Name) when is_atom(Name) -> 
    Tags = [{client, {from_name, 3}}],
    TagsWithType = Tags ++ [{type, {from_name, 5}}],
    [{[hello, client, Name, requests, ok, per_sec], one, TagsWithType},
     {[hello, client, Name, requests, error, per_sec], one, TagsWithType},
     {[hello, client, Name, requests, internal, per_sec], one, TagsWithType},
     {[hello, client, Name, request, handle_time], max, Tags},
     {[hello, client, Name, request, handle_time], mean, Tags},
     {[hello, client, Name, ping_pong_latency], max, Tags},
     {[hello, client, Name, ping_pong_latency], mean, Tags}];
client_metrics(Name) -> client_metrics(convert_name(Name)).

-spec unsubscribe_client(Name :: list()) -> list().
unsubscribe_client(Name) ->
    [begin
         exometer_report:unsubscribe_all(Reporter, Metric),
         exometer:delete(Metric)
     end || {Metric, _, _} <- client_metrics(Name),
            {Reporter, _} <- exometer_report:list_reporters()].

-spec subscribe_server(Name :: list()) -> list().
subscribe_server(Name) ->
    {ok, Metrics} = application:get_env(hello, metrics),
    case lists:member(server, Metrics) of
        true -> subscribe_server_(Name);
        false -> []
    end.

% @private
subscribe_server_(Name) ->
    [exometer_report:subscribe(Reporter, Metric, DataPoint, ?DEFAULT_INTERVAL, Tags, true)
     || {Metric, DataPoint, Tags} <- server_metrics(Name),
        {Reporter, _} <- exometer_report:list_reporters()].

-spec unsubscribe_server(Name :: list()) -> list().
unsubscribe_server(Name) ->
    [begin 
         exometer_report:unsubscribe_all(Reporter, Metric),
         exometer:delete(Metric)
     end || {Metric, _, _} <- server_metrics(Name),
            {Reporter, _} <- exometer_report:list_reporters()].

% @private
server_metrics(Name) when is_atom(Name) -> 
    Tags = [{server, {from_name, 3}}],
    TagsWithType = Tags ++ [{type, {from_name, 5}}],
    [{[hello, server, Name, requests, ok, per_sec], one, TagsWithType},
     {[hello, server, Name, requests, error, per_sec], one, TagsWithType},
     {[hello, server, Name, requests, internal, per_sec], one, TagsWithType},
     {[hello, server, Name, request, handle_time], max, Tags},
     {[hello, server, Name, request, handle_time], mean, Tags},
     {[hello, server, Name, packets_in, size], max, Tags},
     {[hello, server, Name, packets_in, size], mean, Tags},
     {[hello, server, Name, packets_in, per_sec], one, Tags},
     {[hello, server, Name, packets_out, size], max, Tags},
     {[hello, server, Name, packets_out, size], mean, Tags},
     {[hello, server, Name, packets_out, per_sec], one, Tags}];
server_metrics(Name) -> server_metrics(convert_name(Name)).

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

subscriptions(client) -> [];
subscriptions(server) -> [];
subscriptions(common) -> 
    [{[hello, services], value, ?DEFAULT_INTERVAL_2},
     {[hello, bindings], value, ?DEFAULT_INTERVAL_2},
     {[hello, listeners], value, ?DEFAULT_INTERVAL_2},
     {[hello, clients], value, ?DEFAULT_INTERVAL_2}];

subscriptions(Type) -> 
    ?LOG_DEBUG("Hello metrics received unknown subscription type ~p.", [Type], [], ?LOGID50), [].

%% --------------------------------------------------------------------------------
%% -- Collect Metrics
packet_in(Name, Size) -> packet(convert_name(Name), packets_in, Size).

packet_out(Name, Size) -> packet(convert_name(Name), packets_out, Size).

ok_request(Name) -> ok_request(convert_name(Name), 1).

ok_request(Name, Value) -> request(convert_name(Name), ok, Value).

internal_request(Name) -> request(convert_name(Name), internal, 1).

error_request(Name) -> request(convert_name(Name), error, 1).

handle_request_time(Name, Time) ->
    exometer:update_or_create([hello, server, convert_name(Name), request, handle_time], 
                              Time, histogram, [{truncate, false}]).

service(Value) -> 
    exometer:update_or_create([hello, services], Value, counter, []).

binding(Value) -> 
    exometer:update_or_create([hello, bindings], Value, counter, []).

listener(Value) -> 
    exometer:update_or_create([hello, listeners], Value, counter, []).

client(Value) -> 
    exometer:update_or_create([hello, clients], Value, counter, []).

client_ok_request(Name) -> client_request(convert_name(Name), ok).

client_error_request(Name) -> client_request(convert_name(Name), error).

client_internal_request(Name) -> client_request(convert_name(Name), internal).

client_request_handle_time(Name, Time) ->
    exometer:update_or_create([hello, client, convert_name(Name), request, handle_time], 
                              Time, histogram, [{truncate, false}]).

client_ping_pong_latency(Name, Time) ->
    exometer:update_or_create([hello, client, convert_name(Name), ping_pong_latency], 
                              Time, histogram, [{truncate, false}]).

%% --------------------------------------------------------------------------------
%% -- Helpers
packet(Name, Type, Size) ->
    exometer:update_or_create([hello, server, Name, Type, size], Size, histogram, []),
    exometer:update_or_create([hello, server, Name, Type, per_sec], 1, spiral, [{time_span, 1000}]).

request(Name, Type, Value) ->
    exometer:update_or_create([hello, server, Name, requests, Type, per_sec], Value, spiral, [{time_span, 1000}]).

client_request(Name, Type) ->
    exometer:update_or_create([hello, client, Name, requests, Type, per_sec], 1, 
                              spiral, [{time_span, 1000}]).

convert_name(Name) when is_atom(Name) -> Name;
convert_name(Name) when is_list(Name) -> list_to_atom(Name);
convert_name(Name) when is_binary(Name) -> convert_name(binary_to_list(Name));
convert_name(Name) when is_pid(Name) -> convert_name(pid_to_list(Name)).
