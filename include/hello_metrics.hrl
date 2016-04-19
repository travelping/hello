%% This file contains the definitions for all metrics used by exometer.
%%
%% First the used entries and probes for exometer will be described and
%% then the actual metrics. These definitions contain the way metrics are
%% exposed, e.g. for requests:
%%
%%   - request handle time (gauge)
%%   - request counter (counter)
%%
%% In a similar way this holds for other metrics.


-type listener_metrics_name() :: atom().
-type handler_metrics_name() :: atom().
-type client_metrics_name() :: atom().
-type atom_ip() :: atom().
-type atom_port() :: atom().
-type client_metrics_info() :: {client_metrics_name(), atom_ip(), atom_port()}.
-type handler_metrics_info() :: {handler_metrics_name(), listener_metrics_name(), atom_ip(), atom_port()}.
-type listener_metrics_info() :: {listener_metrics_name(), atom_ip(), atom_port()}.
-type request_type() :: atom().
-type packet_size() :: integer().


%% this will be prepended to all eradius metrics
-define(DEFAULT_ENTRIES, [hello]).

-define(METRICS, [{listener, ?LISTENER_METRICS},
                  {handler,  ?HANDLER_METRICS},
                  {client,   ?CLIENT_METRICS}]).

%% exometer basic configuration used for metrics
-define(COUNTER,        {counter,   %% exometer type
                         []}).      %% type options

-define(GAUGE,          {gauge,
                         []}).

-define(HISTOGRAM_60000, {histogram,
                         [{slot_period, 100},
                          {time_span, 60000}]}).

-define(FUNCTION_UPTIME,{{function,
                          hello_metrics,
                          update_listener_uptime,
                          undefined,
                          proplist,
                          [value]},
                         []}).

-define(BASIC_REQUEST_METRICS, [
     {request, total, [
       {gauge, ?HISTOGRAM_60000},
       {counter, ?COUNTER}]},
     {request, success, [
       {gauge, ?HISTOGRAM_60000},
       {counter, ?COUNTER}]},
     {request, error, [
       {gauge, ?HISTOGRAM_60000},
       {counter, ?COUNTER}]},

     {time, last_request, [
       {ticks, ?GAUGE}]}
     ]).

-define(LISTENER_METRICS, [
     {request, internal, [
       {gauge, ?HISTOGRAM_60000},
       {counter, ?COUNTER}]},
     {time, last_reset, [
       {ticks, ?GAUGE}]},
     {time, up, [
       {ticks, ?FUNCTION_UPTIME}]},
     {packet, in, [
       {counter, ?COUNTER},
       {size, ?HISTOGRAM_60000}]},
     {packet, out, [
       {counter, ?COUNTER},
       {size, ?HISTOGRAM_60000}]}
     ] ++ ?BASIC_REQUEST_METRICS).

-define(CLIENT_METRICS, [
     {request, internal, [
       {gauge, ?HISTOGRAM_60000},
       {counter, ?COUNTER}]},
     {request, timeout, [
       {gauge, ?HISTOGRAM_60000},
       {counter, ?COUNTER}]}
     ] ++ ?BASIC_REQUEST_METRICS).

-define(HANDLER_METRICS, ?BASIC_REQUEST_METRICS).
