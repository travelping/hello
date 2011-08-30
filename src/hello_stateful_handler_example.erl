-module(hello_stateful_handler_example).
-export([bind/0]).

-behaviour(hello_stateful_handler).
-export([init/2, handle_request/4, handle_info/3, terminate/3]).

bind() ->
    hello:bind_stateful("zmq-ipc://hello-events.ipc", ?MODULE, []).

init(_Context, []) ->
    hello_stateful_handler:set_idle_timeout(5000),
    {ok, undefined}.

handle_request(_From, <<"subscribe">>, _Args, State) ->
    timer:send_interval(1000, {event, timer}),
    {reply, {ok, <<"ok">>}, State};
handle_request(_From, <<"ping">>, _Args, State) ->
    {noreply, State};
handle_request(_From, _Method, _Args, State) ->
    {reply, {error, {100, <<"bad method">>}}, State}.

handle_info(Context, {event, timer}, State) ->
    hello_stateful_handler:notify_np(Context, event, [{'type', timer}]),
    {noreply, State};

handle_info(_Context, _Info, State) ->
    {noreply, State}.

terminate(_Context, _Reason, _State) ->
    ok.
