-module(hello_stateful_handler_example).
-export([bind/0]).

-behaviour(hello_stateful_handler).
-export([method_info/1, param_info/2, init/2, handle_request/4, handle_info/3, terminate/3]).

-include("hello.hrl").

bind() ->
    hello:bind_stateful("zmq-ipc://hello-events.ipc", ?MODULE, []).

method_info(_State) ->
    [#rpc_method{name = subscribe},
     #rpc_method{name = ping}].

param_info(_, _State) ->
    [].

init(_Context, []) ->
    hello_stateful_handler:set_idle_timeout(5000),
    {ok, undefined}.

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
