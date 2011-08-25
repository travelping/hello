-module(hello_stateful_handler_example).

-behaviour(hello_stateful_handler).
-export([init/2, handle_request/4, handle_info/3, terminate/3]).

init(_Context, [Shell]) ->
    link(Shell),
    hello_stateful_handler:set_session_timeout(2000),
    {ok, Shell}.

handle_request(From, <<"return_whatever">>, _Args, State) ->
    {reply, {ok, <<"whatever">>}, State};
handle_request(_From, _Method, _Args, State) ->
    {reply, {error, 100, <<"bad method">>}, State}.

handle_info(Context, timeout, State) ->
    {stop, oops, State};
handle_info(Context, {event, Type, EvtParams}, State) ->
    hello_stateful_handler:notify_np(Context, event, [{'type', Type}, {'params', EvtParams}]),
    {noreply, State};
handle_info(_Context, _Info, State) ->
    {noreply, State}.

terminate(_Context, _Reason, State) ->
    io:format("terminate called~n", []),
    ok.
