-module(handler1).

-behaviour(hello_handler).
-export([name/0, router_key/0, request/3, init/2, handle_request/4, handle_info/3, terminate/3, typespec/0]).
-export([send_async_reply/2]).

-include_lib("yang/include/typespec.hrl").
-include("hrl/handler1.hrl").

name() -> 'app/test'.
 
router_key() -> 'handler1'.
 
validation() -> t.
request(_Module, Method, Params) -> {ok, Method, Params}.

init(A, Counter) ->
	{ok, Counter}.

handle_request(_Context, <<"handler1.fun1">>, [{_, Arg}] = Args, State) ->
	{reply, {ok, Arg}, State + 1};
handle_request(_Context, <<"handler1.fun2">>, [{_, Arg}], State) ->
	{reply, {ok, Arg}, State + 1};
handle_request(Context, <<"handler1.fun3">>, [{_, Arg}], State) ->
    spawn(?MODULE, send_async_reply, [Context, Arg]),
	{noreply, State + 1};
handle_request(Context, <<"handler1.fun4">>, [{_, Arg}], State) ->
    spawn(?MODULE, send_async_reply, [Context, Arg]),
	{noreply, State + 1};
handle_request(Context, <<"handler1.fun5">>, [{_, Arg}], State) ->
	hello_handler:notify(Context, <<"notification">>, [<<"notification_arg1">>, <<"notification_arg2">>]),
	{reply, {ok, Arg}, State + 1};
handle_request(_Context, <<"handler1.fun6">>, [{<<"arg1">>, Arg1}, {<<"arg2">>, Arg2}], State) ->
	{reply, {ok, [Arg1, Arg2]}, State + 1}.
	

handle_info(_Context, _Message, State) ->
	{noreply, State}.

terminate(_Context, _Reason, _State) ->
	ok.


%% -----------------------------------------------------------------------
%% -- helpers
send_async_reply(Context, Result) ->
    Time = round( random:uniform() * 100 ),
    timer:sleep(Time),
    hello_handler:reply(Context, Result).
