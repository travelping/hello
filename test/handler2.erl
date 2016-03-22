-module(handler2).

-behaviour(hello_handler).
-export([name/0, router_key/0, request/3, init/2, handle_request/4, handle_info/3, terminate/3, typespec/0]).
-export([send_async_reply/2]).

-include_lib("yang/include/typespec.hrl").
-include("hrl/handler2.hrl").

name() -> 'app2/test'.
 
router_key() -> 'handler2'.
 
request(_Module, Method, Params) -> {ok, Method, Params}.

init(_, Counter) ->
	{ok, Counter}.

handle_request(_Context, <<"handler2.fun1">>, [{_, Arg}], State) ->
	{reply, {ok, Arg}, State + 1};
handle_request(_Context, <<"handler2.fun2">>, [{_, Arg}], State) ->
	{reply, {ok, Arg}, State + 1};
handle_request(Context, <<"handler2.fun3">>, [{_, Arg}], State) ->
    spawn(?MODULE, send_async_reply, [Context, Arg]),
	{noreply, State + 1};
handle_request(Context, <<"handler2.fun4">>, [{_, Arg}], State) ->
    spawn(?MODULE, send_async_reply, [Context, Arg]),
	{noreply, State + 1};
handle_request(Context, <<"handler2.fun5">>, [{_, Arg}], State) ->
    hello_handler:notify(Context, [<<"notification_arg1">>, <<"notification_arg2">>]),
    {reply, {ok, Arg}, State + 1}.

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
