-module(jsonrpc_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("hello_test.hrl").

% ---------------------------------------------------------------------
% -- test cases
simple_one_shot(_Config) ->
	Args = get_arg(?SIMPLE_ONE_SHOT),
	Requests = lists:zip(Args, ?SIMPLE_ONE_SHOT),
	[ {ok, Arg} = hello_client:call(ClientName, Request) || {_, ClientName} <- ?CLIENT_NAMES, {Arg, Request} <- Requests ].

simple_batch(_Config) ->
	Args = [ get_arg(Batch) || Batch <- ?SIMPLE_BATCH ],
	Requests = lists:zip(Args, ?SIMPLE_BATCH),
	[ {ok, Arg} = hello_client:call(ClientName, Request) || {_, ClientName} <- ?CLIENT_NAMES, {Arg, Request} <- Requests ].

simple_notification(_Config) ->
	[ ok = hello_client:call(ClientName, Request) || {_, ClientName} <- ?CLIENT_NAMES, Request <- ?SIMPLE_NOTIFICATION ].

batch_notification(_Config) ->
	[ ok = hello_client:call(ClientName, Request) || {_, ClientName} <- ?CLIENT_NAMES, Request <- ?BATCH_NOTIFICATION ].

simple_async_oneshot(_Config) ->
	Args = get_arg(?SIMPLE_ASYNC_ONESHOT),
	Requests = lists:zip(Args, ?SIMPLE_ASYNC_ONESHOT),
	[ {ok, Arg} = hello_client:call(ClientName, Request) || {_, ClientName} <- ?CLIENT_NAMES, {Arg, Request} <- Requests ].

batch_async_oneshot(_Config) ->
	Args = [ get_arg(Batch) || Batch <- ?BATCH_ASYNC_ONESHOT ],
	Requests = lists:zip(Args, ?BATCH_ASYNC_ONESHOT),
	[ {ok, Arg} = hello_client:call(ClientName, Request) || {_, ClientName} <- ?CLIENT_NAMES, {Arg, Request} <- Requests ].

all_mixed_batch_one_callback(_Config) ->
	Args = [ get_arg(Batch) || Batch <- ?ALL_MIXED_BATCH_ONE_CALLBACK ],
	Requests = lists:zip(Args, ?ALL_MIXED_BATCH_ONE_CALLBACK),
	[ {ok, Arg} = hello_client:call(ClientName, Request) || {_, ClientName} <- ?CLIENT_NAMES, {Arg, Request} <- Requests ].

normal_batch_all_callbacks(_Config) ->
	Request = shuffle_requests(?NORMAL_BATCH_ALL_CALLBACKS),
	Args = get_arg(Request),
	[ {ok, Args} = hello_client:call(ClientName, Request) || {_, ClientName} <- ?CLIENT_NAMES ].

notify(_Config) ->
	Args = get_arg(?NOTIFY_REQS),
	Requests = lists:zip(Args, ?NOTIFY_REQS),
	[ {ok, Arg} = hello_client:call(ClientName, Request) || {_, ClientName} <- ?CLIENT_NAMES, {Arg, Request} <- Requests].

all_mixed_batch_all_callbacks(_Config) ->
	Request = shuffle_requests(?ALL_MIXED_BATCH_ALL_CALLBACKS),
	Args = get_arg(Request),
	[ {ok, Args} = hello_client:call(ClientName, Request) || {_, ClientName} <- ?CLIENT_NAMES ].

named_parameter(_Config) ->
	[ {ok, [<<"arg1">>, <<"arg2">>]} = hello_client:call(ClientName, ?NAMED_PARAMETER_REQ) || {_, ClientName} <- ?CLIENT_NAMES ].
% ---------------------------------------------------------------------
% -- common_test callbacks
all() ->
	[simple_one_shot,
	 simple_batch,
	 simple_notification,
	 batch_notification,
	 simple_async_oneshot,
	 batch_async_oneshot,
	 all_mixed_batch_one_callback,
     normal_batch_all_callbacks,
     all_mixed_batch_all_callbacks,
     notify,
     named_parameter
     ].

init_per_suite(Config) ->
    hello:start(),
    bind_all(),
    [ code:ensure_loaded(Callback) || Callback <- ?CALLBACK_MODS ],
    start_named_clients(),
    Config.

end_per_suite(_Config) ->
	unbind_all(),
    application:stop(hello).

% ---------------------------------------------------------------------
% -- helpers
bind_all() ->
    [ ok = bind_all1(Transport, Handler, Protocol) || Transport <- ?TRANSPORTS, Handler <- ?HANDLER, Protocol <- ?PROTOCOLS ].

bind_all1({Url, TransportOpts}, Handler, Protocol) ->
    [FirstCallback, SecondCallback] = proplists:get_value(Handler, ?CALLBACKS),
    HandlerOpts = proplists:get_value(Handler, ?HANDLER_ARGS),
    ProtocolOpts = proplists:get_value(Protocol, ?PROTOCOL_ARGS),
    ok = hello:bind(Url, TransportOpts, FirstCallback, Handler, HandlerOpts, Protocol, ProtocolOpts),
    ok = hello:bind(Url, TransportOpts, SecondCallback, Handler, HandlerOpts, Protocol, ProtocolOpts).

unbind_all() ->
    Bindings = hello:bindings(),
    [ hello:unbind(Url, CallbackMod) || {Url, CallbackMod, _, _} <- Bindings],
    [] = hello:bindings().

start_named_clients() ->
	[ start_named_client(Transport) || Transport <- ?TRANSPORTS ].	

start_named_client(Transport) ->
	Name = proplists:get_value(Transport, ?CLIENT_NAMES),
	{Url, TransportOpts} = Transport, 
	NotificationSink = spawn(?MODULE, notification_sink, []),
	ProtocolOpts = [{protocol, hello_proto_jsonrpc}, {notification_sink, NotificationSink}],
	{ok, _Pid} = hello_client:start_supervised(Name, Url, TransportOpts, ProtocolOpts, []).

shuffle_requests(Reqs) ->
	[ RandomReq || { _ , RandomReq} <- lists:sort([ {random:uniform(), Req} || Req <- Reqs])].

get_arg(Requests) when is_list(Requests) ->
	[ get_arg(Request) || Request = {_, _, Opts} <- Requests, Opts /= [{notification, true}] ];
get_arg({_, [Arg], _}) ->
	Arg.

notification_sink() ->
	notification_acceptor().

notification_acceptor() ->
	receive 
		{notification, ?NOTIFICATION_METHOD, ?NOTIFICATION_ARGS} ->
			notification_acceptor();
		_ ->
			throw(error)
	end.
