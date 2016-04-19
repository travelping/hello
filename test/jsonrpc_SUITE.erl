-module(jsonrpc_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("hello_test.hrl").
-include("../include/jsonrpc_internal.hrl").
-include("../include/hello.hrl").

% ---------------------------------------------------------------------
% -- test cases
call_service(_Config) ->
    {Req1, [Args1], _} = ?REQ11,
    {ok, Args1} = hello:call_service(atom_to_binary(handler1:name(), latin1), handler1:name(), {Req1, [Args1]}),
    {Req2, [Args2], _} = ?REQ21,
    {ok, Args2} = hello:call_service(atom_to_binary(handler2:name(), latin1), handler2:name(), {Req2, [Args2]}).


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
	[{ok, Arg} = hello_client:call(zmq_tcp_client, Request) || {Arg, Request} <- Requests].

all_mixed_batch_all_callbacks(_Config) ->
	Request = shuffle_requests(?ALL_MIXED_BATCH_ALL_CALLBACKS),
	Args = get_arg(Request),
	[ {ok, Args} = hello_client:call(ClientName, Request) || {_, ClientName} <- ?CLIENT_NAMES ].

named_parameter(_Config) ->
	[ {ok, [<<"arg1">>, <<"arg2">>]} = hello_client:call(ClientName, ?NAMED_PARAMETER_REQ) || {_, ClientName} <- ?CLIENT_NAMES ].

%% error tests
binding_not_found(_Config) ->
	%[ {ok, {binding_not_found, _Msg, _Data}} = hello_client:call(ClientName, ?DUMMY_REQ1) || {_, ClientName} <- ?CLIENT_NAMES ].
	[ {ok, #error{code = -32601}} = hello_client:call(ClientName, ?DUMMY_REQ1) || {_, ClientName} <- ?CLIENT_NAMES ].

method_not_found(_Config) ->
	[ {ok, #error{code = -32601}} = hello_client:call(ClientName, ?DUMMY_REQ2) || {_, ClientName} <- ?CLIENT_NAMES ].

invalid_params(_Config) ->
	[ {ok, #error{code = -32602}} = hello_client:call(ClientName, ?DUMMY_REQ3) || {_, ClientName} <- ?CLIENT_NAMES ],
	[ {ok, #error{code = -32602}} = hello_client:call(ClientName, ?DUMMY_REQ4) || {_, ClientName} <- ?CLIENT_NAMES ].

error_batch1(_Config) ->
	[ {ok, [{binding_not_found, _, _}, {binding_not_found, _, _}]} =
							hello_client:call(ClientName, ?DUMMY_BATCH1) || {_, ClientName} <- ?CLIENT_NAMES ],
	[ {ok, [?ARG11, {binding_not_found, _, _}]} =
							hello_client:call(ClientName, ?DUMMY_BATCH1) || {_, ClientName} <- ?CLIENT_NAMES ].

invalid_request(_Config) ->
    BinRequest = <<"{\"id\":123,\"params\":\"undefined\",\"method\":\"undefined\",\"jsonrpc\":\"2.0\"}">>,
    {error, #response{response = #error{code = -32600}}} = hello_proto_jsonrpc:decode(BinRequest, [], request).

invalid_notification(_Config) ->
    BinRequest = <<"{\"id\":null,\"params\":\"undefined\",\"method\":\"undefined\",\"jsonrpc\":\"2.0\"}">>,
    ignore = hello_proto_jsonrpc:decode(BinRequest, [], request).
% ---------------------------------------------------------------------
% -- common_test callbacks
all() ->
    [call_service,
     simple_one_shot,
     simple_batch,
     simple_notification,
     batch_notification,
     simple_async_oneshot,
     batch_async_oneshot,
     all_mixed_batch_one_callback,
     normal_batch_all_callbacks,
     %all_mixed_batch_all_callbacks,
     notify,
     named_parameter,
     binding_not_found,
     method_not_found,
     invalid_params,
     invalid_request,
     invalid_notification
    ].

init_per_suite(Config) ->
    hello:start(),
    [ code:ensure_loaded(Callback) || Callback <- ?CALLBACK_MODS ],
    bind_all(),
    start_named_clients(),
    Config.

end_per_suite(_Config) ->
	unbind_all(),
    application:stop(hello).

% ---------------------------------------------------------------------
% -- helpers
bind_all() ->
    [ ok = bind_all1(Transport, Handler, hello_proto_jsonrpc) || Transport <- ?TRANSPORTS, Handler <- ?CALLBACK_MODS].

bind_all1({Url, TransportOpts}, Handler, Protocol) ->
    HandlerOpts = proplists:get_value(Handler, ?HANDLER_ARGS),
    ProtocolOpts = proplists:get_value(Protocol, ?PROTOCOL_ARGS),
    Self = self(),
    spawn(fun() ->
        hello:start_listener(Url, TransportOpts, Protocol, ProtocolOpts, hello_router),
        ok = hello:bind(Url, Handler, HandlerOpts),
        Self ! next,
        receive after infinity -> ok end
    end), 
    receive next -> ok end.

unbind_all() ->
    [ hello:unbind(Url, CallbackMod) || {Url, _} <- ?TRANSPORTS, CallbackMod <- ?CALLBACK_MODS ],
    [] = hello_binding:all().

start_named_clients() ->
	[ start_named_client(Transport) || Transport <- ?TRANSPORTS ],
    wait_clients().

start_named_client(Transport) ->
	Name = proplists:get_value(Transport, ?CLIENT_NAMES),
	{Url, TransportOpts} = Transport, 
	ProtocolOpts = [{protocol, hello_proto_jsonrpc}, {notification_sink, fun notification_fun/1}],
	{ok, _Pid} = hello_client:start_supervised(Name, Url, TransportOpts, ProtocolOpts, []).

shuffle_requests(Reqs) ->
	[ RandomReq || { _ , RandomReq} <- lists:sort([ {random:uniform(), Req} || Req <- Reqs])].

get_arg(Requests) when is_list(Requests) ->
	[ get_arg(Request) || Request = {_, _, Opts} <- Requests, Opts /= [{notification, true}] ];
get_arg({_, [Arg], _}) ->
	Arg.

wait_clients() ->
    case length(hello_client_sup:clients()) of
        L when L == length(?TRANSPORTS) -> ok;
        _ -> timer:sleep(1000), wait_clients()
    end.

notification_fun([<<"notification_arg1">>, <<"notification_arg2">>]) -> ok;
notification_fun(Args) when is_list(Args) ->
	BatchArgs = [[A || {_, [A], _} <- Requests] || Requests <- ?BATCH_NOTIFICATION],
    case lists:member(Args, BatchArgs) of
         true -> ok;
         false -> throw(error)
    end;
notification_fun(Arg) ->
	SimpleArgs = [A || _Request = {_, [A], _} <- ?SIMPLE_NOTIFICATION],
    case lists:member(Arg, SimpleArgs) of
         true -> ok;
         false -> throw(error)
    end.
