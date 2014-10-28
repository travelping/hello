-module(jsonrpc_error_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("hello_test.hrl").
-include("../include/jsonrpc_internal.hrl").

% ---------------------------------------------------------------------
% -- test cases
binding_not_found(_Config) ->
	[ {ok, {binding_not_found, _Msg, _Data}} = hello_client:call(ClientName, ?DUMMY_REQ1) || {_, ClientName} <- ?CLIENT_NAMES ].

method_not_found(_Config) ->
	[ {ok, {method_not_found, _Msg, _Data}} = hello_client:call(ClientName, ?DUMMY_REQ2) || {_, ClientName} <- ?CLIENT_NAMES ].

invalid_params(_Config) ->
	[ {ok, {invalid_params, _Msg, _Data}} = hello_client:call(ClientName, ?DUMMY_REQ3) || {_, ClientName} <- ?CLIENT_NAMES ],
	[ {ok, {invalid_params, _Msg, _Data}} = hello_client:call(ClientName, ?DUMMY_REQ4) || {_, ClientName} <- ?CLIENT_NAMES ].

error_batch1(_Config) ->
	[ {ok, [{binding_not_found, _, _}, {binding_not_found, _, _}]} =
							hello_client:call(ClientName, ?DUMMY_BATCH1) || {_, ClientName} <- ?CLIENT_NAMES ],
	[ {ok, [?ARG11, {binding_not_found, _, _}]} =
							hello_client:call(ClientName, ?DUMMY_BATCH1) || {_, ClientName} <- ?CLIENT_NAMES ].

invalid_request(_Config) ->
    BinRequest = <<"{\"id\":123,\"params\":\"undefined\",\"method\":\"undefined\",\"jsonrpc\":\"2.0\"}">>,
    {error, #jsonrpc_response{error = #jsonrpc_error{code = -32600}}} = hello_proto_jsonrpc:decode(BinRequest).

invalid_notification(_Config) ->
    BinRequest = <<"{\"id\":null,\"params\":\"undefined\",\"method\":\"undefined\",\"jsonrpc\":\"2.0\"}">>,
    {error, ignore} = hello_proto_jsonrpc:decode(BinRequest).

% ---------------------------------------------------------------------
% -- common_test callbacks
all() ->
    [binding_not_found,
     method_not_found,
     invalid_params,
     invalid_request,
     invalid_notification
     ].

init_per_suite(Config) ->
    hello:start(),
    [ code:load_file(Callback) || Callback <- ?CALLBACK_MODS ],
    bind_transports(),
    start_named_clients(),
    Config.

end_per_suite(_Config) ->
    application:stop(hello).

% ---------------------------------------------------------------------
% -- helpers
start_named_clients() ->
	[ start_named_client(Transport) || Transport <- ?TRANSPORTS ].

start_named_client(Transport) ->
	Name = proplists:get_value(Transport, ?CLIENT_NAMES),
	{Url, TransportOpts} = Transport, 
	ProtocolOpts = [{protocol, hello_proto_jsonrpc}],
	{ok, _Pid} = hello_client:start_supervised(Name, Url, TransportOpts, ProtocolOpts, []).

bind_transports() ->
	[ bind_url(Transport) || Transport <- ?TRANSPORTS ].

bind_url(Transport) ->
    [ ok = bind_url1(Transport, Handler) || Handler <- ?HANDLER ].

bind_url1({Url, TransportOpts}, Handler) ->
    [FirstCallback, SecondCallback] = proplists:get_value(Handler, ?CALLBACKS),
    HandlerOpts = proplists:get_value(Handler, ?HANDLER_ARGS),
    ok = hello:bind(Url, TransportOpts, FirstCallback, Handler, HandlerOpts, hello_proto_jsonrpc, []),
    ok = hello:bind(Url, TransportOpts, SecondCallback, Handler, HandlerOpts, hello_proto_jsonrpc, []).
