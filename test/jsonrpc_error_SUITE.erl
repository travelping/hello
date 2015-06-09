-module(jsonrpc_error_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("hello_test.hrl").
-include("../include/jsonrpc_internal.hrl").
-include("../include/hello.hrl").

% ---------------------------------------------------------------------
% -- test cases
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
	unbind_all(),
    application:stop(hello).

% ---------------------------------------------------------------------
% -- helpers
start_named_clients() ->
	[ start_named_client(Transport) || Transport <- ?TRANSPORTS ].

start_named_client(Transport) ->
	Name = proplists:get_value(Transport, ?CLIENT_NAMES),
	{Url, TransportOpts} = Transport, 
	ProtocolOpts = [{protocol, hello_proto_jsonrpc}],
	{ok, _Pid} = hello_client:start_supervised(Name, Url ++ "/test", TransportOpts, ProtocolOpts, []).

unbind_all() ->
    [ hello:unbind(Url, CallbackMod) || {Url, _} <- ?TRANSPORTS, CallbackMod <- ?CALLBACK_MODS ],
    [] = hello_binding:all().

bind_transports() ->
	[ bind_url(Transport) || Transport <- ?TRANSPORTS ].

bind_url(Transport) ->
    [ ok = bind_url1(Transport, Handler) || Handler <- ?CALLBACK_MODS ].

bind_url1({Url, TransportOpts}, Handler) ->
    HandlerOpts = proplists:get_value(Handler, ?HANDLER_ARGS),
    ProtocolOpts = proplists:get_value(hello_proto_jsonrpc, ?PROTOCOL_ARGS),
    Self = self(),
    spawn(fun() ->
        hello:start_service(Handler, HandlerOpts),
        hello:start_listener(Url, TransportOpts, hello_proto_jsonrpc, ProtocolOpts, hello_router),
        true = hello:bind(Url, Handler),
        Self ! next,
        receive after infinity -> ok end
    end), 
    receive next -> ok end.
