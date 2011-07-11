-module(hello_simple_client_SUITE).
-compile(export_all).

-include("ct.hrl").
-define(HOST, "http://localhost:5671/rpc/example").
-define(UNKNOWN_HOST, "http://undefined.undefined:8888").

% ---------------------------------------------------------------------
% -- test cases
call(_Config) ->
    {ok,<<"abcdef">>} = hello_simple_client:call(?HOST, "append", [<<"abc">>,<<"def">>]).

call_errors(_Config) ->
    {error, method_not_found} = hello_simple_client:call(?HOST, "nonamemethod", [<<"test">>]),
    {error, invalid_params} = hello_simple_client:call(?HOST, "append", [1]),
    {error, 30000} = hello_simple_client:call(?HOST, "return_error", [30000]).

call_http_error(_Config) ->
    {error, {http, _Reason}} = hello_simple_client:call(?UNKNOWN_HOST, "foo", []).

notification(_Config) ->
    ok = hello_simple_client:notification(?HOST, "echo", [<<"test">>]).

notification_http_error(_Config) ->
    {error, {http, _Reason}} = hello_simple_client:notification(?UNKNOWN_HOST, "foo", []).

call_np(_Config) ->
    {ok, <<"cdab">>} = hello_simple_client:call_np(?HOST, "append", [{str2, <<"ab">>}, {str1, <<"cd">>}]).

call_np_method_not_found(_Config) ->
    {error, method_not_found} = hello_simple_client:call_np(?HOST, "nonamemethod", [{str2, <<"ab">>}, {str1, <<"cd">>}]).

call_np_http_error(_Config) ->
    {error, {http, _Reason}} = hello_simple_client:call_np(?UNKNOWN_HOST, "foo", [{str2, <<"ab">>}, {str1, <<"cd">>}]).

% ---------------------------------------------------------------------
% -- common_test callbacks
all() ->
    [call, call_errors, call_http_error,
     notification, notification_http_error,
     call_np, call_np_method_not_found, call_np_http_error].

init_per_suite(Config) ->
    hello:start(),
    hello:bind_stateless(?HOST, hello_stateless_server_example),
    Config.

end_per_suite(_Config) ->
	application:stop(hello).
