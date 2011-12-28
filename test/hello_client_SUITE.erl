-module(hello_client_SUITE).
-compile(export_all).

-include("ct.hrl").
-define(UNKNOWN_HOST, "http://undefined.undefined:8888").

% ---------------------------------------------------------------------
% -- test cases
call(Config) ->
	Clnt = proplists:get_value(client, Config),
    {ok,<<"abcdef">>} = hello_client:call(Clnt, "append", [<<"abc">>,<<"def">>]).


call_errors(Config) ->
	Clnt = proplists:get_value(client, Config),
    {error, method_not_found} = hello_client:call(Clnt, "nonamemethod", [<<"test">>]),
    {error, invalid_params} = hello_client:call(Clnt, "append", [1]),
    {error, {30000, <<"test error message">>}} = hello_client:call(Clnt, "return_error", [30000, <<"test error message">>]).

notification(Config) ->
	Clnt = proplists:get_value(client, Config),
    ok = hello_client:notification(Clnt, "echo", [<<"test">>]).

call_np(Config) ->
	Clnt = proplists:get_value(client, Config),
    {ok, <<"cdab">>} = hello_client:call_np(Clnt, "append", [{str2, <<"ab">>}, {str1, <<"cd">>}]).

call_np_method_not_found(Config) ->
	Clnt = proplists:get_value(client, Config),
    {error, method_not_found} = hello_client:call_np(Clnt, "nonamemethod", [{str2, <<"ab">>}, {str1, <<"cd">>}]).

call_http_error(Config) ->
	Clnt = proplists:get_value(client, Config),
    {error, {http, _Reason}} = hello_client:call(Clnt, "foo", []).

notification_http_error(Config) ->
	Clnt = proplists:get_value(client, Config),
	R = hello_client:notification(Clnt, "foo", []),
	ct:pal("R: ~p~n", [R]),
    {error, {http, _Reason}} = R.


call_np_http_error(Config) ->
	Clnt = proplists:get_value(client, Config),
    {error, {http, _Reason}} = hello_client:call_np(Clnt, "foo", [{str2, <<"ab">>}, {str1, <<"cd">>}]).

call_zmq_tcp_error(Config) ->
	Clnt = proplists:get_value(client, Config),
    {error, {zmq_tcp, _Reason}} = hello_client:call(Clnt, "foo", []).

notification_zmq_tcp_error(Config) ->
	Clnt = proplists:get_value(client, Config),
    {error, {zmq_tcp, _Reason}} = hello_client:notification(Clnt, "foo", []).

call_np_zmq_tcp_error(Config) ->
	Clnt = proplists:get_value(client, Config),
    {error, {zmq_tcp, _Reason}} = hello_client:call_np(Clnt, "foo", [{str2, <<"ab">>}, {str1, <<"cd">>}]).

% ---------------------------------------------------------------------
% -- common_test callbacks
all_ok_test() ->
    [call, call_errors, notification,
     call_np, call_np_method_not_found].

groups() ->
	[{http_put, [],  all_ok_test()}, {http_post, [],  all_ok_test()},
	 {zmq_ipc_req, [],  all_ok_test()}, {zmq_ipc_dealer, [],  all_ok_test()},
	 {zmq_tcp_req, [],  all_ok_test()}, {zmq_tcp_dealer, [],  all_ok_test()},
	 {http_error, [], [call_http_error, notification_http_error, call_np_http_error]},
	 {zmq_tcp_error, [], [call_zmq_tcp_error, notification_zmq_tcp_error, call_np_zmq_tcp_error]}].

all() ->
	[{group, http_put}, {group, http_post},
	 {group, zmq_ipc_req}, {group, zmq_ipc_dealer},
	 {group, zmq_tcp_req}, {group, zmq_tcp_dealer},
	 {group, http_error}, {group, zmq_tcp_error}].

group_config(http_post) ->
	{"http://127.0.0.1:8001", post, post};
group_config(http_put) ->
	{"http://127.0.0.1:8002", put, put};
group_config(zmq_ipc_req) ->
	{"zmq-ipc://simple_client_suite.ipc", router, req};
group_config(zmq_ipc_dealer) ->
	%% don't restart....
	{"zmq-ipc://simple_client_suite.ipc", none, dealer};
group_config(zmq_tcp_req) ->
	{"zmq-tcp://127.0.0.1:5555", router, req};
group_config(zmq_tcp_dealer) ->
	{"zmq-tcp://127.0.0.1:5556", router, dealer};

group_config(http_error) ->
	{"http://undefined.undefined:8888", none, post};
group_config(zmq_tcp_error) ->
	{"zmq-tcp://undefined.undefined:5555", none, req}.

init_per_group(zmq_tcp_error, Config) ->
	{skip, "libzmq name resolving is broken"};

init_per_group(GroupName, Config) ->
	{URI, SrvMethod, ClntMethod} = group_config(GroupName),
	ct:pal("URI: ~s, (~s/~s)~n", [URI, SrvMethod, ClntMethod]),
	%%TODO: apply server method restriction
	case SrvMethod of
		none -> ok;
		_    -> ok = hello:bind_stateless(URI, hello_stateless_handler_example)
	end,
	ct:sleep(500),
	{ok, Clnt} = hello_client:start(URI, ClntMethod, []),
	ct:pal("Clnt: ~w~n", [Clnt]),
	ct:comment({URI, ClntMethod}),
	[{client,Clnt}|Config].

end_per_group(_GroupName, Config) ->
	Clnt = proplists:get_value(client, Config),
	hello_client:stop(Clnt),
	proplists:delete(client, Config).

init_per_suite(Config) ->
    hello:start(),
    Config.

end_per_suite(_Config) ->
	application:stop(hello).
