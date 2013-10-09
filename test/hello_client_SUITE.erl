-module(hello_client_SUITE).
-compile(export_all).

-include("ct.hrl").
-define(UNKNOWN_HOST, "http://undefined.undefined:8888").

-define(equal(Expected, Actual),
    (fun (Expected@@@, Expected@@@) -> true;
         (Expected@@@, Actual@@@) ->
             ct:fail("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
		     [?FILE, ?LINE, ??Actual, Expected@@@, Actual@@@])
     end)(Expected, Actual)).

-define(match(Guard, Expr),
        ((fun () ->
                  case (Expr) of
                      Guard -> ok;
                      V -> ct:fail("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
                                   [?FILE, ?LINE, ??Expr, ??Guard, V])
                  end
          end)())).

% ---------------------------------------------------------------------
% -- test cases
call(Config) ->
    Clnt = proplists:get_value(client, Config),
    ?match({ok,<<"abcdef">>}, hello_client:call(Clnt, "append", [<<"abc">>,<<"def">>])),
    ok.
call_errors(Config) ->
    Clnt = proplists:get_value(client, Config),
    ?match({error, {method_not_found, _}}, hello_client:call(Clnt, "nonamemethod", [<<"test">>])),
    ?match({error, {invalid_params, _}}, hello_client:call(Clnt, "append", [1])),
    ?match({error, {30000, <<"test error message">>}}, hello_client:call(Clnt, "return_error", [30000, <<"test error message">>])),
    ok.

notification(Config) ->
    Clnt = proplists:get_value(client, Config),
    ?match(ok, hello_client:notification(Clnt, "echo", [<<"test">>])),
    ok.

call_np(Config) ->
    Clnt = proplists:get_value(client, Config),
    ?match({ok, <<"cdab">>}, hello_client:call_np(Clnt, "append", [{str2, <<"ab">>}, {str1, <<"cd">>}])),
    ok.

batch_call(Config) ->
    Clnt = proplists:get_value(client, Config),
    ?match([{error, {method_not_found, _}},
	    {error, {invalid_params, _}},
	    {ok, <<"abcd">>}],
	   hello_client:batch_call(Clnt, [{"nonamemethod", [<<"test">>]},
					  {'append', [1]},
					  {'append', {[{str1, <<"ab">>}, {str2, <<"cd">>}]}}])),
    ok.

call_np_method_not_found(Config) ->
    Clnt = proplists:get_value(client, Config),
    ?match({error, {method_not_found, _}}, hello_client:call_np(Clnt, "nonamemethod", [{str2, <<"ab">>}, {str1, <<"cd">>}])),
    ok.

call_http_error(Config) ->
    Clnt = proplists:get_value(client, Config),
    ?match({error, {transport, _Reason}}, hello_client:call(Clnt, "foo", [])),
    ok.

notification_http_error(Config) ->
    Clnt = proplists:get_value(client, Config),
    R = hello_client:notification(Clnt, "foo", []),
    ?match({error, {transport, _Reason}}, R),
    ok.

call_np_http_error(Config) ->
    Clnt = proplists:get_value(client, Config),
    ?match({error, {transport, _Reason}}, hello_client:call_np(Clnt, "foo", [{str2, <<"ab">>}, {str1, <<"cd">>}])),
    ok.

call_zmq_tcp_error(Config) ->
    Clnt = proplists:get_value(client, Config),
    ?match({error, {zmq_tcp, _Reason}}, hello_client:call(Clnt, "foo", [])),
    ok.

notification_zmq_tcp_error(Config) ->
    Clnt = proplists:get_value(client, Config),
    ?match({error, {zmq_tcp, _Reason}}, hello_client:notification(Clnt, "foo", [])),
    ok.

call_np_zmq_tcp_error(Config) ->
    Clnt = proplists:get_value(client, Config),
    ?match({error, {zmq_tcp, _Reason}}, hello_client:call_np(Clnt, "foo", [{str2, <<"ab">>}, {str1, <<"cd">>}])),
    ok.

% ---------------------------------------------------------------------
% -- common_test callbacks
all_ok_test() ->
    [call, call_errors, notification,
     call_np, batch_call, call_np_method_not_found].

groups() ->
	[{http, [],  all_ok_test()},
	 {zmq_ipc, [],  all_ok_test()},
	 {zmq_tcp, [],  all_ok_test()},
	 {http_error, [], [call_http_error, notification_http_error, call_np_http_error]},
	 {zmq_tcp_error, [], [call_zmq_tcp_error, notification_zmq_tcp_error, call_np_zmq_tcp_error]}].

all() ->
	[{group, http}, {group, zmq_ipc}, {group, zmq_tcp},
	 {group, http_error}, {group, zmq_tcp_error}].

group_config(http) ->
	{"http://127.0.0.1:8001", true, []};
group_config(zmq_ipc) ->
	%% don't restart....
	{"zmq-ipc://simple_client_suite.ipc", true, []};
group_config(zmq_tcp) ->
	{"zmq-tcp://127.0.0.1:5556", true, []};

group_config(http_error) ->
	{"http://undefined.undefined:8888", false, []};
group_config(zmq_tcp_error) ->
	{"zmq-tcp://undefined.undefined:5555", false, []}.

init_per_group(zmq_tcp_error, Config) ->
	{skip, "libzmq name resolving is broken"};

init_per_group(GroupName, Config) ->
	{URI, StartServer, ClientOptions} = group_config(GroupName),
	ct:pal("URI: ~s, (~p)~n", [URI, ClientOptions]),
	%%TODO: apply server method restriction
	case StartServer of
		false -> ok;
		true  -> ok = hello:bind_stateless(URI, hello_stateless_handler_example)
	end,
	ct:sleep(500),
	{ok, Clnt} = hello_client:start(URI, ClientOptions),
	ct:pal("Clnt: ~w~n", [Clnt]),
	ct:comment({URI, ClientOptions}),
	[{client,Clnt}|Config].

end_per_group(_GroupName, Config) ->
    Clnt = proplists:get_value(client, Config),
    hello_client:stop(Clnt),
    ok.

init_per_suite(Config) ->
    hello:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(hello),
    ok.
