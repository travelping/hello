-module(hello_client_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include("../include/hello.hrl").
-include_lib("yang/include/typespec.hrl").
-include("hello_test_example.hrl").

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

all_testgroup() ->
    [{group, http},
     {group, zmq_ipc},
     {group, zmq_tcp},
     {group, http_error},
     {group, zmq_tcp_error}].

groups() ->
    [{http, [],  all_ok_test()},
     {zmq_ipc, [],  all_ok_test()},
     {zmq_tcp, [],  all_ok_test()},
     {http_error, [], [call_http_error, notification_http_error, call_np_http_error]},
     {zmq_tcp_error, [], [call_zmq_tcp_error, notification_zmq_tcp_error, call_np_zmq_tcp_error]},
     {old_cb_info, [], all_testgroup()},
     {new_cb_info, [], all_testgroup()}].

all() ->
	[{group, old_cb_info},
	 {group, new_cb_info}].

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

init_per_group(old_cb_info, Config) ->
    Mod = hello_test_stateless_handler,
    ok = meck:new(Mod, [non_strict, no_link]),
    ok = meck:expect(Mod, method_info, 0, [#rpc_method{name = M} || M <- [echo ,append, enum_test, return_error]]),
    ok = meck:expect(Mod, param_info,
        fun
            (echo) ->
                [#rpc_param{name = text, type = string, description = "the text to be echoed"}];
            (append) ->
                [#rpc_param{name = str1, type = string, optional = true, default  = <<"">>},
                    #rpc_param{name = str2, type = string, optional = true, default  = <<"">>}];
            (enum_test) ->
                [#rpc_param{name = atom, type = {enum, [a, b, c]}, description = "the atom to be echoed, \"a\", \"b\", or \"c\""}];
            (return_error) ->
                [#rpc_param{name = code, type = integer},
                    #rpc_param{name = message, type = string, optional = true, default  = <<"">>}]
        end),
    ok = meck:expect(Mod, handle_request,
        fun
            (_Context, echo, [Str]) ->
                {ok, Str};
            (_Context, append, [Str1, Str2]) ->
                {ok, <<Str1/binary, Str2/binary>>};
            (_Context, enum_test, [Atom]) ->
                {ok, Atom};
            (_Context, return_error, [Code, Message]) ->
                {error, Code, Message}
        end),
    [{cb_module, Mod}|Config];

init_per_group(new_cb_info, Config) ->
    Mod = hello_test_stateless_ts_handler,
    ok = meck:new(Mod, [non_strict, no_link]),
    ok = meck:expect(Mod, hello_info, fun hello_test_example_typespec/0),
    ok = meck:expect(Mod, handle_request,
        fun
            (_Context, <<"echo">>, [{_,Str}]) ->
                {ok, Str};
            (_Context, <<"append">>, [{_,Str1}, {_,Str2}]) ->
                {ok, <<Str1/binary, Str2/binary>>};
            (_Context, <<"enum_test">>, [{_,Atom}]) ->
                {ok, Atom};
            (_Context, <<"return_error">>, [{<<"code">>, Code}, {<<"message">>, Message}]) ->
                {error, Code, Message}
        end),
    [{cb_module, Mod}|Config];

init_per_group(zmq_tcp_error, _Config) ->
	{skip, "libzmq name resolving is broken"};

init_per_group(GroupName, Config) ->
    hello:start(),
	{URI, StartServer, ClientOptions} = group_config(GroupName),
	CbModule = proplists:get_value(cb_module, Config),
	ct:pal("CB Module: ~p~n", [CbModule]),
	ct:pal("URI: ~s, (~p)~n", [URI, ClientOptions]),
	%%TODO: apply server method restriction
	case StartServer of
		false ->
            ok;
		true ->
            ok = hello:bind_stateless(URI, CbModule)
	end,
	ct:sleep(500),
	{ok, Clnt} = hello_client:start(URI, ClientOptions),
	ct:pal("Clnt: ~w~n", [Clnt]),
	ct:comment({URI, ClientOptions}),
	[{client,Clnt}|Config].

end_per_group(Group, Config) when Group == old_cb_info; Group == new_cb_info ->
    Mod = proplists:get_value(cb_module, Config),
    meck:unload(Mod),
    ok;

end_per_group(_GroupName, Config) ->
    Clnt = proplists:get_value(client, Config),
    hello_client:stop(Clnt),
    ok = application:stop(hello),
    ok.

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.
