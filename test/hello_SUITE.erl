-module(hello_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("../include/hello.hrl").

% ---------------------------------------------------------------------
% -- test cases
bind_stateless_http_url_ip(_Config) ->
    meck:expect(cowboy, start_listener, fun (_, _, cowboy_tcp_transport, [{port, 5671}, {ip, {127,0,0,1}}], cowboy_http_protocol, _) ->
                                                {ok, self()};
                                            (_, _, _, _, _, _) ->
                                                {error, bad_args}
                                        end),

    ok  = hello:bind_stateless("http://127.0.0.1:5671/test", hello_stateless_server_example),

    [_] = meck:history(cowboy).

bind_stateless_http_same_listener_ip(_Config) ->
    meck:expect(cowboy, start_listener, fun (_, _, cowboy_tcp_transport, [{port, 5672}, {ip, {127,0,0,1}}], cowboy_http_protocol, _) ->
                                                {ok, self()};
                                            (_, _, _, _, _, _) ->
                                                {error, bad_args}
                                        end),

    ok = hello:bind_stateless("http://127.0.0.1:5672/test1", mod_test1),
    ok = hello:bind_stateless("http://127.0.0.1:5672/test2", mod_test2),

    mod_test1 = hello_stateless_httpd:lookup_service(<<"127.0.0.1">>, 5672, [<<"test1">>]),
    mod_test2 = hello_stateless_httpd:lookup_service(<<"127.0.0.1">>, 5672, [<<"test2">>]),

    %% start_listener should only be called once
    [_] = meck:history(cowboy).

bind_stateless_http_url_errors(_Config) ->
    URL = "http://127.0.0.1:5673/test",
    ok = hello:bind_stateless(URL, hello_stateless_server_example),

    %% binding the same module returns already_started
    {error, already_started} = hello:bind_stateless(URL, hello_stateless_server_example),

    %% binding a different one returns occupied
    {error, occupied} = hello:bind_stateless(URL, ?MODULE).

bind_stateless_zmq_url(_Config) ->
    ok = hello:bind_stateless("zmq-tcp://127.0.0.1:6001", hello_stateless_server_example).

bind_stateless_zmq_url_errors(_Config) ->
    URL = "zmq-tcp://127.0.0.1:6002",
    ok = hello:bind_stateless(URL, hello_stateless_server_example),

    %% binding the same module returns already_started
    {error, already_started} = hello:bind_stateless(URL, hello_stateless_server_example),

    %% binding a different one returns occupied
    {error, occupied} = hello:bind_stateless(URL, ?MODULE).

bindings(_Config) ->
    [] = hello:bindings(),

    IPCPath = filename:absname("bindings_test.ipc"),

    Bindings = [{"http://127.0.0.1:6003/test_1", test_1},
                {"http://127.0.0.1:6003/test_2", test_2},
                {"zmq-ipc://" ++ IPCPath, test_3},
                {"zmq-tcp://127.0.0.1:6004", test_4}],

    lists:foreach(fun ({URL, Module}) ->
                          io:format("~p~n", [URL]),
                          ok = hello:bind_stateless(URL, Module)
                  end, Bindings),

    Bindings = hello:bindings().

% ---------------------------------------------------------------------
% -- common_test callbacks
all() ->
    [bindings,
     bind_stateless_http_url_ip, bind_stateless_http_url_errors,
     bind_stateless_http_same_listener_ip,
     bind_stateless_zmq_url, bind_stateless_zmq_url_errors].

init_per_suite(Config) ->
    hello:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(hello).

init_per_testcase(_Case, Config) ->
    meck:new(cowboy, [passthrough]),
    Config.

end_per_testcase(_Case, _Config) ->
    meck:unload(cowboy).
