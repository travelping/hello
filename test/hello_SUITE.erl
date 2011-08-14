-module(hello_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("../include/hello.hrl").

% ---------------------------------------------------------------------
% -- test cases
bind_stateless_http_url_ip(_Config) ->
    Self = self(),
    meck:expect(hello_stateless_http_server, start_listener,
                fun ({127,0,0,1}, 5671)  -> {ok, Self};
                    (_, _)               -> {error, bad_args}
                end),

    ok  = hello:bind_stateless("http://127.0.0.1:5671/test", hello_stateless_server_example),

    %% check that start_listener was actually called, once
    [{ok, Self}] = [Result || {{_Mod, start_listener, _Args}, Result} <- meck:history(hello_stateless_http_server)].

bind_stateless_http_same_listener_ip(_Config) ->
    Self = self(),
    meck:expect(hello_stateless_http_server, start_listener,
                fun ({127,0,0,1}, 5672)  -> {ok, Self};
                    (_, _)               -> {error, bad_args}
                end),

    ok = hello:bind_stateless("http://127.0.0.1:5672/test1", mod_test1),
    ok = hello:bind_stateless("http://127.0.0.1:5672/test2", mod_test2),

    mod_test1 = hello_stateless_http_server:lookup_service(<<"127.0.0.1">>, 5672, [<<"test1">>]),
    mod_test2 = hello_stateless_http_server:lookup_service(<<"127.0.0.1">>, 5672, [<<"test2">>]),

    %% check that start_listener was actually called, once
    [{ok, Self}] = [Result || {{_Mod, start_listener, _Args}, Result} <- meck:history(hello_stateless_http_server)].

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

bind_stateless_cross_protocol_checking(_Config) ->
    ok = hello:bind_stateless("http://localhost:6008", test_1),
    {error, occupied} = hello:bind_stateless("zmq-tcp://127.0.0.1:6008", test_2),

    ok = hello:bind_stateless("zmq-tcp://*:6009", test_1),
    {error, occupied} = hello:bind_stateless("http://127.0.0.1:6009", test_2).

bindings(_Config) ->
    [{_StatusIPC, hello_status}] = OrigBindings = hello:bindings(),

    IPCPath = filename:absname("bindings_test.ipc"),

    Bindings = [{"http://127.0.0.1:6003/test_1", test_1},
                {"http://127.0.0.1:6003/test_2", test_2},
                {"zmq-ipc://" ++ IPCPath, test_3},
                {"zmq-tcp://127.0.0.1:6004", test_4}],

    lists:foreach(fun ({URL, Module}) ->
                          io:format("~p~n", [URL]),
                          ok = hello:bind_stateless(URL, Module)
                  end, Bindings),

    Bindings = hello:bindings() -- OrigBindings.

% ---------------------------------------------------------------------
% -- common_test callbacks
all() ->
    [bindings,
     bind_stateless_http_url_ip, bind_stateless_http_url_errors,
     bind_stateless_http_same_listener_ip,
     bind_stateless_zmq_url, bind_stateless_zmq_url_errors,
     bind_stateless_cross_protocol_checking].

init_per_suite(Config) ->
    hello:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(hello).

init_per_testcase(_Case, Config) ->
    meck:new(hello_stateless_http_server, [passthrough]),
    Config.

end_per_testcase(_Case, _Config) ->
    meck:unload(hello_stateless_http_server).
