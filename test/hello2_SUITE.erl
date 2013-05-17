-module(hello2_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include("../include/hello2.hrl").

% ---------------------------------------------------------------------
% -- test cases
bind_stateless_http_url_errors(_Config) ->
    URL = "http://127.0.0.1:5673/test",
    ok = hello2:bind_stateless(URL, test_1),

    %% binding the same module returns already_started
    {error, already_started} = hello2:bind_stateless(URL, test_1),

    %% binding a different one returns occupied
    {error, occupied} = hello2:bind_stateless(URL, test_2).

bind_stateless_zmq_url(_Config) ->
    ok = hello2:bind_stateless("zmq-tcp://127.0.0.1:6001", test_1).

bind_stateless_zmq_url_errors(_Config) ->
    URL = "zmq-tcp://127.0.0.1:6002",
    ok = hello2:bind_stateless(URL, test_1),

    %% binding the same module returns already_started
    {error, already_started} = hello2:bind_stateless(URL, test_1),

    %% binding a different one returns occupied
    {error, occupied} = hello2:bind_stateless(URL, test_2).

bind_stateless_cross_protocol_checking(_Config) ->
    ok = hello2:bind_stateless("http://localhost:6008", test_1),
    {error, occupied} = hello2:bind_stateless("zmq-tcp://127.0.0.1:6008", test_2),

    ok = hello2:bind_stateless("zmq-tcp://*:6009", test_1),
    {error, occupied} = hello2:bind_stateless("http://127.0.0.1:6009", test_2).

bindings(_Config) ->
    OrigBindings = lists:sort(hello2:bindings()),

    IPCPath = filename:absname("/tmp/bindings_test.ipc"),


    Bindings0 = [{"http://127.0.0.1:6003/test_1", test_1},
                {"http://127.0.0.1:6003/test_2", test_2},
                {"http://127.0.0.1:6004", test_3, [{exclusive, false}]},
                {"http://127.0.0.1:6004", test_4, [{exclusive, false}]},
                {"zmq-ipc://" ++ IPCPath, test_5},
                {"zmq-tcp://127.0.0.1:6005", test_6}],

    Bindings1 = lists:map(fun
            ({Url, Module}) ->
                ct:log("Binding ~p to ~p", [Module, Url]),
                ok = hello2:bind_stateless(Url, Module),
                {Url, Module};
            ({Url, Module, Args}) ->
                ct:log("Binding ~p to ~p with ~p", [Module, Url, Args]),
                ok = hello2:bind_stateless(Url, Module, Args),
                {Url, Module}
        end, Bindings0),

    Bindings1 = lists:sort(hello2:bindings() -- OrigBindings).

% ---------------------------------------------------------------------
% -- common_test callbacks
all() ->
    [bindings,
     bind_stateless_http_url_errors,
     bind_stateless_zmq_url, bind_stateless_zmq_url_errors,
     bind_stateless_cross_protocol_checking].

init_per_suite(Config) ->
    hello2:start(),
    Mods = [test_1, test_2, test_3, test_4, test_5, test_6],
    setup_cb_mocks(Mods),
    [{cb_modules, Mods} | Config].

end_per_suite(Config) ->
    teardown_cb_mocks(?config(cb_modules, Config)),
    application:stop(hello).

setup_cb_mocks([]) -> ok;
setup_cb_mocks([Mod | Rest]) ->
    ok = meck:new(Mod, [non_strict, no_link]),
    ok = meck:expect(Mod, hello2_info, 
        fun() ->
            {Mod, atom_to_binary(Mod, utf8), []}
        end),
    setup_cb_mocks(Rest).
 
teardown_cb_mocks(Mods) ->
    [ok = meck:unload(Mod) || Mod <- Mods].
