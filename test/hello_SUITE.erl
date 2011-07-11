-module(hello_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("../include/hello.hrl").

% ---------------------------------------------------------------------
% -- test cases
bind_stateless_http_url(_Config) ->
    ok = hello:bind_stateless("http://localhost:5671/test", hello_stateless_server_example).

bind_stateless_http_url_errors(_Config) ->
    URL = "http://localhost:5672/test",
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

% ---------------------------------------------------------------------
% -- common_test callbacks
all() ->
    [bind_stateless_http_url, bind_stateless_http_url_errors,
     bind_stateless_zmq_url, bind_stateless_zmq_url_errors].

init_per_suite(Config) ->
    hello:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(hello).
