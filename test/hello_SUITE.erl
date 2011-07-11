-module(hello_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("../include/hello.hrl").

% ---------------------------------------------------------------------
% -- test cases
bind_http_url(_Config) ->
    {ok, _Pid} = hello:bind_stateless("http://localhost:5671/test", hello_example_service).

bind_http_url_errors(_Config) ->
    {ok, _Pid} = hello:bind_stateless("http://localhost:5672/test", hello_example_service),

    %% binding the same module returns already_started
    {error, already_started} = hello:bind_stateless("http://localhost:5672/test", hello_example_service),

    %% binding a different one returns occupied
    {error, occupied} = hello:bind_stateless("http://localhost:5672/test", ?MODULE).

% ---------------------------------------------------------------------
% -- common_test callbacks
all() -> [bind_http_url, bind_http_url_errors].

init_per_suite(Config) ->
    hello:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(hello).
