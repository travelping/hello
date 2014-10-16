-module(hello_log_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("../include/hello.hrl").
-include("../include/internal.hrl").

% ---------------------------------------------------------------------
% -- test cases
log_request(_Config) ->
    hello_request_log:request(?MODULE, self(), <<"TestEndPoint">>, #request{}, #response{}),
    hello_request_log:request(?MODULE, self(), <<"TestEndPoint">>, #request{reqid = 123}, #error{reqid = 123, message = "TestMessage"}),

    Logs = lager_common_test_backend:get_logs(),
    ok.

log_batch_request(_Config) ->
    hello_request_log:request(?MODULE, self(), <<"TestEndPoint">>, #batch_request{requests = [#request{}, #request{}]}, #batch_response{responses = [#response{}, ignore]}),

    Logs = lager_common_test_backend:get_logs(),
    2 = length(Logs),
    ok.

log_bad_request(_Config) ->
    Chars = [0,1,2,3,4,31,32,$",$\\,126,127,128,255,256,257,1024],
    TestChars = [ [integer_to_list(X), ":", X, ";"] || X <- Chars],
    Test = unicode:characters_to_binary(["This is a bad request; ", TestChars]),
    hello_request_log:bad_request(?MODULE, self(), <<"TestBadEndPoint">>, Test, #response{}),

    Logs = lager_common_test_backend:get_logs(),
    2 = length(Logs),
    ok.

% ---------------------------------------------------------------------
% -- common_test callbacks
all() ->
    [log_request, log_batch_request, log_bad_request].

init_per_testcase(_, Config) ->
    lager:start(),
    lager_common_test_backend:bounce(info),
    lager:set_loglevel(lager_common_test_backend, info),
    Config.

end_per_testcase(_, _Config) ->
    application:stop(lager).
