-module(hello_log_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("../include/jsonrpc_internal.hrl").

-define(URL, "http://127.0.0.1:6000").

% ---------------------------------------------------------------------
% -- test cases
log_request(_Config) ->
    %{ok, ExUriUrl, []} = ex_uri:decode(?URL),
    %hello_jsonrpc_log:request(?MODULE, self(), ExUriUrl, #jsonrpc_request{}, #jsonrpc_response{}),
    %Logs = lager_common_test_backend:get_logs(),
    %2 = length(Logs),
    ok.

log_bad_request(_Config) ->
    %BadRequest1 = #jsonrpc_request{method = <<"bad_method">>, params = [<<"bad_param">>], reqid = 123},
    %BadRequest2 = <<"i_am_some_malformed_unparsable_request">>,
    %BadResponse = #jsonrpc_response{reqid = 123, error = #jsonrpc_error{code = 12345, message = <<"TestMessage">>}},
    %{ok, ExUriUrl, []} = ex_uri:decode(?URL),
    %hello_jsonrpc_log:bad_request(?MODULE, self(), ExUriUrl, BadRequest1, BadResponse),
    %hello_jsonrpc_log:bad_request(?MODULE, self(), ExUriUrl, BadRequest2, BadResponse),
    %Logs = lager_common_test_backend:get_logs(),
    %3 = length(Logs),
    ok.

% ---------------------------------------------------------------------
% -- common_test callbacks
all() ->
    [%log_request, 
     %log_bad_request
    ].

init_per_testcase(_, Config) ->
    lager:start(),
    lager_common_test_backend:bounce(info),
    lager:set_loglevel(lager_common_test_backend, info),
    Config.

end_per_testcase(_, _Config) ->
    application:stop(lager).
