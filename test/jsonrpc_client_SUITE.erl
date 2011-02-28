%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(jsonrpc_client_SUITE).

-compile(export_all).

-include("ct.hrl").
-define(HOST, "http://localhost:5671/rpc/example").

% ---------------------------------------------------------------------
% -- test cases
call(_Config) ->
    {ok,<<"abcdef">>} = tp_json_rpc:call(?HOST, "append", [<<"abc">>,<<"def">>]).

call_invalid_method(_Config) ->
    {error, method_not_found} = tp_json_rpc:call(?HOST, "nonamemethod", [<<"test">>]).

call_invalid_params(_Config) ->
    {error, invalid_params} = tp_json_rpc:call(?HOST, "append", [1]).

call_error_code(_Config) ->
    {error, 30000} = tp_json_rpc:call(?HOST, "return_error", [30000]).

notification(_Config) ->
    ok = tp_json_rpc:notification(?HOST, "echo", [<<"test">>]).

% ---------------------------------------------------------------------
% -- common_test callbacks
all() -> [notification, call, call_invalid_method, call_invalid_params, call_error_code].

init_per_suite(Config) ->
	application:start(inets),
	application:start(tp_json_rpc),
    tpjrpc_example_service:register_yourself(),
    Config.

end_per_suite(_Config) ->
	application:stop(tp_json_rpc),
	application:stop(inets).
