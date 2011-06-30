%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(jsonrpc_2_compliance_SUITE).
-behaviour(hello_service).
-export([method_info/0, param_info/1, handle_request/3]).
-compile(export_all).

-include("ct.hrl").
-include("../include/hello.hrl").

-define(REQ_ID, 1).

% ---------------------------------------------------------------------
% -- test cases
error_codes(_Config) ->
	ErrorCode = fun (Code, Req) -> Code = field(request("spec_suite", Req), "error.code") end,

	% parse error
	ErrorCode(-32700, "{aa"),
	ErrorCode(-32700, "\"b"),
	ErrorCode(-32700, "[fb]"),
	ErrorCode(-32700, "{method: null, params: null}"),

	% invalid request
    ErrorCode(-32600, "\"foo\""),
    ErrorCode(-32600, "[]"),
    ErrorCode(-32600, "{\"method\": null, \"params\": [], \"id\": 1}"),
    ErrorCode(-32600, "{\"method\": \"foobar\", \"params\": 0, \"id\": 1}"),

	% method not found
    ErrorCode(-32601, {"does_not_exist", []}),

    % invalid parameters
    ErrorCode(-32602, {"subtract", [<<"a">>, <<"b">>]}), % invalid types
    ErrorCode(-32602, {"subtract", [1]}). % required parameter is missing

param_structures(_Config) ->
    % by-position
    Req1 = request("spec_suite", {"subtract", [2,1]}),
    1    = field(Req1, "result"),

    % by-name
    Req2 = request("spec_suite", {"subtract", {[{"subtrahend", 2}, {"minuend", 1}]}}),
    1    = field(Req2, "result"),

    % by-name reversed order
    Req3 = request("spec_suite", {"subtract", {[{"minuend", 1}, {"subtrahend", 2}]}}),
    1    = field(Req3, "result").

response_fields(_Config) ->
    % success case
    Req1 = {Props1} = request("spec_suite", {"subtract", [2,1]}),
    1         = field(Req1, "result"),
    <<"2.0">> = field(Req1, "jsonrpc"),
    ?REQ_ID   = field(Req1, "id"),
    false     = proplists:is_defined("error", Props1), % error may not be included

    % error case
    Req2 = {Props2} = request("spec_suite", {"subtract", [1]}),
    false     = proplists:is_defined("result", Props2), % result may not be included
    <<"2.0">> = field(Req2, "jsonrpc"),
    ?REQ_ID   = field(Req2, "id"),
    -32602    = field(Req2, "error.code"),

    % error case where request isn't read
    Req3 = {Props3} = request("spec_suite", "{aa"),
    <<"2.0">> = field(Req3, "jsonrpc"),
    null      = proplists:get_value("id", Props3),
    -32700    = field(Req3, "error.code").

notification(_Config) ->
    % leaving id off is treated as notification
    {no_json, <<"">>} = request("spec_suite", "{\"jsonrpc\":\"2.0\", \"method\": \"subtract\", \"params\": [2, 1]}"),

    % although it's use is discouraged, null is a valid id (in jsonrpc 2.0)
    {Res} = request("spec_suite", "{\"id\": null, \"jsonrpc\":\"2.0\", \"method\": \"subtract\", \"params\": [2, 1]}"),
    null = proplists:get_value("id", Res).

batch_calls(_Config) ->
    % success cases
    [Resp1] = request("spec_suite", "[{\"jsonrpc\":\"2.0\", \"id\": 344, \"method\":\"subtract\", \"params\": [2,1]}]"),
    1       = field(Resp1, "result"),
    344     = field(Resp1, "id"),

    Resp2   = request("spec_suite", "[{\"jsonrpc\":\"2.0\", \"id\": 300, \"method\":\"subtract\", \"params\": [2,1]}"
                                    ",{\"jsonrpc\":\"2.0\", \"id\": 400, \"method\":\"subtract\", \"params\": [80,3]}]"),
    2       = length(Resp2),

    % with notifications
    [Resp3] = request("spec_suite", "[{\"jsonrpc\":\"2.0\", \"method\":\"subtract\", \"params\": [2,1]}"
                                    ",{\"jsonrpc\":\"2.0\", \"id\": 400, \"method\":\"subtract\", \"params\": [80,3]}]"),
    400     = field(Resp3, "id"),
    77      = field(Resp3, "result"),

    % only notifications
    {no_json, <<"">>} = request("spec_suite", "[{\"jsonrpc\":\"2.0\", \"method\":\"subtract\", \"params\": [2,1]}"
                                              ",{\"jsonrpc\":\"2.0\", \"method\":\"subtract\", \"params\": [80,3]}]"),

    % rpc call with invalid batch (but not empty)
    [Resp4] = request("spec_suite", "[1]"),
    null    = field(Resp4, "id"),
    -32600  = field(Resp4, "error.code").

% ---------------------------------------------------------------------
% -- service callbacks
method_info() ->
    [#rpc_method{name = subtract}].

param_info(subtract) ->
    [#rpc_param{name = subtrahend,
                type = number},
     #rpc_param{name = minuend,
                type = number}].

handle_request(_Req, subtract, [Subtrahend, Minuend]) ->
    {ok, Subtrahend - Minuend}.

% ---------------------------------------------------------------------
% -- common_test callbacks
all() -> [error_codes, param_structures, response_fields, notification, batch_calls].

init_per_suite(Config) ->
	application:start(inets),
	application:start(hello),
	hello_service:register(spec_suite, ?MODULE),
	Config.

end_per_suite(_Config) ->
	hello_service:unregister(spec_suite),
	application:stop(hello),
	application:stop(inets).

% ---------------------------------------------------------------------
% -- utilities
request(Service, {Method, Params}) ->
    Req = {[{jsonrpc, <<"2.0">>}, {id, ?REQ_ID}, {method, list_to_binary(Method)}, {params, Params}]},
    request(Service, hello_json:encode(Req));
request(Service, Request) when is_list(Request) ->
    request(Service, list_to_binary(Request));
request(Service, Request) ->
    RespJSON = hello:handle_request(Service, Request),
    case hello_json:decode(RespJSON) of
        {ok, RespObj, _Rest}  -> RespObj;
        {error, syntax_error} -> {no_json, RespJSON}
    end.

field(Object, Field) ->
	Flist = re:split(Field, "\\.", [{return, list}]),
	lists:foldl(fun (Name, {CurProps}) ->
		    	    proplists:get_value(Name, CurProps)
			    end, Object, Flist).
