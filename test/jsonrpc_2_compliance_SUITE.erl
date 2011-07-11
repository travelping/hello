-module(jsonrpc_2_compliance_SUITE).
-behaviour(hello_stateless_server).
-export([method_info/0, param_info/1, handle_request/2]).
-compile(export_all).

-include("ct.hrl").
-include("../include/hello.hrl").

-define(REQ_ID, 1).

% ---------------------------------------------------------------------
% -- test cases
error_codes(_Config) ->
	ErrorCode = fun (Code, Req) -> Code = field(request(Req), "error.code") end,

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
    Req1 = request({"subtract", [2,1]}),
    1    = field(Req1, "result"),

    % by-name
    Req2 = request({"subtract", {[{"subtrahend", 2}, {"minuend", 1}]}}),
    1    = field(Req2, "result"),

    % by-name reversed order
    Req3 = request({"subtract", {[{"minuend", 1}, {"subtrahend", 2}]}}),
    1    = field(Req3, "result").

response_fields(_Config) ->
    % success case
    Req1 = {Props1} = request({"subtract", [2,1]}),
    1         = field(Req1, "result"),
    <<"2.0">> = field(Req1, "jsonrpc"),
    ?REQ_ID   = field(Req1, "id"),
    false     = proplists:is_defined("error", Props1), % error may not be included

    % error case
    Req2 = {Props2} = request({"subtract", [1]}),
    false     = proplists:is_defined("result", Props2), % result may not be included
    <<"2.0">> = field(Req2, "jsonrpc"),
    ?REQ_ID   = field(Req2, "id"),
    -32602    = field(Req2, "error.code"),

    % error case where request isn't read
    Req3 = {Props3} = request("{aa"),
    <<"2.0">> = field(Req3, "jsonrpc"),
    null      = proplists:get_value("id", Props3),
    -32700    = field(Req3, "error.code").

notification(_Config) ->
    % leaving id off is treated as notification
    {no_json, <<"">>} = request("{\"jsonrpc\":\"2.0\", \"method\": \"subtract\", \"params\": [2, 1]}"),

    % although it's use is discouraged, null is a valid id (in jsonrpc 2.0)
    {Res} = request("{\"id\": null, \"jsonrpc\":\"2.0\", \"method\": \"subtract\", \"params\": [2, 1]}"),
    null = proplists:get_value("id", Res).

batch_calls(_Config) ->
    % success cases
    [Resp1] = request("[{\"jsonrpc\":\"2.0\", \"id\": 344, \"method\":\"subtract\", \"params\": [2,1]}]"),
    1       = field(Resp1, "result"),
    344     = field(Resp1, "id"),

    Resp2   = request("[{\"jsonrpc\":\"2.0\", \"id\": 300, \"method\":\"subtract\", \"params\": [2,1]}"
                      ",{\"jsonrpc\":\"2.0\", \"id\": 400, \"method\":\"subtract\", \"params\": [80,3]}]"),
    2       = length(Resp2),

    % with notifications
    [Resp3] = request("[{\"jsonrpc\":\"2.0\", \"method\":\"subtract\", \"params\": [2,1]}"
                      ",{\"jsonrpc\":\"2.0\", \"id\": 400, \"method\":\"subtract\", \"params\": [80,3]}]"),
    400     = field(Resp3, "id"),
    77      = field(Resp3, "result"),

    % only notifications
    {no_json, <<"">>} = request("[{\"jsonrpc\":\"2.0\", \"method\":\"subtract\", \"params\": [2,1]}"
                                ",{\"jsonrpc\":\"2.0\", \"method\":\"subtract\", \"params\": [80,3]}]"),

    % rpc call with invalid batch (but not empty)
    [Resp4] = request("[1]"),
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

handle_request(subtract, [Subtrahend, Minuend]) ->
    {ok, Subtrahend - Minuend}.

% ---------------------------------------------------------------------
% -- common_test callbacks
all() -> [error_codes, param_structures, response_fields, notification, batch_calls].

% ---------------------------------------------------------------------
% -- utilities
request({Method, Params}) ->
    Req = {[{jsonrpc, <<"2.0">>}, {id, ?REQ_ID}, {method, list_to_binary(Method)}, {params, Params}]},
    request(hello_json:encode(Req));
request(Request) when is_list(Request) ->
    request(list_to_binary(Request));
request(Request) ->
    RespJSON = hello:run_stateless_binary_request(?MODULE, Request),
    case hello_json:decode(RespJSON) of
        {ok, RespObj, _Rest}  -> RespObj;
        {error, syntax_error} -> {no_json, RespJSON}
    end.

field(Object, Field) ->
	Flist = re:split(Field, "\\.", [{return, list}]),
	lists:foldl(fun (Name, {CurProps}) ->
		    	    proplists:get_value(Name, CurProps)
			    end, Object, Flist).
