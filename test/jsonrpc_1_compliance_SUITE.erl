-module(jsonrpc_1_compliance_SUITE).

-behaviour(hello_stateless_server).
-export([method_info/0, param_info/1, handle_request/2]).
-compile(export_all).

-include("ct.hrl").
-include("../include/hello.hrl").

-define(REQ_ID, 1).

% ---------------------------------------------------------------------
% -- test cases
param_structures(_Config) ->
    % by-position
    Req1 = request({"subtract", [2,1]}),
    1    = field(Req1, "result").

response_fields(_Config) ->
    % success case
    Req1 = request({"subtract", [2,1]}),
    1         = field(Req1, "result"),
    ?REQ_ID   = field(Req1, "id"),
    null      = field(Req1, "error"),

    % error case
    Req2 = request({"subtract", []}),
    null      = field(Req2, "result"),
    ?REQ_ID   = field(Req2, "id"),
    {_}  = field(Req2, "error").

notification(_Config) ->
    % leaving id off is treated as an invalid request
    % since we handle errors jsonrpc-2.0 style, this can be checked
    Res    = request("{\"method\": \"subtract\", \"params\": [2, 1]}"),
    -32600 = field(Res, "error.code"),

    % null giving null for the id indicates a notification
    {no_json, <<"">>} = request("{\"id\": null, \"method\": \"subtract\", \"params\": [2, 1]}").

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
all() -> [param_structures, response_fields, notification].

% ---------------------------------------------------------------------
% -- utilities
request({Method, Params}) ->
    Req = {[{id, ?REQ_ID}, {method, list_to_binary(Method)}, {params, Params}]},
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
