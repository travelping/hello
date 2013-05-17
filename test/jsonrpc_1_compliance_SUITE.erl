-module(jsonrpc_1_compliance_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include("../include/hello2.hrl").
-include_lib("yang/include/typespec.hrl").
-include("hello2_test_jsonrpc_compliance.hrl").

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
% -- common_test callbacks
all() ->
    [{group, old_cb_info},
     {group, new_cb_info}].

groups() ->
    [{all_tests, [], [param_structures, response_fields, notification]},
     {old_cb_info, [], [{group, all_tests}]},
     {new_cb_info, [], [{group, all_tests}]}].

% ---------------------------------------------------------------------
% -- utilities
request({Method, Params}) ->
    Req = {[{id, ?REQ_ID}, {method, list_to_binary(Method)}, {params, Params}]},
    request(hello2_json:encode(Req));
request(Request) when is_list(Request) ->
    request(list_to_binary(Request));
request(Request) ->
    RespJSON = hello2:run_stateless_binary_request(hello2_test_jsonrpc_1_compliance_handler, Request, []),
    case hello2_json:decode(RespJSON) of
        {ok, RespObj, _Rest}  -> RespObj;
        {error, syntax_error} -> {no_json, RespJSON}
    end.

field(Object, Field) ->
	Flist = re:split(Field, "\\.", [{return, binary}]),
	lists:foldl(fun (Name, {CurProps}) ->
                    proplists:get_value(Name, CurProps)
			    end, Object, Flist).

init_per_group(old_cb_info, Config) ->
    Mod = hello2_test_jsonrpc_1_compliance_handler,
    ok = meck:new(Mod, [non_strict, no_link]),
    ok = meck:expect(Mod, method_info, 0, [#rpc_method{name = subtract}]),
    ok = meck:expect(Mod, param_info,
		     fun(subtract) ->
			     [#rpc_param{name = subtrahend, type = number},
			      #rpc_param{name = minuend, type = number}]
		     end),
    ok = meck:expect(Mod, handle_request,
		     fun(_Context, subtract, [Subtrahend, Minuend]) ->
			     {ok, Subtrahend - Minuend}
		     end),
    [{cb_module, Mod}|Config];

init_per_group(new_cb_info, Config) ->
    Mod = hello2_test_jsonrpc_1_compliance_handler,
    ok = meck:new(Mod, [non_strict, no_link]),
    ok = meck:expect(Mod, hello2_info, fun hello2_test_jsonrpc_compliance_typespec/0),
    ok = meck:expect(Mod, handle_request,
		     fun(_Context, <<"subtract">>, [{_, Subtrahend}, {_, Minuend}]) ->
			     {ok, Subtrahend - Minuend}
		     end),
    [{cb_module, Mod}|Config];

init_per_group(_, Config) ->
    Config.

end_per_group(Group, Config) when Group == old_cb_info; Group == new_cb_info ->
    Mod = proplists:get_value(cb_module, Config),
    meck:unload(Mod),
    ok;
end_per_group(_, _) ->
    ok.
