-module(tpjrpc_example_service).
-behaviour(tp_json_rpc_service).

-export([register_yourself/0]).
-export([handle_request/3, param_info/1, method_info/0]).
-include_lib("tp_json_rpc/include/tp_json_rpc.hrl").

register_yourself() -> tp_json_rpc_service:register(example, ?MODULE).

method_info() ->
    [#rpc_method{name        = echo,
                 description = "return the given string"},
     #rpc_method{name        = append,
                 description = "append the given strings"},
     #rpc_method{name        = enum_test,
                 description = "test tp_json_rpc's support for enums"}].

param_info(echo) ->
    [#rpc_param{name = text,
                type = string,
                description = "the text to be echoed"}];
param_info(append) ->
    [#rpc_param{name = str1,
                type = string,
                optional = true,
                default  = <<"">>},
     #rpc_param{name = str2,
                type = string,
                optional = true,
                default  = <<"">>}];
param_info(enum_test) ->
    [#rpc_param{name = atom,
                type = {enum, [a, b, c]},
                description = "the atom to be echoed, \"a\", \"b\", or \"c\""}].

handle_request(_Req, echo, [Str]) ->
    {ok, Str};
handle_request(_Req, append, [Str1, Str2]) ->
    {ok, <<Str1/binary, Str2/binary>>};
handle_request(_Req, enum_test, [Atom]) ->
    {ok, Atom}.
