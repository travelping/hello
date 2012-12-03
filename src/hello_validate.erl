% Copyright (c) 2010-2011 by Travelping GmbH <info@travelping.com>

% Permission is hereby granted, free of charge, to any person obtaining a
% copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
% DEALINGS IN THE SOFTWARE.

% @private
-module(hello_validate).
-export([find_method/2, request_params/3, request_params/4]).
-export([validate_params/3, validate_params/4]).
-export_type([json_type/0, param_type/0]).

-include("hello.hrl").
-include("internal.hrl").

-type json_type()  :: 'boolean' | 'object' | 'integer' | 'float' | 'number' | 'string' | 'list' | 'array' | 'any' | 'iso_date'.
-type param_type() :: json_type() | {enum, [atom()]}.

%% --------------------------------------------------------------------------------
%% -- API functions
-spec find_method(list(#rpc_method{}), atom() | string() | binary()) -> #rpc_method{} | undefined.

find_method(MList, MethodName) ->
    ct:pal("MList: ~p", [MList]),
    case lists:member(MethodName, MList) of
	true -> #rpc_method{name = MethodName};
	false -> undefined
    end.

-spec request_params(#rpc_method{}, module(), #request{})
    -> {ok, [hello_json:value()] | [{atom(), hello_json:value()}]} | {error, iodata()}.

request_params(Method, CallbackModule, Request) ->
    Mod = {CallbackModule},
    Fields = get_param_info(Mod, Method#rpc_method.name),
    Params = params_to_proplist(Fields, Request#request.params),
    {ok, Params}.

-spec request_params(#rpc_method{}, module(), any(), #request{})
    -> {ok, [hello_json:value()] | [{atom(), hello_json:value()}]} | {error, iodata()}.

request_params(Method, CallbackModule, ModuleStat, Request) ->
    Mod = {CallbackModule, ModuleStat},
    Fields = get_param_info(Mod, Method#rpc_method.name),
    Params = params_to_proplist(Fields, Request#request.params),
    {ok, Params}.

validate_params(TypeSpec, RPC, Params) ->
    validate_params(TypeSpec, RPC, -1, Params).

validate_params(TypeSpec, RPC, Depth, Params) ->
    try
	yang_json_validate:validate(TypeSpec, {rpc, RPC, input}, Depth, Params)
    catch
	throw:{error, Error} ->
	    Msg = io_lib:format("Error: ~p", [Error]),
	    {error, invalid_params, Msg};
	throw:{error, Error, EMsg} ->
	    Msg = io_lib:format("Error: ~p, EMsg: ~p", [Error, EMsg]),
	    {error, invalid_params, Msg}
    end.

%% --------------------------------------------------------------------------------
%% -- internal functions
get_param_info({CallbackModule}, Name) ->
    CallbackModule:param_info(Name);
get_param_info({CallbackModule, ModuleStat}, Name) ->
    CallbackModule:param_info(Name, ModuleStat).

params_to_proplist(_PInfo, {Props}) -> Props;
params_to_proplist(Fields,  Params) when is_list(Params) ->
    {Proplist, TooMany} = zip(Fields, Params, {[], false}),
    TooMany andalso throw({invalid, "superfluous parameters"}),
    lists:reverse(Proplist).

zip([], [], Result) ->
    Result;
zip([], _2, {Result, _TM}) ->
    zip([], [], {Result, true});
zip(_1, [], Result) ->
    zip([], [], Result);
zip([H1|R1], [H2|R2], {Result, TooMany}) ->
    zip(R1, R2, {[{H1, H2}|Result], TooMany}).
