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
-module(hello_validate_yang).

-export([request/3, validate_params/3, validate_params/4, get_namespace/1]).

-export_type([json_type/0, param_type/0]).

-include_lib("yang/include/typespec.hrl").

-include("internal.hrl").

-type json_type()  :: 'boolean' | 'object' | 'integer' | 'float' | 'number' | 'string' | 'list' | 'array' | 'any' | 'iso_date'.
-type param_type() :: json_type() | {enum, [atom()]}.

%% --------------------------------------------------------------------------------
%% -- API functions
request(Mod, Method, Params) ->
    case erlang:function_exported(Mod, typespec, 0) of
        false ->
            {error, typespec_not_loaded};
        true ->
            ModSpec = Mod:typespec(),
            try
                Fields = yang_typespec:rpc_params(Method, ModSpec),
                case is_proplist(Params) of
                    true ->
                        Params1 = sort_params(Params, Fields);
                    false ->
                        Params1 = Params
                end,
                case validate_params(ModSpec, Method, params_to_proplist(Fields, Params1)) of
                    {error, Code} ->
                        {error, {Code, undefined, undefined}};
                    {error, Code, Msg} ->
                        {error, {Code, Msg, undefined}};
                    {error, Code, Msg, Data} ->
                        {error, {Code, Msg, Data}};
                    ParamsValidated ->
                        ParamsValidated %{ok, Method, Params}
                end
            catch
                error:{badarg, _} ->
                    {error, {method_not_found, undefined, undefined}};
                throw:{error, unknown_type} ->
                    {error, {invalid_params, <<"unknown type">>, undefined}};
                throw:_ ->
                    {error, {invalid_params, undefined, undefined}}
            end
    end.

get_namespace(Mod) ->
    case erlang:function_exported(Mod, typespec, 0) of
        true ->
            {_, Namespace, _} = Mod:typespec(),
            {ok, Namespace};
        false ->
            {error, no_typespec_loaded}
    end.

%% --------------------------------------------------------------------------------
%% -- internal functions
validate_params(TypeSpec, Method, Params) ->
    validate_params(TypeSpec, Method, -1, Params).

validate_params(TypeSpec, Method, Depth, Params) ->
    try yang_json_validate:validate(TypeSpec, {rpc, Method, input}, Depth, Params) of
        Error when element(1, Error) == error ->
            Error;
        ParamsValidated when is_list(ParamsValidated) ->
            #object{opts = Opts} = yang_typespec:get_type(TypeSpec, {rpc, Method, input}),
            params_return({ok, Method, ParamsValidated, TypeSpec}, Opts)
    catch
        throw:{error, Error} ->
            Msg = io_lib:format("Error: ~p", [Error]),
            {error, invalid_params, Msg};
        throw:{error, invalid_type, {Data, _Type}} ->
            {error, invalid_params, Data};
        throw:{error, invalid_params, Msg} ->
            {error, invalid_params, Msg};
        throw:{error, Error, EMsg} ->
            Msg = io_lib:format("Error: ~p, EMsg: ~p", [Error, EMsg]),
            {error, invalid_params, Msg}
    end.

params_return({ok, Method, Params, _}, []) ->
    {ok, Method, Params};
params_return({ok, Method, Params, TypeSpec}, [{params_name_as, atom}|T]) ->
    params_return({ok, Method, [{binary_to_atom(K, utf8), V} || {K, V} <- Params], TypeSpec}, T);
params_return({ok, Method, Params, TypeSpec}, [{methods_as, atom}|T]) ->
    params_return({ok, binary_to_atom(Method, utf8), Params, TypeSpec}, T);
params_return({ok, Method, Params, {_, _, Spec} = TypeSpec}, [{params_as, list}|T]) ->
    #rpc{input = Input} = lists:keyfind(m2b(Method), #rpc.name, Spec),
    #object{fields = Fields} = Input,
    params_return({ok, Method, strip_keys(Params, Fields, []), TypeSpec}, T);
params_return(Return, [_|T]) ->
    params_return(Return, T).

m2b(Method) when is_atom(Method) -> atom_to_binary(Method, utf8);
m2b(Method) -> Method.

params_to_proplist(_PInfo, {Props}) -> Props;
params_to_proplist(Fields,  Params) when is_list(Params) ->
    {Proplist, TooMany} = zip(Fields, Params, {[], false}),
    TooMany andalso throw({error, invalid_params, "superfluous parameters"}),
    lists:reverse(Proplist).

strip_keys([{K, V} | Proplists], [#field{name = K} | Defs], Acc) ->
    strip_keys(Proplists, Defs, [V | Acc]);
strip_keys([{K, V} | Proplists], [#array{name = K} | Defs], Acc) ->
    strip_keys(Proplists, Defs, [V | Acc]);
strip_keys(Args, [#field{opts = Opts} | Defs], Acc) ->
    strip_keys(tail(Args), Defs, [proplists:get_value(default, Opts) | Acc]);
strip_keys(Args, [#array{opts = Opts} | Defs], Acc) ->
    strip_keys(tail(Args), Defs, [proplists:get_value(default, Opts) | Acc]);
strip_keys([], [], Acc) ->
    lists:reverse(Acc).

tail([_ | V]) -> V;
tail([]) -> [].

zip([], [], Result) ->
    Result;
zip([], _2, {Result, _TM}) ->
    zip([], [], {Result, true});
zip(_1, [], Result) ->
    zip([], [], Result);
zip([H1|R1], [H2|R2], {Result, TooMany}) ->
    zip(R1, R2, {[{H1, H2}|Result], TooMany}).

is_proplist([]) ->
    true;
is_proplist([{_,_} | Tail]) ->
    is_proplist(Tail);
is_proplist(_) ->
    false.

sort_params(Params, Fields) ->
    case length(Params) == length(Fields) of
        true ->
            sort_params1(Params, Fields, []);
        false ->
            throw({error, invalid_params, "superfluous parameters"})
    end.

sort_params1([], [], Akk) ->
    Akk;
sort_params1(Params, [ HeadField | TailFields ], Akk) ->
    case proplists:lookup(HeadField, Params) of
        {HeadField, Value} ->
            sort_params1(proplists:delete(HeadField, Params), TailFields, Akk ++ [Value]);
        none ->
            throw({error, invalid_params, "params do not match"})
    end.