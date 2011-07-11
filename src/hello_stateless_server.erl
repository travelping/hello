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

%% @doc
%%    Behaviour for stateless RPC servers. 
%% @end

-module(hello_stateless_server).
-export([behaviour_info/1]).
-export([run_request/2]).

-compile({no_auto_import, [register/1, register/2, unregister/1]}).

-include("hello.hrl").
-include("internal.hrl").

-spec behaviour_info(callbacks) -> [{atom(), integer()}]. 
behaviour_info(callbacks) -> [{handle_request,2}, {method_info,0}, {param_info,1}];
behaviour_info(_Other)    -> undefined.

-spec run_request(module(), hello:request() | [hello:request()]) -> hello:response() | [hello:response()].
run_request(CallbackModule, BatchReq) when is_list(BatchReq) ->
    lists:map(fun (Req) -> run_maybe_notification(CallbackModule, Req) end, BatchReq);
run_request(CallbackModule, Req) ->
    run_maybe_notification(CallbackModule, Req).

run_maybe_notification(Mod, Req) ->
    case Req#request.id of
        undefined ->
            spawn(fun () -> do_single_request(Mod, Req) end),
            empty_response;
        _ID ->
            do_single_request(Mod, Req)
    end.

do_single_request(Mod, Req = #request{method = MethodName, params = Params}) ->
    case find_method(Mod, MethodName) of
        undefined -> hello_proto:std_error(Req, method_not_found);
        Method    ->
            case validate_params(Mod, Method, Params) of
                {ok, Validated} -> run_callback_module(Req, Mod, Method, Validated);
                {error, Msg}    -> hello_proto:std_error(Req, {invalid_params, Msg})
            end
    end.

run_callback_module(Req, Mod, Method, ValidatedParams) ->
    try Mod:handle_request(Method#rpc_method.name, ValidatedParams) of
        {ok, Result} ->
            hello_proto:response(Req, Result);
        {error, Message} ->
            hello_proto:error_response(Req, 0, Message);
        {error, Code, Message} ->
            hello_proto:error_response(Req, Code, Message);
        _ ->
            hello_proto:std_error(Req, server_error)
    catch
         Type:Error ->
            error_logger:error_msg("Error (~p) thrown by a JSON-RPC handler function while executing the method ~s~n"
                                   "Parameters: ~p~nReason: ~p~nTrace:~n~p~n",
                                   [Type, Req#request.method, Req#request.params, Error, erlang:get_stacktrace()]),
            hello_proto:std_error(Req, server_error)
    end.

find_method(Mod, MethodName) when is_binary(MethodName) ->
    find_method(Mod, binary_to_list(MethodName));
find_method(Mod, MethodName) when is_list(MethodName) ->
    case catch list_to_existing_atom(MethodName) of
        {'EXIT', {badarg, _TR}} -> undefined;
        Atom                    -> find_method(Mod, Atom)
    end;
find_method(Mod, MethodName) when is_atom(MethodName) ->
    case lists:keyfind(MethodName, #rpc_method.name, Mod:method_info()) of
        false  -> undefined;
        Method -> Method
    end.

validate_params(Mod, #rpc_method{name = Method, params_as = WantParamEncoding}, ParamsIn) ->
    PInfo = Mod:param_info(Method),
    try
        Params = params_to_proplist(PInfo, ParamsIn),
        Validated = lists:map(fun (OneParamInfo) -> validate_field(OneParamInfo, Params) end, PInfo),
        case WantParamEncoding of
            proplist -> {ok, Validated};
            list     -> {ok, lists:map(fun ({_K, V}) -> V end, Validated)}
        end
    catch
        throw:{invalid, Msg} -> {error, Msg}
    end.

validate_field(Info = #rpc_param{name = PNameAtom}, Param) ->
    PName = atom_to_list(PNameAtom),
    Value = case proplists:get_value(PName, Param) of
                Undef when (Undef =:= undefined) or (Undef =:= null) ->
                    if Info#rpc_param.optional -> Info#rpc_param.default;
                       true -> throw({invalid, "required parameter '" ++ PName ++ "' is missing"})
                    end;
                GivenValue ->
                    validate_type(PName, Info, GivenValue)
            end,
    {PNameAtom, Value}.

validate_type(PName, #rpc_param{type = PType}, GivenValue) ->
    case PType of
        {enum, Elems} ->
            atom_from_enum(PName, Elems, GivenValue);
        _T ->
            case has_type(PType, GivenValue) of
                true -> GivenValue;
                false -> throw({invalid, "invalid parameter type for param '" ++ PName
                                          ++ "': expected " ++ atom_to_list(PType)})
            end
    end.

atom_from_enum(Param, Enum, Input) ->
    try
        A = erlang:binary_to_existing_atom(Input, utf8),
        case lists:member(A, Enum) of
            true -> A;
            false -> erlang:error(badarg)
        end
    catch
        error:badarg ->
            Choices = string:join(lists:map(fun (P) -> "\"" ++ atom_to_list(P) ++ "\"" end, Enum), ", "),
            throw({invalid, "parameter '" ++ Param ++ "' must be one of: " ++ Choices})
    end.

has_type(boolean, Val) when (Val == true) or (Val == false) -> true;
has_type(object, {_}) -> true;
has_type(integer, Val) when is_integer(Val) -> true;
has_type(float, Val) when is_float(Val) -> true;
has_type(number, Val) when is_number(Val) -> true;
has_type(string, Val) when is_binary(Val) -> true;
has_type(list, Val) when is_list(Val) -> true;
has_type(array, Val) when is_list(Val) -> true;
has_type(any, _Val) -> true;
has_type(_T, _Val) -> false.

params_to_proplist(_PInfo, {Props}) -> Props;
params_to_proplist(PInfo,  Params) when is_list(Params) ->
    Names = lists:map(fun (P) -> atom_to_list(P#rpc_param.name) end, PInfo),
    {Proplist, TooMany} = zip(Names, Params, {[], false}),
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
