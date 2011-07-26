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

% @doc Behaviour for stateless RPC servers.
%   A module implementing this behaviour should always include the
%   hello header file.
%
%   ```
%       -include_lib("hello/include/hello.hrl").
%   '''
%
%   == The Callbacks in Detail ==
%
%   === method_info() -> [#rpc_method{}] ===
%   The ``method_info/0'' callback is used to determine which RPC methods the server
%   provides. It is called on every request. The return value is a list of ``#rpc_method{}''
%   records that contain information about each method.
%
%   ```
%      -record(rpc_method, {
%          name              :: atom(),
%          params_as = list  :: 'list' | 'proplist',
%          description = ""  :: string()
%      }).
%   '''
%
%   === param_info(MethodName :: atom()) -> [#rpc_param{}] ===
%   The ``param_info/1'' callback should have a clause for every method name returned from ``method_info/0''.
%   Its return value is a list of records that describe the parameters of the method requested.
%
%   ```
%      -record(rpc_param, {
%          name               :: atom(),
%          type = any         :: hello:param_type(),
%          optional = false   :: boolean(),
%          default            :: term(),
%          description = ""   :: string()
%      }).
%   '''
%
%   The parameter list is used by hello to validate the arguments of an RPC call.
%   As evident in the record definition, parameters can be optional. When there is no
%   argument value for an optional parameter, the default value given in the record is used.
%   Please note that the default value does not need to be of a JSON-compatible data type, but
%   can be any Erlang term.
%
%   The type field of the record is used to validate the argument value.
%   The following types are supported:
%
%    <table border="1">
%      <thead><tr><td><b>Type</b></td><td><b>Matches</b></td></tr></thead>
%      <tbody>
%        <tr><td>any</td><td>all JSON values</td></tr>
%        <tr><td>boolean</td><td>JSON booleans</td></tr>
%        <tr><td>string</td><td>JSON strings</td></tr>
%        <tr><td>{enum, [atom()]}</td><td>a limited choice of JSON strings (converted to atom)</td></tr>
%        <tr><td>number</td><td>all JSON numbers</td></tr>
%        <tr><td>integer</td><td>JSON numbers that are integers</td></tr>
%        <tr><td>float</td><td>JSON numbers that are floats</td></tr>
%        <tr><td>array</td><td>JSON arrays</td></tr>
%        <tr><td>list</td><td>JSON arrays</td></tr>
%        <tr><td>object</td><td>JSON objects</td></tr>
%      </tbody>
%    </table>
%
%   === handle_request(MethodName :: atom(), Params :: list() | proplist()) -> Return ===
%   ```
%   Return = {ok, hello_json:value()} | {error, Code :: integer(), Message :: string()}
%   '''
%
%   This callback contains the actual implementation of the RPC method. It is called to
%   compute the reponse to an RPC call.
%
%   The type of the argument list depends on the ``params_as'' field of the ``#rpc_method{}'' record.
%   If you declare ``list'' in the record, the arguments are passed by position, as a list.
%   Otherwise, they are passed by name, as a property list (the keys are atoms).
%
%   <br/>
% @end

-module(hello_stateless_server).
-export([behaviour_info/1]).
-export([run_request/2]).

-compile({no_auto_import, [register/1, register/2, unregister/1]}).

-include("hello.hrl").
-include("internal.hrl").

-spec behaviour_info(callbacks) -> [{atom(), integer()}].
behaviour_info(callbacks) -> [{handle_request,2}, {method_info,0}, {param_info,1}];
behaviour_info(_Other)    -> undefined.

% @private
-spec run_request(module(), hello:request() | [hello:request()]) -> hello:response() | [hello:response()].
run_request(CallbackModule, BatchReq) when is_list(BatchReq) ->
    lists:map(fun (Req) -> run_maybe_notification(CallbackModule, Req) end, BatchReq);
run_request(CallbackModule, Req) ->
    run_maybe_notification(CallbackModule, Req).

run_maybe_notification(Mod, Req) ->
    case Req#request.id of
        undefined ->
            do_single_request(Mod, Req),
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
    PName = atom_to_binary(PNameAtom, utf8),
    Value = case proplists:get_value(PName, Param) of
                Undef when (Undef =:= undefined) or (Undef =:= null) ->
                    if Info#rpc_param.optional -> Info#rpc_param.default;
                       true -> throw({invalid, ["required parameter '", PName, "' is missing"]})
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
                false -> throw({invalid, ["invalid parameter type for param '", PName, "': expected ", atom_to_list(PType)]})
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
            Choices = string:join(lists:map(fun (P) -> ["\"", atom_to_list(P), "\""] end, Enum), ", "),
            throw({invalid, ["parameter '", Param, "' must be one of: ", Choices]})
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
    Names = lists:map(fun (P) -> atom_to_binary(P#rpc_param.name, utf8) end, PInfo),
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
