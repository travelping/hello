% Copyright (c) 2010-2012 by Travelping GmbH <info@travelping.com>

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

-module(hello_stateless_handler).
-export([behaviour_info/1]).
-export([run_binary_request/3]).

-compile({no_auto_import, [register/1, register/2, unregister/1]}).

-include("hello.hrl").
-include("internal.hrl").

-spec behaviour_info(callbacks) -> [{atom(), integer()}].
behaviour_info(callbacks) -> [{handle_request,2}, {method_info,0}, {param_info,1}];
behaviour_info(_Other)    -> undefined.

%% @private
-spec run_binary_request(module(), module(), binary()) -> binary().
run_binary_request(Protocol, CallbackModule, Message) ->
    case hello_proto:decode(Protocol, Message) of
        Req = #request{} ->
            hello_proto:encode(do_single_request(CallbackModule, Req));
        Req = #batch_request{requests = GoodReqs} ->
            hello_proto:encode(hello_proto:batch_response(Req, [do_single_request(CallbackModule, R) || R <- GoodReqs]));
        _ ->
            hello_proto:encode(hello_proto:error_response(Protocol, invalid_request))
    end.

do_single_request(Mod, Req = #request{method = MethodName}) ->
    try
        case hello_validate:find_method(Mod:method_info(), MethodName) of
            undefined ->
                hello_proto:error_response(Req, method_not_found);
            Method ->
                case hello_validate:request_params(Method, Mod:param_info(Method#rpc_method.name), Req) of
                    {ok, Validated} -> run_callback_module(Req, Mod, Method, Validated);
                    {error, Msg}    -> hello_proto:error_response(Req, invalid_params, Msg)
                end
        end
    catch
        Type:Error ->
            Report = io_lib:format("Error (~p) thrown by RPC handler '~p' while executing the method \"~s\":~n"
                                   "Parameters: ~p~nReason: ~p~nTrace:~n~p~n",
                                   [Type, Mod, Req#request.method, Req#request.params, Error, erlang:get_stacktrace()]),
            error_logger:error_report(Report),
            hello_proto:error_response(Req, server_error)
    end.

run_callback_module(Req, Mod, Method, ValidatedParams) ->
    case Mod:handle_request(Method#rpc_method.name, ValidatedParams) of
        {ok, Result} ->
            hello_proto:success_response(Req, Result);
        {error, Message} ->
            hello_proto:error_response(Req, 0, Message);
        {error, Code, Message} ->
            hello_proto:error_response(Req, Code, Message);
        {error, Code, Message, Data} ->
            hello_proto:error_response(Req, Code, Message, Data);
        Ret ->
            error({bad_return_value, Ret})
    end.
