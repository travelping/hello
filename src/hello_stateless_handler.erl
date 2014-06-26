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
%   === handle_request(Context, MethodName :: atom(), Params :: list() | proplist()) -> Return ===
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
%   The opaque ``Context'' parameter can be used to retrieve information about the RPC request.
%   See {@link transport_param/3} for more information.
%   <br/>
% @end

-module(hello_stateless_handler).
-export([behaviour_info/1, transport_param/2, transport_param/3]).
-export([handler/4, run_binary_request/4]).
-export_type([context/0]).

-include("hello.hrl").
-include("internal.hrl").

-define(MESSAGE_TIMEOUT, 10000). % 10sec

-record(stateless_context, {transport_params = [] :: hello:transport_params()}).
-opaque context() :: #stateless_context{}.

%% ----------------------------------------------------------------------------------------------------
%% -- API for callback module
-spec behaviour_info(callbacks) -> [{atom(), integer()}].
behaviour_info(callbacks) -> [{handle_request,3}, {hello_info,0}];
behaviour_info(_Other)    -> undefined.

%% @equiv transport_param(Key, Context, undefined)
-spec transport_param(context(), atom()) -> any().
transport_param(Key, Context) ->
    transport_param(Key, Context, undefined).

%% @doc Get the value of a transport parameter from the Context.
%%   The list of supported transport parameters will vary between transport implementations.<br/>
%%   If you rely on transport parameters, your RPC handler <b>will no longer be transport-agnostic</b>.
%%   Please take this into consideration when designing your system.
-spec transport_param(atom(), context(), any()) -> any().
transport_param(Key, #stateless_context{transport_params = Params}, Default) ->
    proplists:get_value(Key, Params, Default).

%% ----------------------------------------------------------------------------------------------------
%% -- Implementation
%% @private
%% @doc stateless handler process function for hello_binding
-spec handler(#binding{}, hello_binding:peer(), pid(), hello:transport_params()) -> any().
handler(#binding{protocol = Protocol, log_url = Endpoint, callbacks = Callbacks}, Peer, Transport, TransportParams) ->
    receive
        {?INCOMING_MSG_MSG, Message} ->
            case run_binary_request(Protocol, Callbacks, TransportParams, Message) of
                {ok, Mod, Request, Response} ->
                    hello_request_log:request(Mod, self(), Endpoint, Request, Response),
                    BinResp = hello_proto:encode(Response),
                    Transport ! {hello_msg, self(), Peer, BinResp};
                {proto_reply, Response} ->
                    hello_request_log:bad_request(undefined, self(), Endpoint, Message, Response),
                    BinResp = hello_proto:encode(Response),
                    Transport ! {hello_msg, self(), Peer, BinResp};
                ignore ->
                    ignore
            end,
            Transport ! {hello_closed, self(), Peer}
    after
        ?MESSAGE_TIMEOUT ->
            Transport ! {hello_closed, self(), Peer}
    end.

%% @private
-spec run_binary_request(module(), hello:callback(), hello:transport_params(), binary()) ->
    {ok, hello_proto:request(), hello_proto:response(), module()} | {error, hello_proto:response()}.
run_binary_request(Protocol, Callbacks, TransportParams, BinRequest) ->
    Context = #stateless_context{transport_params = TransportParams},
    case hello_proto:decode(Protocol, BinRequest) of
        Req = #request{} ->
            {Mod, Response} = do_single_request_get_mod(Callbacks, Context, Req),
            {ok, Mod, Req, Response};
        Req = #batch_request{requests = GoodReqs} ->
            {Mods, Resps} = lists:unzip([do_single_request_get_mod(Callbacks, Context, R) || R <- GoodReqs]),
            {ok, get_batch_mod(Mods), Req, hello_proto:batch_response(Req, Resps)};
        ProtoReply = {proto_reply, _Resp} ->
            ProtoReply;
        #response{} ->
            ignore;
        #batch_response{} ->
            ignore
    end.

do_single_request_get_mod(Callbacks, Context, Req) ->
    case do_single_request(Callbacks, Context, Req) of
        {_, _} = Res -> Res;
        Response -> {undefined, Response}
    end.

do_single_request(Callbacks, Context, Req = #request{namespaces=Namespaces}) when not is_atom(Callbacks) ->
    Cb = lists:foldl(
           fun
               (Ns, undefined) ->
                   case dict:find(Ns, Callbacks) of
                       error ->
                           undefined;
                       {ok, [Callback]} ->
                           Callback
                   end;
               (_, Callback) ->
                   Callback
           end, undefined, Namespaces),
    case Cb of
        undefined ->
            hello_proto:error_response(Req, method_not_found);
        _ ->
            #callback{mod=Mod} = Cb,
            do_single_request(Mod, Context, Req)
    end;
 do_single_request(Mod, Context, Req = #request{method=Method0}) ->
    try
        Method1 = binary_to_atom(Method0, utf8),
        case hello_validate:request(Mod, Req) of
            {error, ErrorMsg} ->
                {Mod, ErrorMsg};
            {ok, Method0, Validated} when is_list(Validated) ->
                {Mod, run_callback_module(Req, Mod, Context, Method0, Validated)};
            {ok, Method1, Validated} when is_list(Validated) ->
                {Mod, run_callback_module(Req, Mod, Context, Method1, Validated)}
        end
    catch
        Type:Error ->
            Report = io_lib:format("Error (~p) thrown by RPC handler '~p' while executing the method \"~s\":~n"
                "Parameters: ~p~nReason: ~p~nTrace:~n~p~n",
                [Type, Mod, Req#request.method, Req#request.params, Error, erlang:get_stacktrace()]),
            error_logger:error_report(Report),
            {Mod, hello_proto:error_response(Req, server_error)}
    end.

run_callback_module(Req, Mod, Context, Method, ValidatedParams) ->
    case Mod:handle_request(Context, Method, ValidatedParams) of
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

get_batch_mod([]) -> undefined;
get_batch_mod([Mod | Other]) -> get_batch_mod(Mod, Other).

get_batch_mod(Mod, []) -> Mod;
get_batch_mod(Mod, [Mod | Other]) -> get_batch_mod(Mod, Other);
%% TODO: fix me, if batch use different modules, the request will not be logged
get_batch_mod(_Mod, [_AnotherMod | _Other]) -> undefined.
