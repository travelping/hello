% Copyright (c) 2011 by Travelping GmbH <info@travelping.com>

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
-module(hello_binding).

-export([outgoing_message/2]). %, send/1, close/1]).
-export([do_binding/7, undo_binding/2, bindings/0]).
-export([send_pong/1]).
-export([behaviour_info/1]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("internal.hrl").

behaviour_info(callbacks) ->
    [{listener_specification, 2},
     {send_response, 3},
     {close, 1},
     {listener_termination, 1}
     ];
behaviour_info(_) ->
    undefined.

incoming_message(Context, ExUriURL, Binary) ->
    spawn(?MODULE, handle_incoming_message, [Context, ExUriURL, Binary]).

outgoing_message(Context = #context{connection_pid = ConnectionPid}, Response) ->
    ConnectionPid ! {?INCOMING_MSG, Response}.

proceed_incoming_message(ProtocolInfo, [], Context, _ExUriURL, Responses) ->
    Response = hello_proto:handle_response(ProtocolInfo, Responses),
    case Response of
        undefined ->
            close(Context);
        _NotUndef ->
            send(Response),
            close(Context)
    end;
proceed_incoming_message(ProtocolInfo, [{Status, Namespace, Request} | ExtractedRequests], Context, ExUriURL, Responses) ->
    case hello_registry:lookup_binding(Namespace, ExUriURL) of
        {ok, Binding} ->
            case hello_registry:lookup_handler(Binding) of
                {ok, Handler} ->
                    Handler ! {?INCOMING_MSG, Request},
                    case Status of
                        reply ->
                            receive
                                {?INCOMING_MSG, Response} ->
                                    proceed_incoming_message(ProtocolInfo, ExtractedRequests, Context, ExUriURL, Responses ++ [Response]);
                                _ ->
                                    throw("Got invalid response")
                            end;
                        noreply ->
                            proceed_incoming_message(ProtocolInfo, ExtractedRequests, Context, ExUriURL, Responses)
                    end;
                {error, not_found} ->
                    ErrorResponse = hello_proto:error_response(internal_error, <<"handler_is_dead">>, undefined, Request),
                    proceed_incoming_message(ProtocolInfo, ExtractedRequests, Context, ExUriURL, Responses ++ [ErrorResponse])
            end;
        {error, not_found} ->
            ErrorResponse = hello_proto:error_response(binding_not_found, undefined, undefined, Request),
            proceed_incoming_message(ProtocolInfo, ExtractedRequests, Context, ExUriURL, Responses ++ [ErrorResponse])
    end.

send(Response) -> todo.

close(Context) -> todo.

%send(Response = #response{proto_mod = Protocol, context = Context}) ->
%    {ok, BinResp} = hello_proto:encode(Response),
%    EncInfo = hello_proto:encoding_info(Protocol),
%    #context{transport = TransportMod} = Context,
%    TransportMod:send_response(Context, EncInfo, BinResp).
%
%close(Context = #context{transport = TransportMod}) ->
%    TransportMod:close(Context).

%% ----------------------------------------------------------------------------------------------------
%% -- Create a new binding and start the handler and listener (if necessary)
do_binding(ExUriURL, TransportOpts, CallbackMod, HandlerMod, HandlerOpts, Protocol, ProtocolOpts) ->
    case hello_listener:start(ExUriURL, TransportOpts) of
        {ok, _ListenerRef} ->
            NewBinding = new_binding(ExUriURL, CallbackMod, HandlerMod, HandlerOpts, Protocol, ProtocolOpts),
            case hello_registry:lookup_binding(NewBinding#binding.namespace, ExUriURL) of
                {ok, _OldBinding} ->
                    {error, callback_already_defined};
                {error, not_found} ->
                    case start_handler(NewBinding) of
                        {ok, NewHandler} ->
                            hello_registry:register_binding({NewBinding#binding.namespace, ExUriURL}, NewBinding),
                            {ok, NewHandler};
                        {error, already_started} ->
                            {error, handler_already_started}
                    end
            end;
        {error, Reason} ->
            {error, {transport, Reason}}
    end.

undo_binding(Url, CallbackMod) ->
    case (catch ex_uri:decode(Url)) of
        {ok, ExUriURL, _} ->
            case hello_validate:get_namespace(CallbackMod) of
                {ok, Namespace} ->
                    hello_registry:unregister_binding({Namespace, ExUriURL});
                {error, Reason} ->
                    {error, Reason}
            end;
        Other ->
            {error, {badurl, Other}}
    end.

new_binding(ExUriURL, CallbackMod, HandlerMod, HandlerOpts, Protocol, ProtocolOpts) ->
    case hello_validate:get_namespace(CallbackMod) of
        {ok, Namespace} ->
            #binding{
                namespace = Namespace,
                callback = CallbackMod,
                handler_type = HandlerMod,
                handler_args = HandlerOpts,
                protocol = Protocol,
                protocol_args = ProtocolOpts,
                url = ExUriURL
            };
        {error, Reason} ->
            erlang:error(Reason)
    end.

bindings() ->
    hello_registry:bindings().
%% ----------------------------------------------------------------------------------------------------
%% -- Start and register a handler for a callback module.
start_handler(Binding) ->
    case hello_handler_supervisor:start_handler(Binding) of
        {ok, Handler} ->
            hello_registry:register_handler(Binding, Handler),
            {ok, Handler};
        {error, _ } ->
            {error, already_started}
    end.

stop_handler(Binding) ->
    case hello_registry:lookup_handler(Binding) of
        {ok, _Handler} ->
            hello_handler_supervisor:stop_handler(Binding);
        _ ->
            ok
    end.

handle_internal(Context, ?PING) ->
    erlang:spawn(?MODULE, send_pong, [Context]),
    ok;
handle_internal(Context, Binary) ->
    handle_internal(Context, binary_to_atom(Binary, latin1)).

send_pong(Context) ->
    BinaryPong = atom_to_binary(?PONG, latin1),
    EncInfo = hello_proto:encoding_info(hello_proto),
    #context{transport = TransportMod} = Context,
    TransportMod:send_response(Context, EncInfo, BinaryPong),
    close(Context).
