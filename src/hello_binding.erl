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

-export([incoming_message/4, outgoing_message/1, send/1, close/1]).
-export([do_binding/7, undo_binding/2, bindings/0,
         start_handler/1, stop_handler/1,
         start_listener/2, stop_listener/1
         ]).
-export([handle_incoming_message/4]).
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

incoming_message(Context, ExUriURL, EncInfo, Binary) ->
    spawn(?MODULE, handle_incoming_message, [Context, ExUriURL, EncInfo, Binary]).

outgoing_message(Response = #response{context = Context}) ->
    ConnectionPid = Context#context.connection_pid,
    ConnectionPid ! {?INCOMING_MSG, Response}.

handle_incoming_message(Context1, ExUriURL, EncInfo, Binary) ->
    Context = Context1#context{connection_pid = self()},
    ProtocolMod = hello_proto:encoding_protocol(EncInfo),
    case hello_proto:decode(ProtocolMod, Binary) of
        {ok, DecodedRequest} ->
            BuildRequest1 = hello_proto:build_request(DecodedRequest, ProtocolMod),
            BuildRequest = BuildRequest1#request{context = Context},
            {ProtocolInfo, ExtractedRequests} = hello_proto:extract_requests(BuildRequest),
            ct:log("test1"),
            proceed_incoming_message(ProtocolInfo, ExtractedRequests, Context, ExUriURL, []);
        {error, parse_error} ->
            ErrorResponse = hello_proto:error_response(parse_error, <<"invalid encoding">>, undefined, undefined),
            hello_proto:log(ProtocolMod, Binary, ErrorResponse, undefined, ExUriURL),
            send(ErrorResponse),
            close(Context);
        {error, ignore} ->
            hello_proto:log(ProtocolMod, Binary, undefined, undefined, ExUriURL),
            close(Context);
        {error, ProtoResponse} ->
            Response = #response{proto_response = ProtoResponse, proto_mod = ProtocolMod},
            hello_proto:log(ProtocolMod, Binary, Response, undefined, ExUriURL),
            send(Response),
            close(Context);
        {internal, Message} ->
            handle_internal(Context, Message)
    end.

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
            ct:log("test6"),
            ErrorResponse = hello_proto:error_response(binding_not_found, undefined, undefined, Request),
            proceed_incoming_message(ProtocolInfo, ExtractedRequests, Context, ExUriURL, Responses ++ [ErrorResponse])
    end.

send(Response = #response{proto_mod = Protocol, context = Context}) ->
    {ok, BinResp} = hello_proto:encode(Response),
    EncInfo = hello_proto:encoding_info(Protocol),
    #context{transport = TransportMod} = Context,
    TransportMod:send_response(Context, EncInfo, BinResp).

close(Context = #context{transport = TransportMod}) ->
    TransportMod:close(Context).

%% ----------------------------------------------------------------------------------------------------
%% -- Create a new binding and start the handler and listener (if necessary)
do_binding(ExUriURL, TransportOpts, CallbackMod, HandlerMod, HandlerOpts, Protocol, ProtocolOpts) ->
    case start_listener(ExUriURL, TransportOpts) of
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

%% --------------------------------------------------------------------------------
%% -- start and stop a listener
start_listener(ExUriURL, TransportOpts) ->
    case hello_registry:lookup_listener(ExUriURL) of
        {error, not_found} ->
            case start_listener1(ExUriURL, TransportOpts) of
                {ok, ListenerRef} ->
                    hello_registry:register_listener(ExUriURL, ListenerRef),
                    {ok, ListenerRef};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, ListenerRef} ->
            {ok, ListenerRef}
    end.

stop_listener(ExUriURL = #ex_uri{scheme = Scheme}) ->
    TransportMod = transport_module(Scheme),
    case TransportMod:listener_termination(ExUriURL) of
        child ->
            hello_listener_supervisor:stop_child(TransportMod, ExUriURL);
        _ ->
            ok
    end.

%% -------------------------------------------------------------------------------
%% -- helpers TODO e.g. for keep alive mechanism
start_listener1(ExUriURL = #ex_uri{scheme = Scheme}, TransportOpts) ->
    TransportMod = transport_module(Scheme),
    case TransportMod:listener_specification(ExUriURL, TransportOpts) of
        {make_child, ListenerChildSpec} ->
            hello_listener_supervisor:start_child(ListenerChildSpec),
            {ok, make_ref()};
        {other_supervisor, _Result} ->
            {ok, make_ref()};
        {error, Reason} ->
            erlang:error(Reason)
    end.

transport_module("zmq-tcp") -> hello_zmq_listener;
transport_module("zmq-ipc") -> hello_zmq_listener;
transport_module("http")    -> hello_http_listener;
transport_module(_Scheme)   -> error(notsup).

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
