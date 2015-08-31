% Copyright (c) 2012 by Travelping GmbH <info@travelping.com>

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
-module(hello_proto).

-export([init_client/2]).
-export([build_request/3]).
-export([encode/3, decode/5, signature/2]).
-export([handle_incoming_message/7]).

% for test
-export([handle_internal/2]). 

-include("hello.hrl").


-callback init_client(protocol_opts()) ->
    {ok, State :: term()} | {error, Reason :: term()}.

-callback build_request(request(), [proplists:property()], State:: term()) ->
    {ok, request(), NewState :: term()}.

-callback encode([request() | response()] | request() | response(), protocol_opts()) ->
    {ok, jsx:json_text()}.

-callback decode(jsx:json_text(), protocol_opts(), request | response) ->
    {ok, response()} | {ok, request()} | {error, #error{}} | {ok, [{ok, response()} | {ok, request()}]}.

-callback signature(protocol_opts()) -> binary().

%% ----------------------------------------------------------------------------------------------------
%% -- Request/Response handling
init_client(ProtocolMod, ProtocolOpts) ->
    ProtocolMod:init_client(ProtocolOpts).

build_request(BatchRequest, ProtocolMod, ProtocolState) when is_list(BatchRequest) ->
    {Requests, NewProtocolState} = lists:mapfoldl(fun(SingleRequest, AccState) ->
                                                          {ok, BuildRequest, NewAccState} = build_request(SingleRequest, ProtocolMod, AccState),
                                                          {BuildRequest, NewAccState}
                                                  end, ProtocolState, BatchRequest),
    {ok, Requests, NewProtocolState};
build_request({Method, Args, Options}, ProtocolMod, ProtocolState) ->
    Request = #request{method = Method, args = Args},
    ProtocolMod:build_request(Request, Options, ProtocolState).

%% ----------------------------------------------------------------------------------------------------
%% -- Request/Response handling

handle_incoming_message(Context1, ProtocolMod, ProtocolOpts, Router, ExUriURL, Signature, Binary) ->
    Context = Context1#context{connection_pid = self()},
    case decode(ProtocolMod, ProtocolOpts, Signature, Binary, request) of
        {ok, Requests} ->
            Length = case Requests of Requests when is_list(Requests) -> length(Requests); _ -> 1 end,
            hello_metrics:ok_request(Length),
            Result = proceed_incoming_message(Requests, Context, ProtocolMod, ProtocolOpts, Router, ExUriURL),
            may_be_encode(ProtocolMod, ProtocolOpts, Result);
        {error, ignore} ->
            %log(ProtocolMod, Binary, undefined, undefined, ExUriURL),
            hello_metrics:error_request(),
            ignore;
        {error, Response} ->
            hello_metrics:error_request(),
            may_be_encode(ProtocolMod, ProtocolOpts, Response);
        {internal, Message} ->
            hello_metrics:internal_request(),
            ?MODULE:handle_internal(Context, Message) % for test
    end.

proceed_incoming_message(Requests, Context, ProtocolMod, ProtocolOpts, Router, ExUriURL) when is_list(Requests) ->
    [proceed_incoming_message(Request, Context, ProtocolMod, ProtocolOpts, Router, ExUriURL) || {_, Request} <- Requests];
proceed_incoming_message(Request = #request{type = Type, proto_data = Info}, Context, _ProtocolMod, _ProtocolOpts, Router, ExUriURL) ->
    case Router:route(Context, Request, ExUriURL) of
        {ok, ServiceName, Identifier} ->
            hello_service:call(ServiceName, Identifier, Request#request{context = Context}),
            may_be_wait(Type, Request, Context);
        {ok, ServiceName, Identifier, NewRequest} ->
            hello_service:call(ServiceName, Identifier, NewRequest#request{context = Context}),
            may_be_wait(Type, NewRequest, Context);
        {error, Error} = _ ->
            #response{proto_data = Info,
                      response = #error{code = Error, message = "the required method is not registered"}}
    end.

may_be_wait(_, #request{proto_data = Info}, _Context) ->
    Answer = hello_service:await(),
    hello_metrics:response(),
    #response{proto_data = Info, response = proto_answer(Answer)};
may_be_wait(async, _Request, _Context) ->
    ignore.

may_be_encode(ProtocolMod, ProtocolOpts, BatchAnswer) when is_list(BatchAnswer) ->
    ShelledResults = [Result || Result <- BatchAnswer, Result =/= ignore],
    case ShelledResults of
        [] -> ignore;
        _  -> encode(ProtocolMod, ProtocolOpts, ShelledResults)
    end;
may_be_encode(_ProtocolMod, _ProtocolOpts, ignore) ->
    ignore;
may_be_encode(ProtocolMod, ProtocolOpts, Answer) ->
    encode(ProtocolMod, ProtocolOpts, Answer).

handle_internal(_Context, ?PING) -> {ok, ?PONG}.

%% ----------------------------------------------------------------------------------------------------
%% -- Encoding/Decoding
encode(Mod, Opts, Request) -> Mod:encode(Request, Opts).
decode(_Mod, _Opts, ?INTERNAL_SIGNATURE, Message, _Type) -> {internal, Message};
decode(_Mod, _Opts, {<<>>, ?INTERNAL_SIGNATURE} = _Signature, Message, _Type) -> {internal, Message};
decode(Mod, Opts, Signature, Message, Type) when is_atom(Mod) ->
    case signature(Mod, Opts) of
        Signature -> Mod:decode(Message, Opts, Type);
        {<<>>, Signature} -> Mod:decode(Message, Opts, Type);
        _ ->
            case jsx:is_json(Message) of %% backward compatibility
                true -> hello_proto_jsonrpc:decode(Message, Opts, Type);
                false -> {error, bad_signature}
            end
    end.
signature(Mod, Opts) when is_atom(Mod) -> Mod:signature(Opts).

proto_answer({error, timeout}) -> #error{code = server_error, message = "doesn't answer within a timeout"};
proto_answer({error, {Code, Message, ProtoData}}) -> #error{code = Code, message = Message, proto_data = ProtoData};
proto_answer({ok, Response}) -> Response.
