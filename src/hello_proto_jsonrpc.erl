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

% @private
-module(hello_proto_jsonrpc).
-behaviour(hello_proto).

-export([init_client/1,
         generate_request_id/1,
         new_request/2,
         do_request/5,
         do_async_request/4,
         do_response/2,
         encoding_info/0,
         encode/1,
         decode/1,
         extract_requests/1,
         error_response/4,
         log/4
         ]).
-export([send_notification/3,
         send_notification/4
         ]).

-include("internal.hrl").
-include("jsonrpc_internal.hrl").

%% -------------------------------------------------------------------------------------
%% -- send a notification
send_notification(ReqContext, Method, Params) ->
    send_notification(ReqContext, Method, Params, ?JSONRPC_2).

send_notification(ReqContext, Method, Params, JsonRPC) ->
    Context = ReqContext#request_context.context,
    ProtoRequest = #jsonrpc_request{reqid = undefined,
                                    method = Method,
                                    params = Params,
                                    jsonrpc = JsonRPC
                                    },
    Response = #response{proto_response = ProtoRequest,
                         proto_mod = ?MODULE,
                         context = Context
                        },
    hello_binding:send(Response).

%% -------------------------------------------------------------------------------------
%% -- callbacks for the hello_proto behaviour
init_client(Proplist) ->
    NotificationSink = proplists:get_value(notification_sink, Proplist, undefined),
    JsonRPCVersion = proplists:get_value(jsonrpc, Proplist, "jsonrpc 2.0"),
    case JsonRPCVersion of
        ?JSONRPC_1 ->
            {ok, #jsonrpc_client_state{ next_reqid = 0, notification_sink = NotificationSink, jsonrpc = ?JSONRPC_1 }};
        ?JSONRPC_2 ->
            {ok, #jsonrpc_client_state{ next_reqid = 0, notification_sink = NotificationSink, jsonrpc = ?JSONRPC_2 }};
        _ ->
            {error, invalid_json_version}
    end.

generate_request_id(Batch) when is_list(Batch) ->
    UnfilteredRequestIds = [ generate_request_id(Request) || Request <- Batch ],
    FilteredRequestIds = [ ShelledId || ShelledId <- UnfilteredRequestIds, ShelledId /= ignore ],
    RequestIds = [ Id || {ok, Id} <- FilteredRequestIds ],
    case RequestIds of
        [] ->
            ignore;
        _NotEmpty ->
            {ok, RequestIds}
    end;
generate_request_id(#jsonrpc_request{reqid = undefined}) ->
    ignore;
generate_request_id(#jsonrpc_request{reqid = ReqId}) ->
    {ok, ReqId}.

new_request(BatchRequest, State) when is_list(BatchRequest) ->
    case build_batch_request(BatchRequest, State) of
        {ok, Requests, State1} ->
            {ok, Requests, State1};
        {error, Reason, State1} ->
            {error, Reason, State1}
    end;
new_request(SingleRequest, State = #jsonrpc_client_state{ next_reqid = ReqId }) ->
    Request = build_single_request(SingleRequest, State),
    {ok, Request, State#jsonrpc_client_state{ next_reqid = ReqId+1 }}.

do_request(HandlerMod, Mod, HandlerInfo, ReqContext, Request) ->
    do_single_request(HandlerMod, Mod, HandlerInfo, ReqContext, Request).

do_async_request(Request = #jsonrpc_request{reqid = ReqId1}, undefined, ReqId, Result) when ReqId == ReqId1  ->
    Response = success_response(Request, Result),
    {reply, Response};
do_async_request(_Request, _Info, _ReqId, _Result) ->
    noreply.

do_response(BatchResponse, State) when is_list(BatchResponse) ->
    AllResponses = [ do_response(Response, State) || Response <- BatchResponse ],
    Results = [ Result || {reply, _, Result, _} <- AllResponses ],
    ReqIds = [ ReqId || {reply, ReqId, _, _} <- AllResponses ],
    {reply, ReqIds, Results, State};
do_response(#jsonrpc_response{reqid = undefined, error = undefined}, PState) ->
    {noreply, PState};
do_response(#jsonrpc_response{reqid = ReqId, error = undefined, result = Result}, PState) ->
    {reply, ReqId, Result, PState};
do_response(#jsonrpc_response{reqid = ReqId, error = Error, result = _Result}, PState) ->
    #jsonrpc_error{code = Code, message = Message, data = Data} = Error,
    {reply, ReqId, {decode_error_code(Code), Message, Data}, PState};
do_response(#jsonrpc_request{reqid = undefined, method = Method, params = Params},
            PState = #jsonrpc_client_state{notification_sink = NoteSink}) ->
    case is_pid(NoteSink) of
        true ->
            NoteSink ! {notification, Method, Params};
        false when is_function(NoteSink) ->
            erlang:apply(NoteSink, [Method, Params]);
        false ->
            ignore
    end,
    {noreply, PState}.

encoding_info() ->
    atom_to_binary(?JSONRPC, latin1).

encode(Batch) when is_list(Batch)  ->
    EncodedBatch = [ encode_single(Request) || Request <- Batch ],
    {ok, jsx:encode(EncodedBatch)};
encode(Single) ->
    EncodedSingle = encode_single(Single),
    {ok, jsx:encode(EncodedSingle)}.

decode(Binary) ->
    try
        case jsx:decode(to_binary(Binary)) of
            Batch = [ Single | _ ] when is_list(Single) ->
                decode_batch(Batch);
            Single ->
                decode_single(Single)
        end
    catch
        _:_ ->
            {error, parse_error}
    end.

extract_requests(BatchRequest) when is_list(BatchRequest) ->
    {batch, [ extract_requests1(Request) || Request <- BatchRequest ]};
extract_requests(SingleRequest) ->
    {single, [extract_requests1(SingleRequest)]}.

extract_requests1(SingleRequest = #jsonrpc_request{reqid = ReqId, method = Method}) ->
    Namespace = get_namespace(Method),
    case ReqId of
        undefined ->
            {noreply, Namespace, SingleRequest};
        _NoNotification ->
            {reply, Namespace, SingleRequest}
    end.

error_response(Code, Message, Data, #jsonrpc_request{reqid = ReqId, jsonrpc = JsonRPC}) ->
    Error = build_error(Code, Message, Data),
    #jsonrpc_response{reqid = ReqId, result = undefined, error = Error, jsonrpc = JsonRPC}.

log(Request = #jsonrpc_request{}, Response = #jsonrpc_response{error = undefined}, Mod, ExUriUrl) ->
    hello_jsonrpc_log:request(Mod, self(), ExUriUrl, Request, Response);
log(BinaryRequest, undefined, Mod, ExUriUrl) when is_binary(BinaryRequest) ->
    hello_jsonrpc_log:bad_request(Mod, self(), ExUriUrl, BinaryRequest, ignore);
log(BinaryRequest, Response = #jsonrpc_response{}, Mod, ExUriUrl) when is_binary(BinaryRequest) ->
    hello_jsonrpc_log:bad_request(Mod, self(), ExUriUrl, BinaryRequest, Response);
log(Request = #jsonrpc_request{}, Response = #jsonrpc_response{}, Mod, ExUriUrl) ->
    hello_jsonrpc_log:bad_request(Mod, self(), ExUriUrl, Request, Response).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helpers
do_single_request(HandlerMod, Mod, HandlerInfo, ReqContext,
                    Request = #jsonrpc_request{reqid = ReqId, method = Method, params = Params}) ->
    ReqContext1 = ReqContext#request_context{protocol_info = ReqId},
    case hello_proto:proceed_request(HandlerMod, Mod, HandlerInfo, ReqContext1, Method, Params) of
        {reply, Result, NewHandlerInfo} ->
            case ReqId of
                undefined ->
                    {ignore, NewHandlerInfo};
                _ReqId ->
                    Response = success_response(Request, Result),
                    {reply, Response, NewHandlerInfo}
            end;
        {stop, Reason, Result, NewHandlerInfo} ->
            Response = success_response(Request, Result),
            {stop, Reason, Response, NewHandlerInfo};
        {stop, Reason, NewHandlerInfo} ->
            {stop, Reason, NewHandlerInfo};
        {noreply, NewHandlerInfo} ->
            case ReqId of
                undefined ->
                    {ignore, NewHandlerInfo};
                _ReqId ->
                    {noreply, NewHandlerInfo}
            end;
        {error, {Code, Message, Data}} ->
            ErrorResponse = error_response(Code, Message, Data, Request),
            {reply, ErrorResponse, HandlerInfo}
    end.

success_response(#jsonrpc_request{jsonrpc = JsonRPC, reqid = ReqId}, Result) ->
    #jsonrpc_response{
        jsonrpc = JsonRPC,
        reqid = ReqId,
        result = Result,
        error = undefined
    }.

build_batch_request(BatchRequest, State) ->
    build_batch_request(BatchRequest, [], State).
build_batch_request([], Reqs, State) ->
    {ok, Reqs, State};
build_batch_request([Request | TailBatch], Akk, State = #jsonrpc_client_state{next_reqid=ReqId}) ->
    NewRequest = build_single_request(Request, State),
    build_batch_request(TailBatch, Akk ++ [NewRequest], State#jsonrpc_client_state{next_reqid=ReqId+1}).

build_single_request({Method, Args, Options}, #jsonrpc_client_state{next_reqid=ReqId, jsonrpc=JsonRPC}) ->
    Notify = proplists:get_value(notification, Options, false),
    case Notify of
        false ->
            #jsonrpc_request{reqid = ReqId, params = Args, method = Method, jsonrpc = JsonRPC};
        true ->
            #jsonrpc_request{reqid = undefined, params = Args, method = Method, jsonrpc = JsonRPC}
    end.

%% ----------------------------------------------------------------------------------------------------
%% -- Encoding
%% request encoding
encode_single(#jsonrpc_request{reqid = ReqId, method = Method, params = Params, jsonrpc = ?JSONRPC_1}) ->
    [{<<"id">>, maybe_null(ReqId)}, {<<"params">>, Params}, {<<"method">>, Method}];
encode_single(#jsonrpc_request{reqid = ReqId, method = Method, params = Params, jsonrpc = ?JSONRPC_2}) ->
    [{<<"id">>, maybe_null(ReqId)}, {<<"params">>, Params}, {<<"method">>, Method}, {<<"jsonrpc">>, <<"2.0">>}];
%% response encoding
encode_single(#jsonrpc_response{reqid = ReqId, result = Result, error = Error, jsonrpc = undefined}) ->
    EncError = encode_single(Error),
    [{<<"error">>, EncError}, {<<"result">>, Result}, {<<"id">>, ReqId}];
encode_single(#jsonrpc_response{reqid = ReqId, result = Result, error = Error, jsonrpc = ?JSONRPC_1}) ->
    EncError = encode_single(Error),
    [{<<"error">>, EncError}, {<<"result">>, Result}, {<<"id">>, ReqId}];
encode_single(#jsonrpc_response{reqid = ReqId, result = Result, error = Error, jsonrpc = ?JSONRPC_2}) ->
    EncError = encode_single(Error),
    [{<<"error">>, EncError}, {<<"result">>, Result}, {<<"id">>, ReqId}, {<<"jsonrpc">>, <<"2.0">>}];
%% error encoding
encode_single(#jsonrpc_error{code = Code, message = Message, data = Data}) ->
    [{<<"data">>, maybe_null(Data)}, {<<"message">>, maybe_null(Message)}, {<<"code">>, maybe_null(Code)}];
encode_single(undefined) ->
    null.

%% ----------------------------------------------------------------------
%% -- Decoding
decode_batch(Batch) ->
    ShelledSingles = [ decode_single(Single) || Single <- Batch ],
    WithoutBadNotifications = [ ShelledSingle || ShelledSingle <- ShelledSingles, ShelledSingle /= ignore ],
    {GoodSingles, _BadSingles} = lists:partition(fun(ShelledSingle) ->
                                                    case ShelledSingle of
                                                        {ok, _Ns, _SingleReq} ->
                                                            true;
                                                        {ok, _SingleResp} ->
                                                            true;
                                                        {error, _Single} ->
                                                            false
                                                    end
                                                end,
                                                WithoutBadNotifications),
    {Requests, Responses} = lists:partition(fun(Single) ->
                                                case Single of
                                                    {ok, _Ns, _SingleReq} ->
                                                        true;
                                                    {ok, _Resp} ->
                                                        false
                                                    end
                                                end,
                                                GoodSingles),
    case Requests of
        [] -> %% this is a batch response
            Responses1 = [ Response || {ok, Response} <- Responses ],
            case Responses1 of
                [] -> %% nothing to do here
                    ignore;
                _NotEmpty ->
                    {ok, Responses1}
            end;
        _NotEmpty -> %% this is a batch request
            Requests1 = [ Request || {ok, _Ns, Request} <- Requests ],
            case Requests1 of
                [] -> %% nothing to do here
                    ignore;
                _NotEmpty1 ->
                    [HeadRequest | _ ] = Requests,
                    {ok, _Req} = HeadRequest,
                    {ok, Requests1}
            end
    end.

decode_single(Props) ->
    try
        JsonRPC     = req_version(Props),
        ReqId       = case proplists:get_value(<<"id">>, Props, null) of
                            null when  JsonRPC == ?JSONRPC_1 ->
                                throw({invalid_request, {JsonRPC, undefined}, <<"JSON-RPC 1.0 requires \"id\"">>});
                            null ->
                                undefined;
                            Value ->
                                Value
                      end,
        case proplists:get_value(<<"result">>, Props, undefined) of
            undefined -> %% this is a request
                decode_single_request(JsonRPC, ReqId, Props);
            _Result -> %% this is a response
                decode_single_response(JsonRPC, ReqId, Props)
        end
    catch
        throw:{_Invalid, {_JsonRPC1, undefined}, _Reason} -> %% just a notification, no need to tell anyone
            {error, ignore};
        throw:{invalid_response, {JsonRPC1, ReqId1}, Reason} -> %% an invalid response, this should never happen
            Error = build_error(invalid_response, Reason, undefined),
            Response = #jsonrpc_response{   reqid = ReqId1,
                                            result = undefined,
                                            error = Error,
                                            jsonrpc = JsonRPC1 },
            {error, Response};
        throw:{invalid_request, {JsonRPC1, ReqId1}, Reason} -> %% an invalid request should be answered with an error
            Error = build_error(invalid_request, Reason, undefined),
            Response = #jsonrpc_response{   reqid = ReqId1,
                                            result = undefined,
                                            error = Error,
                                            jsonrpc = JsonRPC1 },
            {error, Response}
    end.

decode_single_request(JsonRPC, ReqId, Props) ->
    Method      = case proplists:get_value(<<"method">>, Props, undefined) of
                    undefined ->
                        throw({invalid_request, {JsonRPC, ReqId}, <<"method does not exist">>});
                    Method1 when is_list(Method1) or is_binary(Method1) ->
                        Method1;
                    _Method1 ->
                        throw({invalid_request, {JsonRPC, ReqId}, <<"\"method\" must be a string">>})
                  end,
    Params      = case proplists:get_value(<<"params">>, Props, []) of
                    List when is_list(List)    -> List;
                    Obj = {_} when JsonRPC > 1 -> Obj;
                    _                          -> throw({invalid_request, {JsonRPC, ReqId}, <<"\"params\" must be array or object">>})
                  end,
    DecodedRequest = #jsonrpc_request{  reqid = ReqId,
                                        method = Method,
                                        params = Params,
                                        jsonrpc = JsonRPC },
    {ok, DecodedRequest}.

decode_single_response(JsonRPC, ReqId, Props) ->
    case proplists:get_value(<<"error">>, Props, null) of
        null ->
            case proplists:get_value(<<"result">>, Props, undefined) of
                undefined ->
                    throw({invalid_response, {JsonRPC, ReqId}, <<"neither \"error\" nor \"result\"">>});
                Result ->
                    Response = #jsonrpc_response{   reqid = ReqId,
                                                    result = Result,
                                                    error = undefined,
                                                    jsonrpc = JsonRPC },
                    {ok, Response}
            end;
        ErrorProps when is_list(ErrorProps) ->
            Error = #jsonrpc_error{ code = proplists:get_value(<<"code">>, ErrorProps, undefined),
                                    message = proplists:get_value(<<"message">>, ErrorProps, undefined),
                                    data = proplists:get_value(<<"data">>, ErrorProps, undefined) },
            Response = #jsonrpc_response{   reqid = ReqId,
                                            result = undefined,
                                            error = Error,
                                            jsonrpc = JsonRPC },
            {ok, Response};
        OtherError when JsonRPC =:= 1 ->
            Error = #jsonrpc_error{ code = 32000,
                                    message = OtherError,
                                    data = undefined},
            Response = #jsonrpc_response{   reqid = ReqId,
                                            result = undefined,
                                            error = Error,
                                            jsonrpc = JsonRPC },
            {ok, Response};
        _ ->
            throw({invalid_response, {JsonRPC, ReqId}, <<"JSON-RPC 2.0 requires \"error\" to be an object">>})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Create a response object representing a JSON-RPC error.
build_error(Code, Message, Data) ->
    case Code of
        parse_error ->
            NumCode = -32700, MsgPrefix = <<"Parse error">>;
        invalid_request ->
            NumCode = -32600, MsgPrefix = <<"Invalid Request">>;
        method_not_found ->
            NumCode = -32601, MsgPrefix = <<"Method not found">>;
        invalid_params ->
                NumCode = -32602, MsgPrefix = <<"Invalid params">>;
        internal_error ->
            NumCode = -32603, MsgPrefix = <<"Internal Error">>;
        %% implementation specific error codes
        binding_not_found ->
            NumCode = -32000, MsgPrefix = <<"Binding not found">>;
        invalid_response ->
            NumCode = -32001, MsgPrefix = <<"Invalid Response">>;
        server_error ->
            NumCode = -32002, MsgPrefix = <<"Server Error">>;
        _ when is_integer(Code) ->
            NumCode = Code, MsgPrefix = undefined
    end,
    case Message of
        undefined ->
            BinMessage = MsgPrefix;
        _ when MsgPrefix == undefined ->
            BinMessage = to_binary(Message);
        _ ->
            BinMessage = <<MsgPrefix/binary, ": ", (to_binary(Message))/bytes>>
    end,
    case Data of
        undefined ->
            JSONData = undefined;
        null ->
            JSONData = null;
        _ when is_number(Data) ->
            JSONData = Data;
        _ ->
            JSONData = list_to_binary(io_lib:format("~p", [Data]))
    end,
    #jsonrpc_error{code = NumCode, message = BinMessage, data = JSONData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS
decode_error_code(Code) ->
    case Code of
        -32700 -> parse_error;
        -32600 -> invalid_request;
        -32601 -> method_not_found;
        -32602 -> invalid_params;
        -32603 -> internal_error;
        -32000 -> binding_not_found;
        -32001 -> invalid_response;
        -32002 -> server_error
    end.

req_version(Props) ->
    case proplists:get_value(<<"jsonrpc">>, Props, undefined) of
        <<"2.0">> -> ?JSONRPC_2;
        <<"1.2">> -> ?JSONRPC_2;
        <<"1.0">> -> ?JSONRPC_1;
        undefined -> ?JSONRPC_1;
        _Other    ->
            %% probably a 1.1 request, could also be a future version
            case proplists:get_value(<<"result">>, Props, proplists:get_value(<<"error">>, Props)) of
                undefined ->
                    throw({invalid_req, proplists:get_value(<<"id">>, Props, undefined), <<"unknown JSON-RPC protocol version">>});
                _ ->
                    throw({invalid_resp, proplists:get_value(<<"id">>, Props, undefined), <<"unknown JSON-RPC protocol version">>})
            end
    end.

maybe_null(undefined) -> null;
maybe_null(null)      -> null;
maybe_null(Term)      -> Term.

get_namespace(Method) ->
    Method1 = to_binary(Method),
    SplittedMethod = binary:split(Method1, <<".">>, [global]),
    SplittedNs = lists:sublist(SplittedMethod, 1, length(SplittedMethod)-1),
    case SplittedNs of
        [] ->
            <<>>;
        [NsHead] ->
            NsHead;
        [NsHead | NsTail] ->
            lists:foldl( fun(NewNs, Ns) -> <<Ns/binary, ".", NewNs/binary>> end, NsHead, NsTail )
    end.

to_binary(Str) when is_atom(Str)   -> atom_to_binary(Str, utf8);
to_binary(Str) when is_list(Str)   -> unicode:characters_to_binary(Str);
to_binary(Str) when is_binary(Str) -> Str;
to_binary(Term) ->
    unicode:characters_to_binary(io_lib:format("~p", [Term])).

