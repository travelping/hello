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
         build_request/3,
         encode/2,
         decode/3,
         signature/1
         ]).

-include("hello.hrl").
-include("hello_log.hrl").
-include("jsonrpc_internal.hrl").
-import(hello_lib, [get_in/2, get/2, get/3, to_binary/1]).


%% -------------------------------------------------------------------------------------
%% -- callbacks for the hello_proto behaviour
init_client(Proplist) ->
    JsonRPCVersion = proplists:get_value(jsonrpc, Proplist, ?JSONRPC_2),
    case lists:member(JsonRPCVersion, [?JSONRPC_1, ?JSONRPC_2]) of
        true ->
            {ok, #jsonrpc_info{ reqid = 0, version = JsonRPCVersion} };
        _ ->
            ?LOG_INFO("Hello client invoked with invalid JSONRPC version '~p'.", [JsonRPCVersion], [], ?LOGID61),
            {error, invalid_json_version}
    end.

build_request(SingleRequest, Options, State = #jsonrpc_info{ reqid = ReqId }) ->
    Notify = proplists:get_value(notification, Options, false),
    Info = case Notify of
        false ->
            State;
        true ->
            State#jsonrpc_info{reqid = undefined}
    end,
    Request = SingleRequest#request{type = type(Info#jsonrpc_info.reqid), proto_data = Info, id = ReqId},
    {ok, Request, State#jsonrpc_info{ reqid = ReqId+1 }}.

encode(Batch, _Opts) when is_list(Batch)  ->
    EncodedBatch = [ encode_single(Request) || Request <- Batch ],
    {ok, hello_json:encode(EncodedBatch)};
encode(Single, _Opts) ->
    EncodedSingle = encode_single(Single),
    {ok, hello_json:encode(EncodedSingle)}.

decode(Binary, _Opts, Type) ->
    try
        case hello_json:decode(to_binary(Binary)) of
            Batch = [ Single | _ ] when is_map(Single) ->
                decode_batch(Batch, Type);
            Single ->
                decode_single(Single, Type)
        end
    catch
        Error:Reason ->
            ?LOG_INFO("Hello proto unable to decode binary request with error '~p'.", [Error],
                        [{hello_error_reason, {Error, Reason, erlang:get_stacktrace()}}], ?LOGID62),
            {error, #error{code = parse_error}}
    end.

signature(_Opts) -> hello_json:signature().

%% ----------------------------------------------------------------------------------------------------
%% -- Encoding
%% request encoding
encode_single(#request{proto_data = #jsonrpc_info{reqid = ReqId, version = ?JSONRPC_1}, method = Method, args = Params}) ->
    [{<<"id">>, maybe_null(ReqId)}, {<<"params">>, Params}, {<<"method">>, Method}];
encode_single(#request{proto_data = #jsonrpc_info{reqid = ReqId, version = ?JSONRPC_2}, method = Method, args = Params}) ->
    [{<<"id">>, maybe_null(ReqId)}, {<<"params">>, Params}, {<<"method">>, Method}, {<<"jsonrpc">>, <<"2.0">>}];

%% response encoding
%% note in v1.0, The result must be null in case there was an error invoking the method.
encode_single(#response{proto_data = #jsonrpc_info{reqid = ReqId, version = ?JSONRPC_1}, response = #error{} = Error}) ->
    [{<<"error">>, encode_single(Error)}, {<<"result">>, null}, {<<"id">>, ReqId}];
%% note in v2.0, The result member MUST NOT exist if there was an error invoking the method.
encode_single(#response{proto_data = #jsonrpc_info{reqid = ReqId, version = ?JSONRPC_2}, response = #error{} = Error}) ->
    [{<<"error">>, encode_single(Error)}, {<<"id">>, ReqId}, {<<"jsonrpc">>, <<"2.0">>}];
encode_single(#response{proto_data = #jsonrpc_info{reqid = ReqId, version = ?JSONRPC_1}, response = Result}) ->
    [{<<"error">>, null}, {<<"result">>, Result}, {<<"id">>, ReqId}, {<<"jsonrpc">>, <<"1.0">>}];
encode_single(#response{proto_data = #jsonrpc_info{reqid = ReqId, version = ?JSONRPC_2}, response = Result}) ->
    [{<<"result">>, Result}, {<<"id">>, ReqId}, {<<"jsonrpc">>, <<"2.0">>}];

%% error encoding
encode_single(#error{} = Error) ->
    #error{code = Code, message = Message, proto_data = Data} = build_error(Error),
    DataPart = [{<<"data">>, Data} || (Data =/= null andalso Data =/= undefined)],
    [{<<"message">>, maybe_null(Message)}, {<<"code">>, maybe_null(Code)} | DataPart];
encode_single(undefined) ->
    null.

%% ----------------------------------------------------------------------
%% -- Decoding
decode_batch([], Type) ->
    {error, #error{code = invalid(Type)}};
decode_batch(Batch, Type) ->
    ShelledSingles = [ decode_single(Single, Type) || Single <- Batch ],
    {ok, [ ShelledSingle || ShelledSingle <- ShelledSingles, ShelledSingle /= ignore ]}.

decode_single(Object, Type) ->
    try
        JsonRPC = req_version(Object),
        ReqId   = request_id(JsonRPC, Object),
        Info    = #jsonrpc_info{reqid = ReqId, version = JsonRPC},
        decode_single(Type, Object, Info)
    catch
        throw:{_Invalid, #jsonrpc_info{reqid = null}, _Reason} -> %% just a notification, no need to tell anyone
            ?LOG_INFO("Hello proto attempted to decode invalid notification object.", [],
                        [{hello_error_reason, {invalid_notification, Object}}], ?LOGID63),
            ignore;
        throw:{invalid, Info1, Reason} -> %% an invalid response, this should never happen
            Error =  build_error(#error{code = invalid(Type), message = Reason}),
            Response = #response{proto_data = Info1, response = Error},
            {error, Response}
    end.

decode_single(request, #{<<"method">> := Method} = Object, #jsonrpc_info{reqid = ReqId, version = JsonRPC} = Info)
  when is_list(Method) orelse is_binary(Method) ->
    Params = case get(Object, <<"params">>, []) of
                 List when is_list(List) -> List;
                 Obj = #{} when JsonRPC > ?JSONRPC_1 -> Obj;
                 _ -> throw({invalid, Info, <<"\"params\" must be array or object">>})
             end,
    Request = #request{method = Method, type=type(ReqId), args = Params, proto_data = Info, id = ReqId},
    {ok, Request};
decode_single(request, #{<<"method">> := _Method} = _Object, Info) ->
    throw({invalid, Info, <<"\"method\" must be a string">>});
decode_single(request, _Object, Info) ->
    throw({invalid, Info, <<"method does not exist">>});
decode_single(response, #{<<"error">> := Error = #{}} = _Object, Info) ->
    ErrorRec = #error{ code = get(Error, <<"code">>),
                       message = get(Error, <<"message">>),
                       proto_data = get(Error, <<"data">>) },
    {ok, #response{response = ErrorRec, proto_data = Info, id = Info#jsonrpc_info.reqid}};
decode_single(response, #{<<"error">> := Error} = _Object, #jsonrpc_info{version = ?JSONRPC_1} = Info) ->
    ErrorRec = #error{ code = 32000,
                       message = Error,
                       proto_data = undefined},
    {ok, #response{response = ErrorRec, proto_data = Info, id = Info#jsonrpc_info.reqid}};
decode_single(response, #{<<"error">> := Error} = _Object, Info) when (Error =/= null) and (Error =/= nil) ->
    throw({invalid, Info, <<"JSON-RPC 2.0 requires \"error\" to be an object">>});
decode_single(response, #{<<"result">> := Result}, Info) ->
    {ok, #response{response = Result, proto_data = Info, id = Info#jsonrpc_info.reqid}};
decode_single(response, _Object, Info) ->
    throw({invalid, Info, <<"neither \"error\" nor \"result\"">>}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
invalid(request)  -> invalid_request;
invalid(response) -> invalid_response.

%% @doc Create a response object representing a JSON-RPC error.
build_error(#error{code = Code, message = Message, proto_data = Data}) ->
    {NumCode, MsgPrefix} = case Code of
        parse_error ->
            {-32700, <<"Parse error">>};
        invalid_request ->
            {-32600, <<"Invalid Request">>};
        method_not_found ->
            {-32601, <<"Method not found">>};
        invalid_params ->
            {-32602, <<"Invalid params">>};
        internal_error ->
            {-32603, <<"Internal Error">>};
        invalid_response ->
            {-32001, <<"Invalid Response">>};
        server_error ->
            {-32002, <<"Server Error">>};
        _ when is_integer(Code) ->
            {Code, undefined}
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
    #error{code = NumCode, message = BinMessage, proto_data = JSONData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS
%decode_error_code(Code) ->
    %case Code of
        %-32700 -> parse_error;
        %-32600 -> invalid_request;
        %-32601 -> method_not_found;
        %-32602 -> invalid_params;
        %-32603 -> internal_error;
        %-32000 -> binding_not_found;
        %-32001 -> invalid_response;
        %-32002 -> server_error
    %end.

req_version(#{<<"jsonrpc">> := <<"2.0">>}) -> ?JSONRPC_2;
req_version(#{<<"jsonrpc">> := <<"1.2">>}) -> ?JSONRPC_2;
req_version(#{<<"jsonrpc">> := <<"1.0">>}) -> ?JSONRPC_1;
req_version(#{<<"Jsonrpc">> := _JsonRPC} = Req) -> throw({invalid, #jsonrpc_info{reqid = get(Req, <<"id">>)}, <<"unknown JSON-RPC protocol version">>});
req_version(#{}) -> ?JSONRPC_1.

request_id(JsonRPC, Object) ->
    case get(Object, <<"id">>, null) of
        null when JsonRPC == ?JSONRPC_1 ->
            throw({invalid, #jsonrpc_info{version = JsonRPC}, <<"JSON-RPC 1.0 requires \"id\"">>});
        <<"undefined">> -> undefined;
        Value ->
            Value
    end.

type(null) -> async;
type(undefined) -> async;
type(_) -> sync.

maybe_null(undefined) -> null;
maybe_null(Term)      -> Term.
