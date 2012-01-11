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

%% new interface
-export([mime_type/0, defaults/0, new_request/3, error_response/5, error_resp_to_error_reply/1, encode/1, decode/1]).

-include("internal.hrl").
-record(jsonrpc, {
    version = 2 :: 1..2
}).

mime_type() ->
    <<"application/json">>.

defaults() ->
    #jsonrpc{}.

new_request(ReqId, Method, Args) when is_list(Args) orelse (is_tuple(Args) and (tuple_size(Args) == 1) and is_list(element(1, Args))) ->
    #request{reqid = ReqId, method = Method, params = Args, proto_data = defaults(), proto_mod = ?MODULE}.

%% @doc Create a response object representing a JSON-RPC error.
error_response(ProtoData, ReqId, Code, Message, Data) ->
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
        server_error ->
            NumCode = -32099, MsgPrefix = <<"Server Error">>;
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
    #error{proto_mod = ?MODULE, proto_data = ProtoData, reqid = ReqId, code = NumCode, message = BinMessage, data = JSONData}.

to_binary(Str) when is_atom(Str)   -> atom_to_binary(Str, utf8);
to_binary(Str) when is_list(Str)   -> unicode:characters_to_binary(Str);
to_binary(Str) when is_binary(Str) -> Str;
to_binary(Term) ->
    unicode:characters_to_binary(io_lib:format("~p", [Term])).

error_resp_to_error_reply(#error{code = Code, message = Msg}) ->
    case Code of
        -32700 -> {parse_error, Msg};
        -32600 -> {invalid_request, Msg};
        -32601 -> {method_not_found, Msg};
        -32602 -> {invalid_params, Msg};
        -32603 -> {internal_error, Msg};
        -32099 -> {server_error, Msg};
        _      -> {Code, Msg}
    end.

%% ----------------------------------------------------------------------------------------------------
%% -- Encoding
-spec encode(hello_proto:request() | hello_proto:response()) -> binary().
encode(Thing) ->
    hello_json:encode(encode_json(Thing)).

encode_json(R = #response{reqid = ID, proto_data = #jsonrpc{version = 1}}) ->
    {[{<<"error">>, null}, {<<"result">>, R#response.result}, {<<"id">>, ID}]};
encode_json(R = #response{reqid = ID, proto_data = #jsonrpc{version = 2}}) ->
    {[{<<"result">>, R#response.result}, {<<"id">>, ID}, {<<"jsonrpc">>, <<"2.0">>}]};
encode_json(R = #error{proto_data = #jsonrpc{version = 1}}) ->
    case R#error.data of
        undefined ->
            ErrorObj = {[{<<"message">>, maybe_null(R#error.message)}, {<<"code">>, maybe_null(R#error.code)}]};
        Data ->
            ErrorObj = {[{<<"data">>, Data}, {<<"message">>, maybe_null(R#error.message)}, {<<"code">>, maybe_null(R#error.code)}]}
    end,
    {[{<<"result">>, null}, {<<"error">>, ErrorObj}, {<<"id">>, R#error.reqid}]};
encode_json(R = #error{proto_data = #jsonrpc{version = 2}}) ->
    case R#error.data of
        undefined ->
            ErrorObj = {[{<<"message">>, maybe_null(R#error.message)}, {<<"code">>, maybe_null(R#error.code)}]};
        Data ->
            ErrorObj = {[{<<"data">>, Data}, {<<"message">>, maybe_null(R#error.message)}, {<<"code">>, maybe_null(R#error.code)}]}
    end,
    {[{<<"result">>, null}, {<<"error">>, ErrorObj}, {<<"id">>, R#error.reqid}, {<<"jsonrpc">>, <<"2.0">>}]};
encode_json(#batch_response{responses = Resps}) ->
    [encode_json(R) || R <- Resps];

encode_json(R = #request{reqid = undefined, proto_data = #jsonrpc{version = 1}}) ->
    {[{<<"id">>, null}, {<<"params">>, R#request.params}, {<<"method">>, R#request.method}]};
encode_json(R = #request{reqid = undefined, proto_data = #jsonrpc{version = 2}}) ->
    {[{<<"params">>, R#request.params}, {<<"method">>, R#request.method}, {<<"jsonrpc">>, <<"2.0">>}]};
encode_json(R = #request{reqid = Id, proto_data = #jsonrpc{version = 1}}) ->
    {[{<<"id">>, Id}, {<<"params">>, R#request.params}, {<<"method">>, R#request.method}]};
encode_json(R = #request{reqid = Id, proto_data = #jsonrpc{version = 2}}) ->
    {[{<<"id">>, Id}, {<<"params">>, R#request.params}, {<<"method">>, R#request.method}, {<<"jsonrpc">>, <<"2.0">>}]};
encode_json(#batch_request{requests = Reqs}) ->
    [encode_json(R) || R <- Reqs].

%% ----------------------------------------------------------------------
%% -- Decoding
-spec decode(binary()) -> hello_proto:request() | hello_proto:response() | {proto_reply, hello_proto:response()}.
decode(Binary) ->
    case hello_json:decode(Binary) of
        {error, _Error} ->
            {proto_reply, error_response(defaults(), undefined, parse_error, undefined, undefined)};
        {ok, Request, _Rest} ->
            decode_json(Request)
    end.

-spec decode_json(hello_json:value()) -> hello_proto:request() | hello_proto:response() | {proto_reply, hello_proto:response()}.
decode_json(Obj) ->
    case Obj of
        [] ->
            {proto_reply, error_response(defaults(), undefined, invalid_request, <<"empty batch">>, undefined)};
        Lis when is_list(Lis) ->
            decode_batch(Lis);
        _ ->
            single_request(Obj)
    end.

-spec decode_batch([hello_json:value()]) -> #batch_request{} | #batch_response{}.
decode_batch([First | Rest]) ->
    case single_request(First) of
        Req = #request{} ->
            decode_batch_request(Rest, [Req], []);
        {proto_reply, Error} ->
            decode_batch_request(Rest, [], [Error]);
        Resp = #response{} ->
            decode_batch_response(Rest, [Resp]);
        Resp = #error{} ->
            decode_batch_response(Rest, [Resp])
    end.

-spec decode_batch_request([hello_json:value()], [#request{}], [#error{}]) -> #batch_request{}.
decode_batch_request([], ReqAcc, ErrorAcc) ->
    #batch_request{proto_mod = ?MODULE, requests = ReqAcc, errors = ErrorAcc};
decode_batch_request([Obj | Rest], ReqAcc, ErrorAcc) ->
    case single_request(Obj) of
        #error{reqid = Id, proto_data = Data} ->
            Error = error_response(Data, Id, invalid_request, <<"response object">>, undefined),
            decode_batch_request(Rest, ReqAcc, [Error | ErrorAcc]);
        #response{reqid = Id, proto_data = Data} ->
            Error = error_response(Data, Id, invalid_request, <<"response object">>, undefined),
            decode_batch_request(Rest, ReqAcc, [Error | ErrorAcc]);
        {proto_reply, Error} ->
            decode_batch_request(Rest, ReqAcc, [Error | ErrorAcc]);
        Request ->
            decode_batch_request(Rest, [Request | ReqAcc], ErrorAcc)
    end.

-spec decode_batch_response([hello_json:value()], [#response{} | #error{}]) -> #batch_response{}.
decode_batch_response([], RespAcc) ->
    #batch_response{proto_mod = ?MODULE, responses = RespAcc};
decode_batch_response([Obj | Rest], RespAcc) ->
    case single_request(Obj) of
        Resp = #error{} ->
            decode_batch_response(Rest, [Resp | RespAcc]);
        Resp = #response{} ->
            decode_batch_response(Rest, [Resp | RespAcc]);
        _ ->
            decode_batch_response(Rest, RespAcc)
    end.

single_request({Props}) ->
    try
        Version = req_version(Props),
        ID      = case Version of
                      2 -> proplists:get_value(<<"id">>, Props);
                      1 -> case proplists:get_value(<<"id">>, Props) of
                              undefined -> throw({invalid_req, undefined, <<"JSON-RPC 1.0 requires \"id\"">>});
                              null      -> undefined;
                              Value     -> Value
                          end
                  end,
        case property(Props, <<"method">>) of
            Method when is_binary(Method) ->
                Params = case property(Props, <<"params">>, []) of
                             List when is_list(List)    -> List;
                             Obj = {_} when Version > 1 -> Obj;
                             _                          -> throw({invalid_req, ID, <<"\"params\" must be array or object">>})
                         end,
                #request{reqid = ID,
                         method = Method,
                         params = Params,
                         proto_mod = ?MODULE,
                         proto_data = #jsonrpc{version = Version}};
            undefined ->
                single_response(Version, ID, Props);
            _ ->
                throw({invalid_req, ID, <<"\"method\" must be a string">>})
        end
    catch
        throw:{invalid_req, ReqId, Reason} ->
            {proto_reply, error_response(defaults(), ReqId, invalid_request, Reason, undefined)};
        throw:{invalid_resp, ReqId, Reason} ->
            %% decode invalid responses as 'internal_error' to satisfy waiting clients
            #error{reqid = ReqId,
                   code = -32603,
                   message = <<"invalid JSON-RPC response: ", Reason/binary>>,
                   proto_mod = ?MODULE,
                   proto_data = defaults()}
    end;
single_request(_Other) ->
    {proto_reply, error_response(defaults(), undefined, invalid_request, <<"non-object">>, undefined)}.

single_response(Version, ID, Props) ->
    case proplists:get_value(<<"error">>, Props) of
        undefined ->
            case proplists:get_value(<<"result">>, Props) of
                undefined ->
                    throw({invalid_resp, ID, <<"neither \"error\" nor \"result\"">>});
                Result ->
                    #response{reqid = ID,
                              result = Result,
                              proto_mod = ?MODULE,
                              proto_data = #jsonrpc{version = Version}}
            end;
        {ErrorProps} ->
            #error{reqid = ID,
                   code = property(ErrorProps, <<"code">>),
                   message = property(ErrorProps, <<"message">>),
                   data = property(ErrorProps, <<"data">>),
                   proto_mod = ?MODULE,
                   proto_data = #jsonrpc{version = Version}};
        OtherError when Version =:= 1 ->
            #error{reqid = ID,
                   code = undefined,
                   message = OtherError,
                   proto_mod = ?MODULE,
                   proto_data = #jsonrpc{version = Version}};
        _ ->
            throw({invalid_resp, ID, <<"JSON-RPC 2.0 requires \"error\" to be an object">>})
    end.

req_version(Props) ->
    case property(Props, <<"jsonrpc">>) of
        <<"2.0">> -> 2;
        <<"1.2">> -> 2;
        <<"1.0">> -> 1;
        undefined -> 1;
        _Other    ->
            %% probably a 1.1 request, could also be a future version
            case proplists:get_value(<<"result">>, Props, proplists:get_value(<<"error">>, Props)) of
                undefined ->
                    throw({invalid_req, proplists:get_value(<<"id">>, Props), <<"unknown JSON-RPC protocol version">>});
                _ ->
                    throw({invalid_resp, proplists:get_value(<<"id">>, Props), <<"unknown JSON-RPC protocol version">>})
            end
    end.

property(Plist, Key) ->
    property(Plist, Key, undefined).
property(Plist, Key, Default) ->
    case proplists:get_value(Key, Plist, undefined) of
        undefined -> Default;
        null      -> Default;
        Value     -> Value
    end.

maybe_null(undefined) -> null;
maybe_null(null)      -> null;
maybe_null(Term)      -> Term.
