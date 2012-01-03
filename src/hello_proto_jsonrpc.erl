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

% @private
-module(hello_proto_jsonrpc).

%% new interface
-export([defaults/0, new_request/4, std_error_to_error_resp/1, error_resp_to_error_reply/1, encode/1, decode/1]).

-include("internal.hrl").
-record(jsonrpc, {
    version = 2 :: 1..2
}).

-spec std_error_to_error_resp(Error::hello_proto:standard_error()) -> hello_proto:response().
-spec decode(binary()) -> #request{} | #response{} | #error{}.
-spec encode(hello_proto:request() | hello_proto:response()) -> binary().

defaults() ->
    #jsonrpc{}.

new_request(ReqId, Method, Args, JSONRPC) when is_list(Args) orelse (is_tuple(Args) and tuple_size(Args) == 1 and is_list(element(1, Args))) ->
    #request{reqid = ReqId, method = Method, params = Args, proto_data = JSONRPC, proto_mod = ?MODULE}.

%% @doc Create a response object representing a JSON-RPC standard error.
std_error_to_error_resp(Error) ->
    {Code, Msg} = case Error of
                      parse_error         -> {-32700, "Parse error"};
                      invalid_request     -> {-32600, "Invalid Request"};
                      method_not_found    -> {-32601, "Method not found"};
                      invalid_params      -> {-32602, "Invalid params"};
                      {invalid_params, M} -> {-32602, "Invalid params: " ++ M};
                      internal_error      -> {-32603, "Internal Error"};
                      server_error        -> {-32099, "Server Error"};
                      {E, _}              -> {-32099, io_lib:format("Server Error: ~p", [E])};
                      E                   -> {-32099, io_lib:format("Server Error: ~p", [E])}
                  end,
    Data = case Error of
              {server_error, Term} -> list_to_binary(io_lib:format("~p", [Term]));
              _                    -> undefined
           end,
    #error{code = Code, message = Msg, data = Data}.

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
encode(empty_response) ->
    <<>>;
encode(Resps) when is_list(Resps) ->
    Conc = lists:foldl(fun (Resp, Bin) ->
                               case Resp of
                                   empty_response -> Bin;
                                   _              -> <<Bin/binary, ",", (encode(Resp))/binary>>
                               end
                       end, <<>>, Resps),
    case Conc of
        <<>>               -> <<>>;
        <<",",Res/binary>> -> <<"[", Res/binary, "]">>
    end;
encode(R = #response{reqid = ID, proto_data = #jsonrpc{version = 1}}) ->
    hello_json:encode({[{<<"error">>, null}, {<<"result">>, R#response.result}, {<<"id">>, ID}]});
encode(R = #response{reqid = ID, proto_data = #jsonrpc{version = 2}}) ->
    hello_json:encode({[{<<"result">>, R#response.result}, {<<"id">>, ID}, {<<"jsonrpc">>, <<"2.0">>}]});
encode(R = #error{proto_data = #jsonrpc{version = 1}}) ->
    case R#error.data of
        undefined ->
            ErrorObj = {[{<<"message">>, maybe_null(R#error.message)}, {<<"code">>, maybe_null(R#error.code)}]};
        Data ->
            ErrorObj = {[{<<"data">>, Data}, {<<"message">>, maybe_null(R#error.message)}, {<<"code">>, maybe_null(R#error.code)}]}
    end,
    hello_json:encode({[{<<"result">>, null}, {<<"error">>, ErrorObj}, {<<"id">>, R#error.reqid}]});
encode(R = #error{proto_data = #jsonrpc{version = 2}}) ->
    case R#error.data of
        undefined ->
            ErrorObj = {[{<<"message">>, maybe_null(R#error.message)}, {<<"code">>, maybe_null(R#error.code)}]};
        Data ->
            ErrorObj = {[{<<"data">>, Data}, {<<"message">>, maybe_null(R#error.message)}, {<<"code">>, maybe_null(R#error.code)}]}
    end,
    hello_json:encode({[{<<"result">>, null}, {<<"error">>, ErrorObj}, {<<"id">>, R#error.reqid}, {<<"jsonrpc">>, <<"2.0">>}]});

encode(R = #request{reqid = undefined, proto_data = #jsonrpc{version = 1}}) ->
    hello_json:encode({[{<<"id">>, null}, {<<"params">>, R#request.params}, {<<"method">>, R#request.method}]});
encode(R = #request{reqid = undefined, proto_data = #jsonrpc{version = 2}}) ->
    hello_json:encode({[{<<"params">>, R#request.params}, {<<"method">>, R#request.method}, {<<"jsonrpc">>, <<"2.0">>}]});
encode(R = #request{reqid = Id, proto_data = #jsonrpc{version = 1}}) ->
    hello_json:encode({[{<<"id">>, Id}, {<<"params">>, R#request.params}, {<<"method">>, R#request.method}]});
encode(R = #request{reqid = Id, proto_data = #jsonrpc{version = 2}}) ->
    hello_json:encode({[{<<"id">>, Id}, {<<"params">>, R#request.params}, {<<"method">>, R#request.method}, {<<"jsonrpc">>, <<"2.0">>}]}).

%% ----------------------------------------------------------------------
%% -- Decoding
decode(Binary) ->
    case hello_json:decode(Binary) of
        {error, _Error} ->
            {error, parse_error};
        {ok, Request, _Rest} ->
            decode_json(Request)
    end.

-spec decode_json(hello_json:json_value()) -> hello_proto:request() | hello_proto:response() | {error, hello_proto:std_error()} | {batch, Valid, Invalid} when
    Valid   :: [hello_proto:request()],
    Invalid :: [hello_proto:response()].
%% @doc Create a request object from parsed request structure
%%      A request or response is returned if the given request/response is valid.<br/>
%%      `{error, StdError}' is returned if the given request is not valid.<br/>
%%      `{batch, Valid, Invalid}' is returned for batch requests. `Valid' contains
%%      all valid requests in the batch, `Invalid' is a list of error responses for the ones that were invalid.
decode_json(Obj) ->
    case Obj of
        [] ->
            {error, std_error_to_error_resp(invalid_request)};
        Lis when is_list(Lis) ->
            {Valid, Invalid} = lists:foldl(
                    fun (ReqObj, {Va, Iva}) ->
                            case single_request(ReqObj) of
                                {ok, Req}     -> {[Req | Va], Iva};
                                {error, Resp} -> {Va, [Resp | Iva]}
                            end
                    end, {[], []}, Lis),
            {batch, Valid, Invalid};
        _ ->
            single_request(Obj)
    end.

single_request({Props}) ->
    try
        Version = req_version(Props),
        ID      = case Version of
                      2 -> proplists:get_value(<<"id">>, Props);
                      1 -> case proplists:get_value(<<"id">>, Props) of
                              undefined -> throw(invalid);
                              null      -> undefined;
                              Value     -> Value
                          end
                  end,
        case property(Props, <<"method">>) of
            Method when is_binary(Method) ->
                Params = case property(Props, <<"params">>, []) of
                             List when is_list(List)    -> List;
                             Obj = {_} when Version > 1 -> Obj;
                             _                          -> throw(invalid)
                         end,
                #request{reqid = ID,
                         method = Method,
                         params = Params,
                         proto_mod = ?MODULE,
                         proto_data = #jsonrpc{version = Version}};
            undefined ->
                single_response(Version, ID, Props);
            _ ->
                throw(invalid)
        end
    catch
        throw:invalid -> {error, invalid_request}
    end;
single_request(_Other) ->
    {error, invalid_request}.

single_response(Version, ID, Props) ->
    case proplists:get_value(<<"error">>, Props) of
        undefined ->
            case proplists:get_value(<<"result">>, Props) of
                undefined ->
                    throw(invalid);
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
            throw(invalid)
    end.

req_version(Props) ->
    case property(Props, <<"jsonrpc">>) of
        <<"2.0">> -> 2;
        <<"1.2">> -> 2;
        <<"1.0">> -> 1;
        undefined -> 1;
        _Other    -> throw(invalid)
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
