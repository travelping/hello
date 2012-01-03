-module(hello_proto).
-export([new_request/4, success_response/2, error_response/2, error_response/3, error_response/4, error_resp_to_error_reply/1, encode/1, decode/2]).
-export_type([request/0, response/0, standard_error/0]).

-include("internal.hrl").

-type request()  :: #request{}.
-type response() :: #response{} | #error{}.

-type error_message() :: term().
-type error_code()    :: integer().
-type error_data()    :: hello_json:value().
-type error_reply()   :: error_code() | {error_code(), error_message()} | {error_code(), error_message(), error_data()}.

-type standard_error() :: parse_error | invalid_request | method_not_found | invalid_params | {invalid_params, string()} | internal_error | server_error | {server_error, string()} | string().

%% ----------------------------------------------------------------------------------------------------
%% -- Record Creation/Conversion
-spec new_request(module() | request(), hello_json:value(), atom() | string() | binary(), hello_json:value()) -> request().
new_request(Mod, ReqId, Method, Args) when is_atom(Mod) ->
    Mod:new_request(ReqId, to_binary(Method), Args, Mod:defaults());
new_request(#request{proto_mod = Mod, proto_data = Data}, ReqId, Method, Args) ->
    Mod:new_request(ReqId, to_binary(Method), Args, Data).

to_binary(Str) when is_atom(Str)   -> atom_to_binary(Str, utf8);
to_binary(Str) when is_list(Str)   -> unicode:characters_to_binary(Str);
to_binary(Str) when is_binary(Str) -> Str.

-spec success_response(request(), hello_json:value()) -> response().
success_response(Req, Result) ->
    #response{proto_mod  = Req#request.proto_mod,
              proto_data = Req#request.proto_data,
              reqid      = Req#request.reqid,
              result     = Result}.

-spec error_response(request(), error_reply()) -> response().
error_response(Req, ErrorCode) when is_integer(ErrorCode) ->
    error_response(Req, ErrorCode, <<>>);
error_response(Req, {ErrorCode, ErrorMsg}) when is_integer(ErrorCode) ->
    error_response(Req, ErrorCode, ErrorMsg);
error_response(Req, {ErrorCode, ErrorMsg, ErrorData}) when is_integer(ErrorCode) ->
    error_response(Req, ErrorCode, ErrorMsg, ErrorData).

%% @equiv error_response(Req, Code, Msg, undefined)
-spec error_response(request(), error_code(), error_message()) -> response().
error_response(Req, Code, Msg) ->
    error_response(Req, Code, Msg, undefined).

%% @doc Creates a response object that represents an error response to the given request.
-spec error_response(request(), error_code(), error_message(), error_data()) -> response().
error_response(Req, Code, Msg, Data) ->
    MsgBin = if is_binary(Msg) -> Msg;
                is_list(Msg)   -> list_to_binary(Msg);
                true           -> list_to_binary(io_lib:format("~w", [Msg]))
             end,
    #error{proto_mod  = Req#request.proto_mod,
           proto_data = Req#request.proto_data,
           reqid      = Req#request.reqid,
           code       = Code,
           message    = MsgBin,
           data       = Data}.

error_resp_to_error_reply(Error = #error{proto_mod = Mod}) ->
    Mod:error_resp_to_error_reply(Error).

%% ----------------------------------------------------------------------------------------------------
%% -- Encoding/Decoding
-spec encode(request() | response()) -> binary().
encode(Req = #request{proto_mod = Mod}) ->
    Mod:encode(Req);
encode(Resp = #response{proto_mod = Mod}) ->
    Mod:encode(Resp);
encode(Resp = #error{proto_mod = Mod}) ->
    Mod:encode(Resp).

-spec decode(module() | request(), binary()) -> request() | response() | {error, atom()} | {batch, [request()], [response()]}.
decode(Mod, Binary) when is_atom(Mod) ->
    Mod:decode(Binary);
decode(#request{proto_mod = Mod}, Binary) ->
    Mod:decode(Binary).
