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
-export([new_request/4, new_notification/3, new_batch_request/2]).
-export([success_response/2, error_response/2, error_response/3, error_response/4,
         batch_response/2, error_resp_to_error_reply/1, mime_type/1]).
-export([encode/1, decode/2]).
-export_type([request/0, response/0, standard_error/0]).

-include("internal.hrl").

-type request()  :: #request{} | #batch_request{}.
-type response() :: #response{} | #error{} | #batch_response{}.

-type error_code()    :: integer() | standard_error().
-type error_message() :: undefined | term().
-type error_data()    :: undefined | hello_json:value().
-type error_reply()   :: error_code() | {error_code(), error_message()} | {error_code(), error_message(), error_data()}.

-type standard_error() :: parse_error | invalid_request | method_not_found | invalid_params | internal_error | server_error.

%% ----------------------------------------------------------------------------------------------------
%% -- Record Creation/Conversion
-spec new_request(module(), hello_json:value(), atom() | string() | binary(), hello_json:value()) -> #request{}.
new_request(Mod, ReqId, Method, Args) when is_atom(Mod) ->
    Mod:new_request(ReqId, to_binary(Method), Args).

-spec new_notification(module(), atom() | string() | binary(), hello_json:value()) -> #request{}.
new_notification(Mod, Method, Args) when is_atom(Mod) ->
    Mod:new_request(undefined, to_binary(Method), Args).

-spec new_batch_request(module(), [#request{}]) -> #batch_request{}.
new_batch_request(Mod, Requests) when is_atom(Mod) ->
    #batch_request{proto_mod = Mod, requests = Requests}.

to_binary(Str) when is_atom(Str)   -> atom_to_binary(Str, utf8);
to_binary(Str) when is_list(Str)   -> unicode:characters_to_binary(Str);
to_binary(Str) when is_binary(Str) -> Str.

-spec batch_response(#batch_request{}, list(#response{})) -> #batch_response{} | ignore.
batch_response(#batch_request{proto_mod = Mod, errors = Errors}, Responses) ->
    case [R || R <- Errors ++ Responses, R /= ignore] of
        [] ->
            ignore;
        AllResps ->
            #batch_response{proto_mod = Mod, responses = AllResps}
    end.

-spec success_response(#request{}, hello_json:value()) -> #response{}.
success_response(Req, Result) ->
    #response{proto_mod  = Req#request.proto_mod,
              proto_data = Req#request.proto_data,
              reqid      = Req#request.reqid,
              result     = Result}.

-spec error_response(#request{}, error_reply()) -> #error{}.
error_response(Req, {ErrorCode, ErrorMsg}) ->
    error_response(Req, ErrorCode, ErrorMsg);
error_response(Req, {ErrorCode, ErrorMsg, ErrorData}) ->
    error_response(Req, ErrorCode, ErrorMsg, ErrorData);
error_response(Req, ErrorCode) ->
    error_response(Req, ErrorCode, undefined).

%% @equiv error_response(Req, Code, Msg, undefined)
-spec error_response(#request{}, error_code(), error_message()) -> #error{}.
error_response(Req, Code, Msg) ->
    error_response(Req, Code, Msg, undefined).

%% @doc Creates a response object that represents an error response to the given request.
-spec error_response(#request{}, error_code(), error_message(), error_data()) -> #error{}.
error_response(#request{proto_mod = Mod, proto_data = ProtoData, reqid = ReqId}, Code, Msg, Data) ->
    Mod:error_response(ProtoData, ReqId, Code, Msg, Data).

error_resp_to_error_reply(Error = #error{proto_mod = Mod}) ->
    Mod:error_resp_to_error_reply(Error).

-spec mime_type(module() | request()) -> binary().
mime_type(#request{proto_mod = Mod}) -> Mod:mime_type();
mime_type(#batch_request{proto_mod = Mod}) -> Mod:mime_type();
mime_type(Mod) when is_atom(Mod)     -> Mod:mime_type().

%% ----------------------------------------------------------------------------------------------------
%% -- Encoding/Decoding
-spec encode(request() | response()) -> binary().
encode(Req = #request{proto_mod = Mod}) ->
    Mod:encode(Req);
encode(Req = #batch_request{proto_mod = Mod}) ->
    Mod:encode(Req);
encode(Resp = #response{proto_mod = Mod}) ->
    Mod:encode(Resp);
encode(Resp = #error{proto_mod = Mod}) ->
    Mod:encode(Resp);
encode(Resp = #batch_response{proto_mod = Mod}) ->
    Mod:encode(Resp).

-spec decode(module() | request(), binary()) -> request() | response() | {proto_reply, response()}.
decode(Mod, Binary) when is_atom(Mod) ->
    Mod:decode(Binary);
decode(#request{proto_mod = Mod}, Binary) ->
    Mod:decode(Binary);
decode(#batch_request{proto_mod = Mod}, Binary) ->
    Mod:decode(Binary).
