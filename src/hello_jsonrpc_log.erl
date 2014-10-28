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
-module(hello_jsonrpc_log).
-export([request/5, bad_request/5,fmt_request/1]).

-include("internal.hrl").
-include("jsonrpc_internal.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").

%% --------------------------------------------------------------------------------
%% -- API
-spec request(module(), atom() | pid(), binary(), hello_proto:request(), hello_proto:response()) -> ok | {error, no_such_log}.
request(CallbackModule, Handler, ExUriUrl, Request, Response) ->
    Endpoint = list_to_binary(ex_uri:encode(ExUriUrl)),
    lager:info([{hello_request, api}, {hello_handler, CallbackModule}],
		"~p ~s - ~s - ~s", [Handler, Endpoint, fmt_request(Request), fmt_response(Response)]).

bad_request(CallbackModule, Handler, ExUriUrl, Request, Response) ->
    Endpoint = list_to_binary(ex_uri:encode(ExUriUrl)),
    lager:info([{hello_request, error}, {hello_handler, CallbackModule}],
		"~p ~s - ~s - ~s", [Handler, Endpoint, fmt_request(Request), fmt_response(Response)]).

%% --------------------------------------------------------------------------------
%% -- helpers
fmt_request(undefined) ->
    [];
fmt_request(BinaryRequest) when is_binary(BinaryRequest) ->
    escape_badreq(BinaryRequest);
fmt_request(#jsonrpc_request{reqid = undefined, method = Method, params = Params}) ->
    ["{\"method\":", jsx:encode(Method),
     ",\"params\":", jsx:encode(Params), "}"];
fmt_request(#jsonrpc_request{reqid = ReqId, method = Method, params = Params}) ->
    ["{\"id\":", jsx:encode(ReqId),
     ",\"method\":", jsx:encode(Method),
     ",\"params\":", jsx:encode(Params), "}"];
fmt_request(Batch) when is_list(Batch) ->
    ["[", string:join([fmt_request(Req) || Req <- Batch], ","), "]"].

fmt_response(ignore) ->
    ["ignored"];
fmt_response(#jsonrpc_response{reqid = ReqId, error = undefined, result = Result}) ->
    ["{\"id\":", jsx:encode(ReqId),
     ",\"result\":", jsx:encode(Result), "}"];
fmt_response(#jsonrpc_response{reqid = ReqId, error = Error}) ->
    ["{\"id\":", jsx:encode(ReqId),
     ",\"error\":", fmt_response(Error), "}"];
fmt_response(#jsonrpc_error{code = Code, message = Message, data = Data}) ->
    ["{",
     maybe_undefined(<<"\"code\":">>, Code),
     maybe_undefined(<<",\"message\":">>, Message),
     maybe_undefined(<<",\"data\": ">>, Data), "}"];
fmt_response(Batch) when is_list(Batch) ->
    ["[", string:join([fmt_response(Resp) || Resp <- Batch], ","), "]"].

-compile([{inline, maybe_undefined/2}]).
maybe_undefined(_Key, undefined) -> [];
maybe_undefined(Key, Value) -> [Key, jsx:encode(Value)].

-compile([{inline, escape_badreq/1}]).
escape_badreq(Message) ->
    [ escape_byte(Byte) || <<Byte>> <= Message ].

-compile([{inline, escape_byte/1}]).


escape_byte(Byte) when Byte < 32; Byte > 126 ->
    io_lib:format("\\x~2.16.0b", [Byte]);
escape_byte($\\) ->
    "\\\\";
escape_byte($") ->
    "\\\"";
escape_byte(Byte) ->
    Byte.