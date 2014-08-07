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
-module(hello_request_log).
-export([request/5, bad_request/5]).

-include("internal.hrl").

%% --------------------------------------------------------------------------------
%% -- API
-spec request(module(), atom() | pid(), binary(), hello_proto:request(), hello_proto:response()) ->
        ok | {error, no_such_log}.
request(CallbackModule, Handler, Endpoint, Request, Response) ->
    lager:info([{hello_request, api}, {hello_handler, CallbackModule}],
		"~p ~s - ~s - ~s", [Handler, Endpoint, fmt_request(Request), fmt_response(Response)]).

bad_request(CallbackModule, Handler, Endpoint, Message, Response) ->
    lager:info([{hello_request, error}, {hello_handler, CallbackModule}],
		"~p ~s - ~s - ~s", [Handler, Endpoint, escape_badreq(Message), fmt_response(Response)]).

%% --------------------------------------------------------------------------------
%% -- helpers
fmt_request(#request{reqid = undefined, method = Method, params = Params}) ->
    ["{\"method\":", hello_json:encode(Method),
     ",\"params\":", hello_json:encode(Params), "}"];
fmt_request(#request{reqid = ReqId, method = Method, params = Params}) ->
    ["{\"id\":", hello_json:encode(ReqId),
     ",\"method\":", hello_json:encode(Method),
     ",\"params\":", hello_json:encode(Params), "}"];
fmt_request(#batch_request{requests = Requests}) ->
    ["[", string:join([fmt_request(Req) || Req <- Requests], ","), "]"].

fmt_response(ignore) ->
    ["ignored"];
fmt_response(#response{reqid = ReqId, result = Result}) ->
    ["{\"id\":", hello_json:encode(ReqId),
     ",\"result\":", hello_json:encode(Result), "}"];
fmt_response(#error{reqid = ReqId, code = Code, message = Message, data = Data}) ->
    ["{\"id\":", hello_json:encode(ReqId),
     maybe_undefined(<<",\"code\":">>, Code),
     maybe_undefined(<<",\"message\":">>, Message),
     maybe_undefined(<<",\"data\": ">>, Data), "}"];
fmt_response(#batch_response{responses = Responses}) ->
    ["[", string:join([fmt_response(Resp) || Resp <- Responses], ","), "]"].

-compile([{inline, maybe_undefined/2}]).
maybe_undefined(_Key, undefined) -> [];
maybe_undefined(Key, Value) -> [Key, hello_json:encode(Value)].

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
