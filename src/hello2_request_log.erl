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
-module(hello2_request_log).
-export([open/2, close/1, open_bad_requests/1, close_bad_requests/0, request/5, bad_request/5]).

-include("internal.hrl").

%% --------------------------------------------------------------------------------
%% -- API
-spec open(module(), pid()) -> ok | {error, term()}.
open(CallbackModule, Owner) ->
    Name         = reg_key(CallbackModule),
    {ok, LogDir} = application:get_env(hello2, request_log_dir),
    File         = filename:join(LogDir, atom_to_list(CallbackModule) ++ ".log"),
    LogOptions   = [{name, Name}, {linkto, Owner}, {file, File}, {format, external}, {type, halt}],

    %% disk_log does reference counting internally, so we can open the log each time.
    %% the log will be closed when the last endpoint terminates.
    case disk_log:open(LogOptions) of
        {ok, _Log} ->
            ok;
        {repaired, _Log, _Recovered, _Badbytes} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.

open_bad_requests(Owner) ->
    open(bad_requests, Owner).

-spec close(module()) -> ok | {error, no_such_log}.
close(CallbackModule) ->
    disk_log:close(reg_key(CallbackModule)).

close_bad_requests() ->
    close(bad_requests).

-spec request(module(), atom() | pid(), binary(), hello2_proto:request(), hello2_proto:response()) ->
        ok | {error, no_such_log}.
request(CallbackModule, Handler, Endpoint, Request, Response) ->
    Date = cowboy_clock:rfc1123(),
    Msg = <<Date/binary, " ", (fmt_handler(Handler))/binary, " ", Endpoint/binary, "\n",
            (fmt_request(Request))/binary, (fmt_response(Response))/binary, "\n">>,
    disk_log:blog(reg_key(CallbackModule), Msg).

bad_request(CallbackModule, Handler, Endpoint, Message, Response) ->
    Date = cowboy_clock:rfc1123(),
    Msg = <<Date/binary,
            " ", (atom_to_binary(CallbackModule, latin1))/binary,
            " ", (fmt_handler(Handler))/binary,
            " ", Endpoint/binary, "\n",
            (escape_badreq(Message))/binary, "\n",
            (fmt_response(Response))/binary>>,
    disk_log:blog(reg_key(bad_requests), Msg).

%% --------------------------------------------------------------------------------
%% -- helpers
-compile({inline,reg_key/1}).
reg_key(Module) ->
    {hello2_request_log, Module}.

fmt_handler(Pid) ->
    list_to_binary(pid_to_list(Pid)).

fmt_request(#request{reqid = undefined, method = Method, params = Params}) ->
    <<"{\"method\":", (hello2_json:encode(Method))/binary,
      ",\"params\":", (hello2_json:encode(Params))/binary, "}\n">>;
fmt_request(#request{reqid = ReqId, method = Method, params = Params}) ->
    <<"{\"id\":", (hello2_json:encode(ReqId))/binary,
      ",\"method\":", (hello2_json:encode(Method))/binary,
      ",\"params\":", (hello2_json:encode(Params))/binary, "}\n">>;
fmt_request(#batch_request{requests = Requests}) ->
    lists:foldl(fun (Req, Acc) ->
                        <<Acc/binary, "\t", (fmt_request(Req))/binary>>
                end, <<"Batch Request:\n">>, Requests).

fmt_response(ignore) ->
    <<>>;
fmt_response(#response{reqid = ReqId, result = Result}) ->
    <<"{\"id\":", (hello2_json:encode(ReqId))/binary,
      ",\"result\":", (hello2_json:encode(Result))/binary, "}\n">>;
fmt_response(#error{reqid = ReqId, code = Code, message = Message, data = Data}) ->
    <<"{\"id\":", (hello2_json:encode(ReqId))/binary,
      (maybe_undefined(<<",\"code\":">>, Code))/binary,
      (maybe_undefined(<<",\"message\":">>, Message))/binary,
      (maybe_undefined(<<",\"data\": ">>, Data))/binary, "}\n">>;
fmt_response(#batch_response{responses = Responses}) ->
    lists:foldl(fun (Resp, Acc) ->
                        <<Acc/binary, "\t", (fmt_response(Resp))/binary>>
                end, <<"Batch Response:\n">>, Responses).

-compile([{inline, maybe_undefined/2}]).
maybe_undefined(_Key, undefined) -> <<>>;
maybe_undefined(Key, Value) -> <<Key/binary, (hello2_json:encode(Value))/binary>>.

-compile([{inline, escape_badreq/1}]).
escape_badreq(Message) ->
    << <<(escape_byte(Byte))/binary>> || <<Byte>> <= Message >>.

-compile([{inline, escape_byte/1}]).
escape_byte(Byte) when Byte < 16 ->
    <<"\\x0", (<< <<Chr:8>> || Chr <- integer_to_list(Byte, 16) >>)/binary>>;
escape_byte(Byte) when Byte < 32; Byte > 126 ->
    <<"\\x", (<< <<Chr:8>> || Chr <- integer_to_list(Byte, 16) >>)/binary>>;
escape_byte($\\) ->
    <<"\\\\">>;
escape_byte($") ->
    <<"\\\"">>;
escape_byte(Byte) ->
    <<Byte>>.
