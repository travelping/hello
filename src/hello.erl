% Copyright 2010-2011, Travelping GmbH <info@travelping.com>

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

% @doc This module is the main interface to the hello application.
-module(hello).
-behaviour(application).
-export([start/2, stop/1]).
-export([start/0, run_stateless_request/2, run_stateless_binary_request/2]).
-export([bind_stateless/2]).

-include("internal.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-define(HTTPD, hello_httpd).

%% @doc Starts the application and all dependencies.
%% This is useful for debugging purposes.
start() ->
    application:start(cowboy),
    application:start(inets),
    application:start(ex_uri),
    application:start(gproc),
    application:start(hello).

start(_Type, _StartArgs) ->
    ets:new(?HANDLER_TAB, [public, named_table, {read_concurrency, true}]),
    {ok, Supervisor} = hello_stateless_zmq_supervisor:start_link(),
    RequestLog = case application:get_env(hello, request_log_enabled) of
                     {ok, true} ->
                         {ok, RequestLogFile} = application:get_env(hello, request_log_file),
                         {ok, Log} = hello_logger:open(RequestLogFile),
                         Log;
                     {ok, false} ->
                         undefined
                 end,
    {ok, Supervisor, RequestLog}.

stop(undefined) ->
    hello_httpd:stop(?HTTPD),
    ok;
stop(Log) ->
    hello_httpd:stop(?HTTPD),
    hello_logger:close(Log),
    ok.

%% @doc Start a stateless RPC server on the given URL.
-type url() :: string().
-type urn() :: string().
-spec bind_stateless(url() | urn(), module()) -> ok | {error, already_bound} | {error, occupied}.
bind_stateless("urn:" ++ _, _Module) ->
    error(badurl);
bind_stateless(URL, CallbackModule) ->
    case (catch ex_uri:decode(URL)) of
        {ok, Rec = #ex_uri{}, _} ->
            bind_stateless_uri(Rec, CallbackModule);
        _Other ->
            error(badurl)
    end.

bind_stateless_uri(#ex_uri{scheme = "http", path = Path, authority = #ex_uri_authority{host = Host, port = Port}}, Mod) ->
    hello_httpd:start("http", Host, Port, Path, Mod);
bind_stateless_uri(URL = #ex_uri{scheme = "zmq-tcp"}, Mod) ->
    hello_stateless_zmq_supervisor:start_listener(URL#ex_uri{scheme = "tcp"}, Mod);
bind_stateless_uri(URL = #ex_uri{scheme = "zmq-ipc"}, Mod) ->
    hello_stateless_zmq_supervisor:start_listener(URL#ex_uri{scheme = "ipc"}, Mod);
bind_stateless_uri(_, _Mod) ->
    exit(badprotocol).

%% @doc Run a single not-yet-decoded JSON-RPC request against the given callback module.
%%   This can be used for testing, but please note that the request must be
%%   given as an encoded binary. It's better to use {@link run_stateless_request/2} for that.
%%   At the moment, this will also write the request to the log.
-spec run_stateless_binary_request(module(), binary()) -> binary().
run_stateless_binary_request(CallbackModule, JSON) ->
    case hello_proto:request_json(JSON) of
        {ok, RequestRec} ->
            Response = hello_stateless_server:run_request(CallbackModule, RequestRec);
        {batch, Valid, Invalid} ->
            HandledResps = hello_stateless_server:run_request(CallbackModule, Valid),
            Response = Invalid ++ HandledResps;
        {error, Error} ->
            Response = Error
    end,
    ResponseJSON = hello_proto:response_json(Response),
    hello_logger:log(JSON, ResponseJSON),
    ResponseJSON.

%% @doc Run a single JSON-RPC request against the given callback module.
%%   Use this function to test your stateless servers.
%%   Please note that the request is <b>not</b> logged.
-spec run_stateless_request(module(), hello_json:value()) -> hello_json:value().
run_stateless_request(CallbackModule, DecodedRequest) ->
    case hello_proto:request(DecodedRequest) of
        {ok, RequestRec} ->
            hello_stateless_server:run_request(CallbackModule, RequestRec);
        {batch, Valid, Invalid} ->
            Resps = hello_stateless_server:run_request(CallbackModule, Valid),
            Invalid ++ Resps;
        {error, Error} ->
            Error
    end.
