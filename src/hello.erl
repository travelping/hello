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
-export([start/0, run_stateless_request/2, run_stateless_binary_request/2,
         call/3, notification/3, call_np/3]).
-export([bind_stateless/2]).

-include("internal.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-define(TIME_OUT, {timeout, 15000}).
-define(HTTPD, hello_httpd).

%% @type binary_or_string() = binary() | string()
%% @type rpc_error() = {error, {http, term()}} | {error, syntax_error}
%%                   | {error, invalid_request} | {error, method_not_found} | {error, invalid_params}
%%                   | {error, internal_error} | {error, internal_error} | {error, integer()}

%% @doc Starts the application and all dependencies.
%% This is useful for debugging purposes.
start() ->
    application:start(cowboy),
    application:start(inets),
    application:start(ex_uri),
    application:start(hello).

start(_Type, _StartArgs) ->
    ets:new(?HANDLER_TAB, [public, named_table, {read_concurrency, true}]),

    RequestLog = case application:get_env(hello, request_log_enabled) of
                     {ok, true} ->
                        {ok, RequestLogFile} = application:get_env(hello, request_log_file),
                        {ok, Log} = hello_logger:open(RequestLogFile),
                        Log;
                     {ok, false} ->
                        undefined
                 end,

    {ok, self(), RequestLog}.

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

rpc_request(HostURL, Request) ->
    case (catch ex_uri:decode(HostURL)) of
        {ok, Rec = #ex_uri{}, _} ->
            rpc_request_scheme(Rec, Request);
        _Other ->
            error(badurl)
    end.

rpc_request_scheme(URI = #ex_uri{scheme = "http"}, Request) ->
    rpc_request_http((URI), Request);
rpc_request_scheme(URI = #ex_uri{scheme = "https"}, Request) ->
    rpc_request_http(URI, Request);
rpc_request_scheme(URI = #ex_uri{scheme = "zmq-tcp"}, Request) ->
    rpc_request_zmq(URI#ex_uri{scheme = "tcp"}, Request);
rpc_request_scheme(URI = #ex_uri{scheme = "zmq-ipc"}, Request) ->
    rpc_request_zmq(URI#ex_uri{scheme = "ipc"}, Request).

rpc_request_zmq(URI, Request = #request{id = ReqID}) ->
    RequestJSON = encode_request(Request),
    {ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, req),
    ok = erlzmq:connect(Socket, ex_uri:encode(URI)),
    erlzmq:send(Socket, RequestJSON),
    case ReqID of
        undefined -> Resp = {ok, undefined};
        _         -> Resp = erlzmq:recv(Socket)
    end,
    erlzmq:close(Socket),
    erlzmq:term(Context),
    Resp.

rpc_request_http(URI, Request) ->
    HostURL     = ex_uri:encode(URI),
    RequestJSON = encode_request(Request),
    {ok, Hostname, _Path} = split_url(HostURL),
    {ok, Vsn}   = application:get_key(hello, vsn),
    Headers     = [{"Host", Hostname}, {"Connection", "keep-alive"},
                   {"Content-Length", integer_to_list(iolist_size(RequestJSON))},
                   {"Content-Type", "application/json"},
                   {"Accept", "application/json"},
                   {"User-Agent", "hello/" ++ Vsn}],
    HTTPRequest = {HostURL, Headers, "application/json", RequestJSON},
    case httpc:request(post, HTTPRequest, [?TIME_OUT], [{headers_as_is, true}, {full_result, false}]) of
       {ok, {_Code, Body}} -> {ok, Body};
       {error, Reason}     -> {error, {http, Reason}}
    end.

encode_request(#request{id = Id, method = Method, params = ArgList}) ->
    Methodto    = into_bin(Method),
    IDField     = case Id of
                      undefined -> [];
                      _         -> [{"id", Id}]
                  end,
    hello_json:encode({IDField ++ [{"jsonrpc", <<"2.0">>}, {"method", Methodto}, {"params", ArgList}]}).

%% @spec (Host::string(), Method::binary_or_string(), Arguments::list()) -> {ok, hello_json:json()} | {error, rpc_error()}
%% @doc Function performs a JSON-RPC method call using HTTP.
call(Host, Method, ArgList) when is_list(ArgList) or is_tuple(ArgList) ->
    Request = #request{id = 1, method = Method, params = ArgList},
    case rpc_request(Host, Request) of
        {error, Error} -> {error, Error};
        {ok, Body} ->
            case hello_json:decode(Body) of
               {error, syntax_error} -> {error, syntax_error};
               {ok, {Props}, _Rest} ->
                   case proplists:get_value("error", Props, null) of
                       null ->
                           Result = proplists:get_value("result", Props),
                           {ok, Result};
                       {ErrorObject} ->
                           case proplists:get_value("code", ErrorObject) of
                               -32600 -> {error, invalid_request};
                               -32601 -> {error, method_not_found};
                               -32602 -> {error, invalid_params};
                               -32603 -> {error, internal_error};
                               Code when (Code >= -32099) and (Code =< -32000) -> {error, server_error};
                               Code -> {error, Code}
                           end
                   end
            end
    end.

%% @spec (Host::string(), Method::binary_or_string(), Arguments::list()) -> ok | {error, rpc_error()}
%% @doc Special form of a JSON-RPC method call that returns no result.
notification(Host, Method, ArgList) ->
    case rpc_request(Host, #request{method=Method, params=ArgList}) of
        {error, Reason} -> {error, Reason};
        {ok, _Body}     -> ok
    end.

%% @spec (Host::string(), Method::binary_or_string(), Arguments::[{string(), hello_json:json()}]) -> {ok, hello_json:json()} | {error, rpc_error()}
%% @doc Performs a JSON-RPC method call with named parameters (property list).
call_np(Host, Method, List) when is_list(List) ->
    call(Host, Method, {List}).

into_bin(Bin) when is_list(Bin) -> list_to_binary(Bin);
into_bin(Bin)                   -> Bin.

split_url(URL) ->
    case re:run(URL, "^http://([^/]+)(.*)", [{capture, all_but_first, list}]) of
        {match, [Host, Path]} -> {ok, Host, Path};
        nomatch               -> {error, not_url}
    end.
