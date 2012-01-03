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

%% @doc This module contains a simple JSON-RPC client.
%%   The client defined in this module should <b>only</b> be used
%%   for testing and debugging purposes, not in actual production code.
-module(hello_simple_client).
-export([call/3, notification/3, call_np/3]).
-export_type([url/0, method/0, rpc_error/0]).

-include("internal.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-define(TIMEOUT, {timeout, 15000}).

%% --------------------------------------------------------------------------------
%% -- Client API
-type url() :: string().
-type method() :: binary() | string() | atom().
-type rpc_error() :: {error, {http, term()}} | {error, syntax_error}
                     | {error, invalid_request} | {error, method_not_found} | {error, invalid_params}
                     | {error, internal_error} | {error, internal_error} | {error, integer()}.

%% @doc Perform a JSON-RPC call.
-spec call(url(), method(), [hello_json:value()]) -> {ok, hello_json:value()} | {error, rpc_error()}.
call(Server, Method, ArgList) ->
    call(hello_proto_jsonrpc, Server, Method, ArgList).

%% @doc Perform an RPC method call.
-spec call(module(), url(), method(), [hello_json:value()]) -> {ok, hello_json:value()} | {error, rpc_error()}.
call(Protocol, Server, Method, ArgList) when is_list(ArgList) or is_tuple(ArgList) ->
    Request = hello_proto:request(Protocol, into_bin(Method), ArgList),
    case rpc_request(Server, Request) of
        {error, Error} ->
            {error, Error};
        {ok, Body} ->
            case hello_proto:decode(Request, Body) of
                #response{result = Result} ->
                    {ok, Result};
                Error = #error{} ->
                    {error, hello_proto:error_desc(Error)}
            end
    end.

%% @doc Performs a JSON-RPC method call with named parameters (property list).
-spec call_np(url(), method(), [{string(), hello_json:value()}]) -> {ok, hello_json:value()} | {error, rpc_error()}.
call_np(Server, Method, ArgProps) when is_list(ArgProps) ->
    call_np(hello_proto_jsonrpc, Server, Method, ArgProps).

-spec call_np(module(), url(), method(), [{string(), hello_json:value()}]) -> {ok, hello_json:value()} | {error, rpc_error()}.
call_np(Protocol, Server, Method, ArgProps) when is_list(ArgProps) ->
    call(Protocol, Server, Method, {ArgProps}).

%% @doc Special form of an RPC call that returns no result.
-spec notification(url(), method(), [hello_json:value()]) -> ok | {error, rpc_error()}.
notification(Server, Method, ArgList) ->
    notification(hello_proto_jsonrpc, Server, Method, ArgList).

-spec notification(module(), url(), method(), [hello_json:value()]) -> ok | {error, rpc_error()}.
notification(Protocol, Server, Method, ArgList) ->
    case rpc_request(Server, hello_proto:notification(Protocol, Method, ArgList)) of
        {error, Reason} -> {error, Reason};
        {ok, _Body}     -> ok
    end.

%% --------------------------------------------------------------------------------
%% -- Helper functions
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

rpc_request_zmq(URI, Request) ->
    EncRequest = hello_proto:encode(Request),
    {ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, req),
    ok = erlzmq:connect(Socket, ex_uri:encode(URI)),
    erlzmq:send(Socket, EncRequest),
    case hello_proto:is_notification(Request) of
        true  -> Resp = {ok, undefined};
        false -> Resp = erlzmq:recv(Socket)
    end,
    erlzmq:close(Socket),
    erlzmq:term(Context),
    Resp.

rpc_request_http(URI, Request) ->
    HostURL = ex_uri:encode(URI),
    EncRequest = hello_proto:encode(Request),
    {ok, Hostname, _Path} = split_url(HostURL),
    {ok, Vsn} = application:get_key(hello, vsn),
    MimeType = hello_proto:mime_type(Request),
    Headers = [{"Host", Hostname},
               {"Connection", "close"},
               {"Content-Length", integer_to_list(iolist_size(EncRequest))},
               {"Content-Type", MimeType},
               {"Accept", MimeType},
               {"User-Agent", "hello/" ++ Vsn}],
    HTTPRequest = {HostURL, Headers, MimeType, EncRequest},
    case httpc:request(post, HTTPRequest, [?TIMEOUT], [{headers_as_is, true}, {full_result, false}]) of
       {ok, {_Code, Body}} -> {ok, Body};
       {error, Reason}     -> {error, {http, Reason}}
    end.

into_bin(Bin) when is_list(Bin) -> list_to_binary(Bin);
into_bin(Bin)                   -> Bin.

split_url(URL) ->
    case re:run(URL, "^http[s]?://([^/]+)(.*)", [{capture, all_but_first, list}]) of
        {match, [Host, Path]} -> {ok, Host, Path};
        nomatch               -> {error, not_url}
    end.
