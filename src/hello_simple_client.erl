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

%% @doc Perform a JSON-RPC method call.
-spec call(url(), method(), [hello_json:value()]) -> {ok, hello_json:value()} | {error, rpc_error()}.
call(Server, Method, ArgList) when is_list(ArgList) or is_tuple(ArgList) ->
    Request = #request{id = 1, method = Method, params = ArgList},
    case rpc_request(Server, Request) of
        {error, Error} -> {error, Error};
        {ok, Body} ->
            case hello_json:decode(Body) of
               {error, syntax_error} -> {error, syntax_error};
               {ok, {Props}, _Rest} ->
                   case proplists:get_value(<<"error">>, Props, null) of
                       null ->
                           Result = proplists:get_value(<<"result">>, Props),
                           {ok, Result};
                       {ErrorObject} ->
                           case proplists:get_value(<<"code">>, ErrorObject) of
                               -32600 -> {error, invalid_request};
                               -32601 -> {error, method_not_found};
                               -32602 -> {error, invalid_params};
                               -32603 -> {error, internal_error};
                               Code when (Code >= -32099) and (Code =< -32000) -> {error, server_error};
                               Code -> {error, {Code, proplists:get_value("message", ErrorObject)}}
                           end
                   end
            end
    end.

%% @doc Performs a JSON-RPC method call with named parameters (property list).
-spec call_np(url(), method(), [{string(), hello_json:value()}]) -> {ok, hello_json:value()} | {error, rpc_error()}.
call_np(Server, Method, ArgProps) when is_list(ArgProps) ->
    call(Server, Method, {ArgProps}).

%% @doc Special form of a JSON-RPC method call that returns no result.
-spec notification(url(), method(), [hello_json:value()]) -> ok | {error, rpc_error()}.
notification(Server, Method, ArgList) ->
    case rpc_request(Server, #request{method=Method, params=ArgList}) of
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
    case httpc:request(post, HTTPRequest, [?TIMEOUT], [{headers_as_is, true}, {full_result, false}]) of
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

into_bin(Bin) when is_list(Bin) -> list_to_binary(Bin);
into_bin(Bin)                   -> Bin.

split_url(URL) ->
    case re:run(URL, "^http[s]?://([^/]+)(.*)", [{capture, all_but_first, list}]) of
        {match, [Host, Path]} -> {ok, Host, Path};
        nomatch               -> {error, not_url}
    end.
