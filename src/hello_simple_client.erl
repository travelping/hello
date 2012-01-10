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

%% @doc This module contains a simple RPC client.
%%   The client defined in this module should <b>only</b> be used
%%   for testing and debugging purposes, not in actual production code.
-module(hello_simple_client).
-export([call/3, notification/3, call_np/3, batch_call/2]).

-include("internal.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-define(TIMEOUT, {timeout, 15000}).

%% --------------------------------------------------------------------------------
%% -- Client API

%% @doc Perform an RPC method call.
-spec call(hello:url(), hello_client:method(), [hello_json:value()]) ->
        {ok, hello_json:value()} | {error, hello_client:rpc_error()}.
call(Server, Method, ArgList) ->
    call(hello_proto_jsonrpc, Server, Method, ArgList).

%% @doc Perform an RPC method call.
-spec call(module(), hello:url(), hello_client:method(), [hello_json:value()]) ->
        {ok, hello_json:value()} | {error, hello_client:rpc_error()}.
call(Protocol, Server, Method, ArgList) when is_list(ArgList) or is_tuple(ArgList) ->
    with_client(Protocol, Server, fun (C) -> hello_client:call(C, Method, ArgList) end).

%% @doc Performs an RPC method call with named parameters (property list).
-spec call_np(hello:url(), hello_client:method(), [{string(), hello_json:value()}]) ->
        {ok, hello_json:value()} | {error, hello_client:rpc_error()}.
call_np(Server, Method, ArgProps) when is_list(ArgProps) ->
    call_np(hello_proto_jsonrpc, Server, Method, ArgProps).

-spec call_np(module(), hello:url(), hello_client:method(), [{string(), hello_json:value()}]) ->
        {ok, hello_json:value()} | {error, hello_client:rpc_error()}.
call_np(Protocol, Server, Method, ArgProps) when is_list(ArgProps) ->
    with_client(Protocol, Server, fun (C) -> hello_client:call_np(C, Method, ArgProps) end).

%% @doc Special form of an RPC call that returns no result.
-spec notification(hello:url(), hello_client:method(), [hello_json:value()]) ->
        ok | {error, hello_client:rpc_error()}.
notification(Server, Method, ArgList) ->
    notification(hello_proto_jsonrpc, Server, Method, ArgList).

-spec notification(module(), hello:url(), hello_client:method(), [hello_json:value()]) ->
        ok | {error, hello_client:rpc_error()}.
notification(Protocol, Server, Method, ArgList) ->
    with_client(Protocol, Server, fun (C) -> hello_client:notification(C, Method, ArgList) end).

-spec batch_call(hello:url(), [{hello_client:method(), hello_json:json_array() | hello_json:json_object()}]) ->
    [{ok, hello_json:value()} | {error, hello_client:rpc_error()}].
batch_call(Server, Batch) ->
    with_client(hello_proto_jsonrpc, Server, fun (C) -> hello_client:batch_call(C, Batch) end).

%% --------------------------------------------------------------------------------
%% -- Helper functions
with_client(Protocol, URL, Function) ->
    case hello_client:start(URL, [{protocol, Protocol}]) of
        {ok, Client} ->
            try
                Function(Client)
            after
                hello_client:stop(Client)
            end;
        {error, Error} ->
            {error, Error}
    end.
