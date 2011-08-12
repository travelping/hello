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
-export([bind_stateful/3, bind_stateless/2, bindings/0]).

-include("internal.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").

% @doc Starts the application and all dependencies.
% This is useful for debugging purposes.
start() ->
    application:start(cowboy),
    application:start(inets),
    application:start(ex_uri),
    application:start(erlzmq),
    application:start(hello).

start(_Type, _StartArgs) ->
    %% create the log dir
    {ok, LogDir} = application:get_env(hello, request_log_dir),
    ok           = filelib:ensure_dir(filename:join(LogDir, ".")),

    {ok, Supervisor} = hello_supervisor:start_link(),

    case application:get_env(hello, status_ipc) of
        {ok, StatusIPC} when is_list(StatusIPC) ->
            ok = bind_stateless("zmq-ipc://" ++ StatusIPC, hello_status);
        _ ->
            error({option_value, status_ipc})
    end,

    {ok, Supervisor, undefined}.

stop(_) ->
    case application:get_env(hello, status_ipc) of
        {ok, StatusIPC} when is_list(StatusIPC) ->
            file:delete(StatusIPC);
        _ ->
            ok
    end.

bind_stateful(URL, CallbackModule, Args) ->
    case (catch ex_uri:decode(URL)) of
        {ok, Rec = #ex_uri{}, _} ->
            bind_stateful_uri(Rec, CallbackModule, Args);
        _Other ->
            error(badurl)
    end.

bind_stateful_uri(#ex_uri{scheme = "http", path = Path, authority = #ex_uri_authority{host = Host, port = Port}}, Mod, _Args) ->
    error(notsup);
bind_stateful_uri(URL = #ex_uri{scheme = "zmq-tcp"}, Mod, Args) ->
    hello_stateful_zmq_server:start_supervised(URL#ex_uri{scheme = "tcp"}, Mod, Args);
bind_stateful_uri(URL = #ex_uri{scheme = "zmq-ipc"}, Mod, Args) ->
    hello_stateful_zmq_server:start_supervised(URL#ex_uri{scheme = "ipc"}, Mod, Args);
bind_stateful_uri(_, _Mod, _Args) ->
    error(badurl).

% @doc Starts a stateless RPC server on the given URL.
%   The transport implementation that is chosen depends on
%   the protocol field of the URL. The available transports are documented
%   below:
%
%    <table border="1">
%      <thead><tr><td width="140"><b>URL scheme</b></td><td><b>Transport</b></td><td><b>Notes</b></td></tr></thead>
%      <tbody>
%        <tr><td>http://...</td><td>HTTP</td><td>Multiple services can be bound to the same host/port
%                                                combination, as long as the path is different.</td></tr>
%        <tr><td>zmq-tcp://...</td><td>ZeroMQ over TCP</td><td>The port <em>must</em> be specified.</td></tr>
%        <tr><td>zmq-ipc://...</td><td>ZeroMQ over Unix Sockets</td><td>The path can be either absolute or relative.</td></tr>
%      </tbody>
%    </table>
%   <br/>
%   The function returns:
%   <ul>
%     <li>``ok'' when the server has been bound successfully</li>
%     <li>``{error, already_started}'' when the same module is already bound to the given URL</li>
%     <li>``{error, occupied}'' when a different module is already bound to the given URL</li>
%     <li>``{error, {transport, term()}}'' when there was an error starting the server</li>
%   </ul>
%
%   The function will <b>exit</b>:
%   <ul>
%     <li>with reason ``badurl'' when the URL is malformed</li>
%     <li>with reason ``badprotocol'' when the URL uses an unknown scheme</li>
%   </ul>
-type url() :: string().
-spec bind_stateless(url(), module()) -> ok | {error, already_started} | {error, occupied} | {error, {transport, term()}}.
bind_stateless(URL, CallbackModule) ->
    case (catch ex_uri:decode(URL)) of
        {ok, Rec = #ex_uri{}, _} ->
            bind_stateless_uri(Rec, CallbackModule);
        _Other ->
            error(badurl)
    end.

bind_stateless_uri(#ex_uri{scheme = "http", path = Path, authority = #ex_uri_authority{host = Host, port = Port}}, Mod) ->
    hello_stateless_httpd:start("http", Host, Port, Path, Mod);
bind_stateless_uri(URL = #ex_uri{scheme = "zmq-tcp"}, Mod) ->
    hello_stateless_zmq_server:start_supervised(URL#ex_uri{scheme = "tcp"}, Mod);
bind_stateless_uri(URL = #ex_uri{scheme = "zmq-ipc"}, Mod) ->
    hello_stateless_zmq_server:start_supervised(URL#ex_uri{scheme = "ipc"}, Mod);
bind_stateless_uri(_, _Mod) ->
    error(badurl).

% @doc Return the list of bound modules.
-spec bindings() -> [{url(), module()}].
bindings() ->
    [{URL, Mod} || {_Protocol, URL, _Pid, Mod} <- hello_registry:bindings()].

% @doc Run a single not-yet-decoded JSON-RPC request against the given callback module.
%   This can be used for testing, but please note that the request must be
%   given as an encoded binary. It's better to use {@link run_stateless_request/2} for testing.
%
%   The request is <b>not</b> logged.
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
    hello_proto:response_json(Response).

% @doc Run a single JSON-RPC request against the given callback module.
%   This function allows you to test your stateless servers without binding
%   to any URL. The request argument should be a complete JSON-RPC request in
%   Erlang data form.
%
%   The request is <b>not</b> logged.
-spec run_stateless_request(module(), hello_json:value()) -> hello_json:value().
run_stateless_request(CallbackModule, Request) ->
    case hello_proto:request(Request) of
        {ok, RequestRec} ->
            hello_stateless_server:run_request(CallbackModule, RequestRec);
        {batch, Valid, Invalid} ->
            Resps = hello_stateless_server:run_request(CallbackModule, Valid),
            Invalid ++ Resps;
        {error, Error} ->
            Error
    end.
