% Copyright 2010-2012, Travelping GmbH <info@travelping.com>

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

% @doc This module is the main interface to the hello2 application.
-module(hello2).
-behaviour(application).
-export([start/2, stop/1]).
-export([start/0, run_stateless_binary_request/3, run_stateless_binary_request/4]).
-export([bind_stateful/3, bind_stateless/2, bind_stateless/3, bindings/0]).
-export_type([url/0, decoded_url/0, transport_params/0]).

-include("internal.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-type url() :: string().
-type decoded_url() :: #ex_uri{}.

% @doc Starts the application and all dependencies.
% This is useful for debugging purposes.
start() ->
    application:start(sasl),
    application:start(cowboy),
    application:start(ex_uri),
    application:start(ibrowse),
    application:start(erlzmq),
    application:start(hello2).

start(_Type, _StartArgs) ->
    %% create the log dir
    {ok, LogDir} = application:get_env(hello2, request_log_dir),
    ok = filelib:ensure_dir(filename:join(LogDir, ".")),
    {ok, Supervisor} = hello2_supervisor:start_link(),
    ok = hello2_request_log:open_bad_requests(Supervisor),
    {ok, Supervisor, undefined}.

stop(_) ->
    ok.

bind_stateful(URL, CallbackModule, Args) ->
    bind_uri(stateful, URL, CallbackModule, Args).

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
-spec bind_stateless(url(), module()) -> ok | {error, already_started} | {error, occupied} | {error, {transport, term()}}.
bind_stateless(URL, CallbackModule) ->
    bind_stateless(URL, CallbackModule, []).

bind_stateless(URL, CallbackModule, Args) ->
    bind_uri(stateless, URL, CallbackModule, Args).

bind_uri(Type, URL, CallbackModule, Args) ->
    case (catch ex_uri:decode(URL)) of
        {ok, Rec = #ex_uri{scheme = Scheme}, _} ->
            ListenerMod = binding_module(Scheme),
            case hello2_binding_supervisor:start_binding(ListenerMod, Rec, CallbackModule, Type, Args) of
                {ok, _Pid}     -> ok;
                {error, Error} -> {error, Error}
            end;
        Other ->
            error({badurl, URL, Other})
    end.

binding_module("zmq-tcp") -> hello2_zmq_listener;
binding_module("zmq-ipc") -> hello2_zmq_listener;
binding_module("http")    -> hello2_http_listener;
binding_module("sockjs")  -> hello2_sockjs_listener;
binding_module(_Scheme)   -> error(notsup).

% @doc Return the list of bound modules.
-spec bindings() -> [{url(), module()}].
bindings() ->
    Bindings0 = [{ex_uri:encode(Binding#binding.url), dict:to_list(Binding#binding.callbacks)} || Binding <- hello2_registry:bindings()],
    lists:foldl(
        fun({Url, Callbacks}, Bindings1) ->
                Bindings1 ++ [{Url, Cb#callback.mod} || {_, [Cb]} <- Callbacks]
        end, [], Bindings0).

-type transport_params() :: [{atom(), any()}].
% @doc Run a single not-yet-decoded RPC request against the given callback module.
%   The request must be given as an encoded binary.
%   The values in ``TransportContextParams'' is inserted into the context of the request.
%
%   The request is <b>not</b> logged.
-spec run_stateless_binary_request(module(), binary(), transport_params()) -> binary().
run_stateless_binary_request(CallbackModule, Message, TransportContextParams) ->
    run_stateless_binary_request(hello2_proto_jsonrpc, CallbackModule, Message, TransportContextParams).

run_stateless_binary_request(Protocol, CallbackModule, Message, TransportProperties) ->
    case hello2_stateless_handler:run_binary_request(Protocol, CallbackModule, TransportProperties, Message) of
        {ok, _Req, Resp} ->
            hello2_proto:encode(Resp);
        {proto_reply, Resp} ->
            hello2_proto:encode(Resp)
    end.
