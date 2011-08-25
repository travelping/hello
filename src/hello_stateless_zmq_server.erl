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
-module(hello_stateless_zmq_server).
-export([start_link/2, url_for_log/1, url_for_zmq/1]).

-behaviour(hello_binding).
-export([listener_childspec/2, listener_key/1, binding_key/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("internal.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").

-define(SHUTDOWN_TIMEOUT, 500).

%% --------------------------------------------------------------------------------
%% -- API
start_link(URI, Module) ->
    gen_server:start_link(?MODULE, {URI, Module}, []).

%% --------------------------------------------------------------------------------
%% -- hello_binding callbacks
listener_childspec(ListenerID, #binding{url = URL, callback_mod = CallbackModule}) ->
    {ListenerID, {?MODULE, start_link, [URL, CallbackModule]}, transient, ?SHUTDOWN_TIMEOUT, worker, [?MODULE]}.

listener_key(#binding{url = #ex_uri{scheme = "zmq-tcp"}, ip = IP, port = Port}) ->
    hello_registry:listener_key(IP, Port);
listener_key(#binding{url = #ex_uri{scheme = "zmq-ipc"}, host = Host, path = Path}) ->
    hello_registry:listener_key({ipc, ipc_path(Host, Path)}, undefined).

binding_key(#binding{url = #ex_uri{scheme = "zmq-tcp"}, ip = IP, port = Port}) ->
    {IP, Port};
binding_key(#binding{url = #ex_uri{scheme = "zmq-ipc"}, host = Host, port = Port}) ->
    ipc_path(Host, Port).

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(state, {socket, uri, context, mod}).

init({URI = #ex_uri{}, CallbackModule}) ->
    process_flag(trap_exit, true),

    Endpoint = url_for_zmq(URI),
    {ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, [rep, {active, true}]),
    case erlzmq:bind(Socket, Endpoint) of
        ok ->
            {ok, _Log} = hello_request_log:open(CallbackModule, self()),
            EncURI     = url_for_log(URI),
            {ok, #state{socket = Socket, uri = EncURI, context = Context, mod = CallbackModule}};
        {error, Error} ->
            {stop, Error}
    end.

handle_info({zmq, Socket, Message, []}, State = #state{socket = Socket, uri = URI, mod = Mod}) ->
    spawn(fun () ->
                  JSONReply = hello:run_stateless_binary_request(Mod, Message),
                  ok = hello_request_log:request(Mod, URI, Message, JSONReply),
                  ok = erlzmq:send(Socket, JSONReply)
          end),
    {noreply, State}.

terminate(_Reason, State) ->
    hello_request_log:close(State#state.mod),
    erlzmq:close(State#state.socket),
    erlzmq:term(State#state.context).

%% unused callbacks
handle_call(_Call, _From, State) ->
    {reply, {error, unknown_call}, State}.
handle_cast(_Cast, State) ->
    {noreply, State}.
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

%% --------------------------------------------------------------------------------
%% -- helpers
url_for_zmq(URI = #ex_uri{scheme = "zmq-tcp"}) -> ex_uri:encode(URI#ex_uri{scheme = "tcp"});
url_for_zmq(URI = #ex_uri{scheme = "zmq-ipc"}) -> ex_uri:encode(URI#ex_uri{scheme = "ipc"}).

url_for_log(URI = #ex_uri{scheme = "zmq-ipc", authority = #ex_uri_authority{host = Host}, path = Path}) ->
    WithFullPath = URI#ex_uri{path = ipc_path(Host, Path), authority = #ex_uri_authority{host = ""}},
    list_to_binary(ex_uri:encode(WithFullPath));
url_for_log(URI) ->
    list_to_binary(ex_uri:encode(URI)).

ipc_path(Host, Path) ->
    case Host of
        undefined -> Path;
        _         -> filename:absname(filename:join(Host, Path))
    end.
