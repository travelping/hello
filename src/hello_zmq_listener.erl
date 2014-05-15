% Copyright (c) 2011 by Travelping GmbH <info@travelping.com>

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
-module(hello_zmq_listener).
-export([start_link/1]).

-behaviour(hello_binding).
-export([listener_specification/2, listener_key/1, binding_key/1, url_for_log/1, listener_termination/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("internal.hrl").
-define(SHUTDOWN_TIMEOUT, 500).

start_link(Binding) ->
    gen_server:start_link(?MODULE, Binding, []).

%% --------------------------------------------------------------------------------
%% -- hello_binding
listener_specification(ListenerID, Binding) ->
    StartFun = {?MODULE, start_link, [Binding]},
    Specs = {ListenerID, StartFun, transient, ?SHUTDOWN_TIMEOUT, worker, [?MODULE]},
    {child, Specs}.

listener_key(#binding{url = #ex_uri{scheme = "zmq-tcp"}, ip = IP, port = Port}) ->
    hello_registry:listener_key(IP, Port);
listener_key(#binding{url = #ex_uri{scheme = "zmq-ipc"}, host = Host, path = Path}) ->
    hello_registry:listener_key({ipc, ipc_path(Host, Path)}, undefined).

binding_key(#binding{url = #ex_uri{scheme = "zmq-tcp"}, ip = IP, port = Port}) ->
    {IP, Port};
binding_key(#binding{url = #ex_uri{scheme = "zmq-ipc"}, host = Host, port = Port}) ->
    ipc_path(Host, Port).

url_for_log(#binding{url = URL}) ->
    url_for_log1(URL).

listener_termination(ListenerID) ->
    child.

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(state, {
    context :: erlzmq:erlzmq_context(),
    socket  :: erlzmq:erlzmq_socket(),
    binding_key :: term(),
    lastmsg_peer :: binary()
}).

init(Binding = #binding{url = URL}) ->
    process_flag(trap_exit, true),
    Endpoint      = url_for_zmq(URL),
    {ok, Context} = erlzmq:context(),
    {ok, Socket}  = erlzmq:socket(Context, [router, {active, true}]),
    case erlzmq:bind(Socket, Endpoint) of
        ok ->
            State = #state{binding_key = binding_key(Binding), socket = Socket, context = Context},
            {ok, State};
        {error, Error} ->
            {stop, Error}
    end.

handle_info({zmq, Socket, Message, [rcvmore]}, State = #state{socket = Socket, lastmsg_peer = undefined}) ->
    %% first message part is peer identity
    {noreply, State#state{lastmsg_peer = Message}};
handle_info({zmq, Socket, <<>>, [rcvmore]}, State = #state{socket = Socket, lastmsg_peer = Peer}) when is_binary(Peer) ->
    %% empty message part separates envelope from data
    {noreply, State};
handle_info({zmq, Socket, Message, []}, State = #state{binding_key=BindingKey, socket = Socket, lastmsg_peer = Peer}) ->
    %% second message part is the actual request
    case hello_registry:lookup_binding(?MODULE, BindingKey) of
        {error, not_found} ->
            ok; % should never happen
        {ok, Binding} ->
            case hello_binding:lookup_handler(Binding, Peer) of
                {error, not_found} ->
                    TransportParams = [{peer_identity, Peer}],
                    HandlerPid = hello_binding:start_registered_handler(Binding, Peer, self(), TransportParams),
                    hello_binding:incoming_message(HandlerPid, Message);
                {ok, HandlerPid} ->
                    hello_binding:incoming_message(HandlerPid, Message)
            end
    end,
    {noreply, State#state{lastmsg_peer = undefined}};

handle_info({hello_msg, _Handler, Peer, Message}, State = #state{socket = Socket}) ->
    ok = erlzmq:send(Socket, Peer, [sndmore]),
    ok = erlzmq:send(Socket, <<>>, [sndmore]),
    ok = erlzmq:send(Socket, Message),
    {noreply, State};

handle_info({hello_closed, _HandlerPid, _Peer}, State) ->
    {noreply, State};

handle_info({'EXIT', _HandlerPid, _Reason}, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
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

url_for_log1(URI = #ex_uri{scheme = "zmq-ipc", authority = #ex_uri_authority{host = Host}, path = Path}) ->
    WithFullPath = URI#ex_uri{path = ipc_path(Host, Path), authority = #ex_uri_authority{host = ""}},
    list_to_binary(ex_uri:encode(WithFullPath));
url_for_log1(URI) ->
    list_to_binary(ex_uri:encode(URI)).

ipc_path(Host, Path) ->
    case Host of
        undefined -> Path;
        _         -> filename:absname(filename:join(Host, Path))
    end.
