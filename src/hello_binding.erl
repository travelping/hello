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
-module(hello_binding).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Binding process
-export([start_link/5, stop/1, stop/2, behaviour_info/1]).
% API for listeners
-export([start_registered_handler/4, start_handler/4, incoming_message/2, lookup_handler/2]).
-export([new/7]).
-export_type([peer/0, handler/0]).

-include_lib("ex_uri/include/ex_uri.hrl").

-include("internal.hrl").

-type peer() :: term().
-type handler() :: pid().

%% ----------------------------------------------------------------------------------------------------
%% -- Contruct a new binding
-spec new(pid(), module(), string() | #ex_uri{}, module(), module(), stateful | stateless, term()) -> #binding{}.
new(Pid, ListenerModule, URL, Protocol, CallbackMod, CallbackType, CallbackArgs) when is_list(URL) ->
    {ok, DecURL, _} = ex_uri:decode(URL),
    new(Pid, ListenerModule, DecURL, Protocol, CallbackMod, CallbackType, CallbackArgs);
new(Pid, ListenerModule, URL = #ex_uri{}, Protocol, CallbackMod, CallbackType, CallbackArgs0) ->
    {IP, Host} = extract_ip_and_host(URL),
    CallbackArgs1 = case lists:keymember(exclusive, 1, CallbackArgs0) of
        true ->
            CallbackArgs0;
        false ->
            [{exclusive, true} | CallbackArgs0]
    end,
    Callback = #callback{
        mod = CallbackMod,
        type = CallbackType,
        args = CallbackArgs1
    },
    {_, Namespace, _} = hello_validate:find_hello_info(CallbackMod, <<"">>),
    Callbacks0 = dict:new(),
    Callbacks1 = dict:append(Namespace, Callback, Callbacks0),
    Binding = #binding{pid = Pid,
        url = URL,
        ip = IP,
        host = Host,
        port = (URL#ex_uri.authority)#ex_uri_authority.port,
        path = URL#ex_uri.path,
        protocol = Protocol,
        listener_mod = ListenerModule,
        callbacks = Callbacks1
    },
    Binding#binding{log_url = ListenerModule:url_for_log(Binding)}.

%% ----------------------------------------------------------------------------------------------------
%% -- API for listeners
-spec start_registered_handler(#binding{}, peer(), pid(), hello:transport_params()) -> handler().
start_registered_handler(Binding, Peer, Transport, TransportParams) ->
    case lookup_handler(Binding, Peer) of
        {error, not_found} ->
            Handler = start_handler(Binding, Peer, Transport, TransportParams),
            register_handler(Binding#binding.pid, Handler, Peer),
            Handler;
        {ok, Handler} ->
            Handler
    end.

-spec start_handler(#binding{}, peer(), pid(), hello:transport_params()) -> handler().
start_handler(#binding{callbacks=Callbacks}=Binding, Peer, Transport, TransportParams) ->
    [{_, [Callback]} | _] = dict:to_list(Callbacks),
    case Callback#callback.type of
        stateful ->
            {ok, Pid} = hello_stateful_handler:start_link(Binding, Peer, Transport, TransportParams),
            Pid;
        stateless ->
            spawn_link(hello_stateless_handler, handler, [Binding, Peer, Transport, TransportParams])
    end.

-spec incoming_message(handler(), binary()) -> ok.
incoming_message(Pid, Message) ->
    Pid ! {?INCOMING_MSG_MSG, Message}, ok.

-spec lookup_handler(#binding{}, peer()) -> {ok, handler()} | {error, not_found}.
lookup_handler(Binding, Peer) ->
    case hello_registry:lookup({listener_peer, Binding#binding.pid, Peer}) of
        {ok, Pid, _Data} ->
            {ok, Pid};
        Error ->
            Error
    end.

-spec register_handler(pid(), handler(), peer()) -> ok | {already_registered, pid(), peer()}.
register_handler(BindingPid, Pid, Peer) ->
    hello_registry:register({listener_peer, BindingPid, Peer}, undefined, Pid).

%% ----------------------------------------------------------------------------------------------------
%% -- Binding process
start_link(ListenerModule, URL, CallbackModule, CallbackType, CallbackArgs) when is_list(URL) ->
    {ok, DecURL, _} = ex_uri:decode(URL),
    start_link(ListenerModule, DecURL, CallbackModule, CallbackType, CallbackArgs);
start_link(ListenerModule, URL = #ex_uri{}, CallbackModule, CallbackType, CallbackArgs) ->
    Binding = new(self(), ListenerModule, URL, hello_proto_jsonrpc, CallbackModule, CallbackType, CallbackArgs),
    StarterRef = make_ref(),
    case gen_server:start(?MODULE, {self(), StarterRef, Binding}, []) of
        {ok, Pid} ->
            link(Pid), {ok, Pid};
        {error, Reason} ->
            {error, Reason};
        ignore ->
            receive
                {error, StarterRef, Error} ->
                    {error, Error}
            end
    end.

stop(BindingServer) ->
    stop(BindingServer, normal).

stop(BindingServer, Reason) ->
    gen_server:call(BindingServer, {stop, Reason}).

behaviour_info(callbacks) ->
    [{listener_key,1}, {binding_key,1}, {url_for_log,1}, {listener_specification,2}, {listener_termination,1}];
%% Listener termination will be used in hello_registry to terminate non hello supervised processes

behaviour_info(_) ->
    undefined.

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
-define(REFC(ID), {listener_refc, ID}).
-record(state, {binding, listener_pid, listener_id, listener_mref}).

init({StarterPid, StarterRef, Binding}) ->
    process_flag(trap_exit, true),

    case start_listener(Binding) of
        {ok, ListenerPid, ListenerID, ListenerMonitor} ->
            #binding{callbacks=Callbacks} = Binding,
            [{_, [Callback]} | _] = dict:to_list(Callbacks),
            ok = hello_request_log:open(Callback#callback.mod, self()),
            State = #state{binding = Binding,
                           listener_id = ListenerID,
                           listener_pid = ListenerPid,
                           listener_mref = ListenerMonitor},
            {ok, State, hibernate};
        {error, Error} ->
            StarterPid ! {error, StarterRef, Error},
            ignore
    end.

handle_call({stop, Reason}, _From, State) ->
    {stop, Reason, ok, State};
handle_call(_Call, _From, State) ->
    {noreply, State, hibernate}.

handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};
handle_info({'DOWN', MRef, process, Pid, Reason}, State = #state{listener_mref = MRef, listener_pid = Pid}) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State, hibernate}.

terminate(_Reason, #state{binding = Binding, listener_id = ListenerID, listener_pid = Pid}) ->
    #binding{callbacks=Callbacks} = Binding,
    [{_, [Callback]} | _] = dict:to_list(Callbacks),
    hello_request_log:close(Callback#callback.mod),
    case hello_registry:add_to_key(?REFC(ListenerID), -1) of
        {ok, Pid, 0} -> catch stop_listener(Binding#binding.listener_mod, ListenerID);
        _            -> ok
    end,
    error_logger:info_report([{hello_binding, ex_uri:encode(Binding#binding.url)}, {action, stopped}]).

%% unused callbacks
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_cast(_Cast, State)           -> {noreply, State, hibernate}.

%% --------------------------------------------------------------------------------
%% -- helpers
start_listener(BindingIn = #binding{listener_mod = Mod, callbacks = Callbacks}) ->
    Binding       = BindingIn#binding{log_url = Mod:url_for_log(BindingIn)},
    ListenerKey   = Mod:listener_key(BindingIn),
    ModBindingKey = Mod:binding_key(Binding),
    BindingKey    = {binding, Mod, ModBindingKey},
    [{_, [NewCallback]} | _] = dict:to_list(Callbacks),
    case hello_registry:lookup_listener(ListenerKey) of
        {error, not_found} ->
            %% no server running on Host:Port, we need to start a listener
            ListenerID = make_ref(),
            case start_listener(Mod, ListenerID, Binding) of
                {ok, ListenerPid} ->
                    ListenerMonitor = monitor(process, ListenerPid),
                    ListenerData = {ListenerID, Mod},
                    ok = hello_registry:multi_register([{?REFC(ListenerID), 1}, {ListenerKey, ListenerData}], ListenerPid),
                    ok = hello_registry:register(BindingKey, Binding, self()),
                    {ok, ListenerPid, ListenerID, ListenerMonitor};
                {error, {StartError, _Child}} ->
                    {error, {transport, StartError}}
            end;
        {ok, ListenerPid, {ListenerID, Mod}} ->
            %% listener (with same listener module) already running, do binding checks
            case hello_registry:lookup(BindingKey) of
                {error, not_found} ->
                    %% nobody is on that path yet, let's register the Module
                    %% this is only relevant to http listeners because there can
                    %% be N bindings per HTTP listener.
                    ok = hello_registry:register(BindingKey, Binding, self()),
                    {ok, ListenerPid, _} = hello_registry:add_to_key(?REFC(ListenerID), 1),
                    ListenerMonitor = monitor(process, ListenerPid),
                    {ok, ListenerPid, ListenerID, ListenerMonitor};
                {ok, _Pid, #binding{callbacks = Callbacks0}=ExistingBinding} ->
                    #callback{mod=NewCallbackMod, type=NewCallbackType} = NewCallback,
                    {_, Namespace, _} = hello_validate:find_hello_info(NewCallbackMod, <<"">>),
                    case dict:find(Namespace, Callbacks0) of
                        {ok, _} ->
                            {error, already_started};
                        error ->
                            Exclusive = dict:fold(
                                    fun
                                        (_, _, true) ->
                                            true;
                                        (_, [#callback{args=CallbackArgs, type=CallbackType}], false) when
                                                NewCallbackType =:= CallbackType ->
                                            {exclusive, Exclusive0} = lists:keyfind(exclusive, 1, CallbackArgs),
                                            Exclusive0;
                                        (_, _, false) ->
                                            true
                                    end, false, Callbacks0),
                            case Exclusive of
                                true ->
                                    {error, occupied};
                                false ->
                                    Callbacks1 = dict:append(Namespace, NewCallback, Callbacks0),
                                    NewBinding = ExistingBinding#binding{callbacks=Callbacks1},
                                    ok = hello_registry:update(BindingKey, NewBinding),
                                    ListenerMonitor = monitor(process, ListenerPid),
                                    {ok, ListenerPid, ListenerID, ListenerMonitor}
                            end
                    end
            end;
        {ok, _ListenerPid, _Data} ->
            %% there is a listener, but the listener module is different
            {error, occupied}
    end.

start_listener(Mod, ListenerID, Binding) ->
    case Mod:listener_specification(ListenerID, Binding) of
        {child, ListenerChildSpec} ->
            hello_listener_supervisor:start_child(ListenerChildSpec);
        {started, Result} ->
            Result
    end.

stop_listener(Mod, ListenerID) ->
    case Mod:listener_termination(ListenerID) of
        child ->
            hello_listener_supervisor:stop_child(ListenerID);
        _ ->
            ok
    end.

extract_ip_and_host(#ex_uri{authority = #ex_uri_authority{host = Host}}) ->
    case Host of
        "*"  ->
            {{0,0,0,0}, "0.0.0.0"};
        Host ->
            case inet_parse:address(Host) of
                {error, einval} ->
                    {{0,0,0,0}, Host};
                {ok, Address} ->
                    {Address, Host}
            end
    end.
