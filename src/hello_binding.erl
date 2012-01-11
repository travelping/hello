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
-export([start_registered_handler/3, start_handler/3, incoming_message/2, lookup_handler/2]).
% Super Secret Internal API
-export([stateless_request/4]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("internal.hrl").

-export_type([handler/0]).
-type peer() :: term().

-record(stateless, {
    handler_mod :: module(),
    peer        :: peer()
}).

-opaque handler() :: pid() | #stateless{}.

%% ----------------------------------------------------------------------------------------------------
%% -- API for listeners
-spec start_registered_handler(#binding{}, peer(), pid()) -> term().
start_registered_handler(Binding, Peer, Transport) ->
    case lookup_handler(Binding, Peer) of
        {error, not_found} ->
            Handler = start_handler(Binding, Peer, Transport),
            register_handler(Binding#binding.pid, Handler, Peer),
            Handler;
        {ok, Handler} ->
            Handler
    end.

-spec start_handler(#binding{}, peer(), pid()) -> handler().
start_handler(Binding, Peer, Transport) ->
    case Binding#binding.callback_type of
        stateful ->
            {ok, Pid} = hello_stateful_handler:start_link(Binding, Peer, Transport),
            Pid;
        stateless ->
            #stateless{handler_mod = Binding#binding.callback_mod, peer = Peer}
    end.

-spec incoming_message(handler(), binary()) -> any().
incoming_message(#stateless{handler_mod = Mod, peer = Peer}, Message) ->
    spawn(?MODULE, stateless_request, [self(), Peer, Mod, Message]);
incoming_message(Pid, Message) ->
    hello_stateful_handler:incoming_message(Pid, Message).

-spec stateless_request(pid(), term(), module(), binary()) -> any().
stateless_request(Pid, Peer, CallbackModule, Message) ->
    Response = hello:run_stateless_binary_request(CallbackModule, Message),
    Handler = #stateless{handler_mod = CallbackModule, peer = Peer},
    Pid ! {hello_msg, Handler, Peer, Response},
    Pid ! {hello_closed, Handler, Peer}.

-spec lookup_handler(#binding{}, peer()) -> {ok, handler()} | {error, not_found}.
lookup_handler(Binding = #binding{callback_type = stateful}, Peer) ->
    case hello_registry:lookup({listener_peer, Binding#binding.pid, Peer}) of
        {ok, Pid, _Data} ->
            {ok, Pid};
        Error ->
            Error
    end;
lookup_handler(#binding{callback_mod = CallbackMod}, Peer) ->
    {ok, #stateless{handler_mod = CallbackMod, peer = Peer}}.

-spec register_handler(pid(), handler(), peer()) -> ok | {already_registered, pid(), peer()}.
register_handler(BindingPid, Pid, Peer) when is_pid(Pid) ->
    hello_registry:register({listener_peer, BindingPid, Peer}, undefined, Pid);
register_handler(_BindingPid, _Handler, _Peer) ->
    ok.

%% ----------------------------------------------------------------------------------------------------
%% -- Binding process
start_link(ListenerModule, URL, CallbackModule, CallbackType, CallbackArgs) when is_list(URL) ->
    {ok, DecURL, _} = ex_uri:decode(URL),
    start_link(ListenerModule, DecURL, CallbackModule, CallbackType, CallbackArgs);
start_link(ListenerModule, URL = #ex_uri{}, CallbackModule, CallbackType, CallbackArgs) ->
    {IP, Host} = extract_ip_and_host(URL),
    Binding = #binding{pid = self(),
                       url = URL,
                       ip = IP,
                       host = Host,
                       port = (URL#ex_uri.authority)#ex_uri_authority.port,
                       path = URL#ex_uri.path,
                       listener_mod = ListenerModule,
                       callback_mod = CallbackModule,
                       callback_type = CallbackType,
                       callback_args = CallbackArgs},
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
    [{listener_key,1}, {binding_key,1}, {listener_childspec,2}];

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
    case hello_registry:add_to_key(?REFC(ListenerID), -1) of
        {ok, Pid, 0} -> catch hello_listener_supervisor:stop_child(ListenerID);
        _            -> ok
    end,
    error_logger:info_msg("hello binding ~s stopped~n", [ex_uri:encode(Binding#binding.url)]).

%% unused callbacks
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_cast(_Cast, State)           -> {noreply, State, hibernate}.

    %% --------------------------------------------------------------------------------
%% -- helpers
start_listener(Binding = #binding{listener_mod = Mod, callback_mod = CallbackMod}) ->
    ListenerKey   = Mod:listener_key(Binding),
    ModBindingKey = Mod:binding_key(Binding),
    BindingKey    = {binding, Mod, ModBindingKey},

    case hello_registry:lookup_listener(ListenerKey) of
        {error, not_found} ->
            %% no server running on Host:Port, we need to start a listener
            ListenerID = make_ref(),
            ListenerChildSpec = Mod:listener_childspec(ListenerID, Binding),
            case hello_listener_supervisor:start_child(ListenerChildSpec) of
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
                {ok, _Pid, #binding{callback_mod = CallbackMod}} ->
                    {error, already_started};
                {ok, _Pid, #binding{callback_mod = _OtherModule}} ->
                    {error, occupied}
            end;
        {ok, _ListenerPid, _Data} ->
            %% there is a listener, but the listener module is different
            {error, occupied}
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
