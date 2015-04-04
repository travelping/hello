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

%% @doc This module contains an RPC client.
-module(hello_client).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/2, start_link/3,
         start/4, start/5, stop/1,
         start_supervised/4, start_supervised/5, stop_supervised/1,
         call/2, call/3]
         ).

%% for tests
-export([handle_internal/2]).

%% client transport implementation API
-export([behaviour_info/1]).

-include("internal.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-define(DEFAULT_TIMEOUT, 10000).

behaviour_info(callbacks) ->
    [{init_transport,2},
     {send_request,2},
     {terminate_transport,2}];
behaviour_info(_) ->
    undefined.

%% API to start without supervisor
start(URI, TransportOpts, ProtocolOpts, ClientOpts) ->
    gen_server:start_link(?MODULE, {URI, TransportOpts, ProtocolOpts, ClientOpts}, []).

start(Name, URI, TransportOpts, ProtocolOpts, ClientOpts) ->
    gen_server:start_link(Name, ?MODULE, {URI, TransportOpts, ProtocolOpts, ClientOpts}, []).

stop(Client) ->
    gen_server:call(Client, terminate).

%% callbacks for supervisor
start_link(URI, {TransportOpts, ProtocolOpts, ClientOpts}) ->
    start(URI, TransportOpts, ProtocolOpts, ClientOpts).

start_link(Name, URI, {TransportOpts, ProtocolOpts, ClientOpts}) ->
    start(Name, URI, TransportOpts, ProtocolOpts, ClientOpts).

%% API to start with hello supervisor
start_supervised(URI, TransportOpts, ProtocolOpts, ClientOpts) ->
    hello_client_sup:start_client(URI, {TransportOpts, ProtocolOpts, ClientOpts}).

start_supervised(Name, URI, TransportOpts, ProtocolOpts, ClientOpts) ->
    hello_client_sup:start_named_client(Name, URI, {TransportOpts, ProtocolOpts, ClientOpts}).

stop_supervised(Client) ->
    hello_client_sup:stop_client(Client).

call(Client, Call) ->
    timeout_call(Client, {call, Call}, ?DEFAULT_TIMEOUT).

call(Client, Call, Timeout) ->
    timeout_call(Client, {call, Call}, Timeout).

timeout_call(Client, Call, infinity) ->
    gen_server:call(Client, Call, infinity);
timeout_call(Client, Call, Timeout) ->
    try
        gen_server:call(Client, Call, Timeout)
    catch
        exit:{timeout, {gen_server, call, _}} ->
            {error, timeout}
    end.

%% ----------------------------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(client_state, {
    transport_mod :: module(),
    transport_state :: term(),
    protocol_mod :: atom(),
    protocol_state ::term(),
    async_request_map :: gb_tree(),
    keep_alive_interval :: number(),
    keep_alive_ref :: term(),
    notification_sink :: pid() | function()
}).

%% @hidden
init({URI, TransportOpts, ProtocolOpts, ClientOpts}) ->
    case (catch ex_uri:decode(URI)) of
        {ok, URIRec = #ex_uri{}, _} ->
            case uri_client_module(URIRec) of
                {NewURIRec, TransportModule} ->
                    case TransportModule:init_transport(NewURIRec, TransportOpts) of
                        {ok, TransportState} ->
                            NotificationSink = proplists:get_value(notification_sink, ProtocolOpts, undefined),
                            ProtocolMod = proplists:get_value(protocol, ProtocolOpts, hello_proto_jsonrpc),
                            case hello_proto:init_client(ProtocolMod, ProtocolOpts) of
                                {ok, ProtocolState} ->
                                    State = #client_state{  transport_mod = TransportModule,
                                                            transport_state = TransportState,
                                                            protocol_mod = ProtocolMod,
                                                            protocol_state = ProtocolState,
                                                            async_request_map = gb_trees:empty(),
                                                            notification_sink = NotificationSink
                                                            },
                                    case evaluate_client_options(ClientOpts, State) of
                                        {ok, State1} ->
                                            {ok, State1};
                                        {error, Reason} ->
                                            {error, Reason}
                                    end;
                                {error, Reason} ->
                                    {stop, Reason}
                            end;
                        {error, Reason} ->
                            {stop, Reason}
                    end;
                badscheme ->
                    {stop, badscheme}
            end;
        _Other ->
            {stop, badurl}
    end.

%% @hidden
handle_call({call, Call}, From, State = #client_state{protocol_mod = ProtocolMod, protocol_state = ProtocolState}) ->
    case hello_proto:build_request(Call, ProtocolMod, ProtocolState) of
        {ok, Request, NewProtocolState} ->
            State1 = State#client_state{protocol_state = NewProtocolState},
            case outgoing_message(Request, From, State1) of
                {ok, State2} ->
                    {noreply, State2};
                {ok, Reply, State2} ->
                    {reply, Reply, State2};
                {error, Reason, State2} ->
                    {reply, Reason, State2}
            end;
        {error, Reason, NewProtocolState} ->
            {reply, Reason, State#client_state{protocol_state = NewProtocolState}}
    end;
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

%% @hidden
handle_info({?INCOMING_MSG, Message}, State) ->
    incoming_message(Message, State);
handle_info(?PING, State = #client_state{transport_mod=TransportModule, transport_state=TransportState}) ->
    EncodeInfo = hello_proto:encoding_info(hello_proto),
    BinaryPing = list_to_binary(atom_to_list(?PING)),
    {ok, NewTransportState} = TransportModule:send_request({BinaryPing, EncodeInfo}, TransportState),
    {noreply, State#client_state{transport_state = NewTransportState}};
handle_info(Message, State = #client_state{transport_mod=TransportModule, transport_state=TransportState}) ->
    case TransportModule:handle_info(Message, TransportState) of
        {?INCOMING_MSG, Message} ->
            incoming_message(Message, State);
        {noreply, NewTransportState} ->
            {noreply, State#client_state{transport_state = NewTransportState}}
    end.

%% @hidden
terminate(Reason, #client_state{transport_mod = TransportModule, transport_state = TransportState, keep_alive_ref = TimerRef}) ->
    timer:cancel(TimerRef),
    TransportModule:terminate_transport(Reason, TransportState).

%% @hidden
handle_cast(_Cast, State) ->
    {noreply, State}.
%% @hidden
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

%% --------------------------------------------------------------------------------
%% -- Helper functions
incoming_message({error, _Reason, NewTransportState}, State) -> %%will be logged later
    {noreply, State#client_state{transport_state = NewTransportState}};
incoming_message({ok, BinResponse, NewTransportState},
                 State = #client_state{protocol_mod = ProtocolMod, protocol_state = ProtocolState, async_request_map = AsyncMap}) ->
    case hello_proto:decode(ProtocolMod, BinResponse, response) of
        {ok, Response = #request{id = undefined}} ->
            notification(Response, State),
            {noreply, State#client_state{transport_state = NewTransportState}};
        {ok, Response = #response{id = RequestId}} ->
            NewAsyncMap = request_reply(Response, State),
            {noreply, State#client_state{transport_state = NewTransportState, async_request_map = NewAsyncMap}};
        {error, _Reason} ->
            {noreply, State#client_state{transport_state = NewTransportState}};
        ignore ->
            {noreply, State#client_state{transport_state = NewTransportState}};
        {internal, Message} ->
            {ok, State1} = ?MODULE:handle_internal(Message, State),
            {noreply, State1}
    end.

outgoing_message(Request, From, State = #client_state{protocol_mod = ProtocolMod, transport_mod=TransportModule, transport_state=TransportState, async_request_map=AsyncMap}) ->
    case hello_proto:encode(ProtocolMod, Request) of
        {ok, BinRequest} ->
            case TransportModule:send_request(BinRequest, TransportState) of
                {ok, NewTransportState} ->
                    {ok, State#client_state{transport_state = NewTransportState, async_request_map = update_map(Request, From, AsyncMap)}};
                {error, Reason, NewTransportState} ->
                    {error, Reason, State#client_state{transport_state = NewTransportState}}
            end;
        {error, Reason, State} ->
            {error, Reason, State};
        ignore ->
            {ok, State}
    end.

update_map(#request{id = undefined}, _From, AsyncMap) -> AsyncMap;
update_map(#request{id = RequestId} = Request, From, AsyncMap) -> gb_trees:enter(RequestId, {From, Request}, AsyncMap).

evaluate_client_options(ClientOpts, State) ->
    KeepAliveInterval = proplists:get_value(keep_alive_interval, ClientOpts, -1),
    if
        KeepAliveInterval =< 0 ->
            {ok, State#client_state{keep_alive_interval = -1}};
        KeepAliveInterval > 0 ->
            timer:send_after(KeepAliveInterval, self(), ?PING),
            {ok, State#client_state{keep_alive_interval = KeepAliveInterval}}
    end.


handle_internal(?PONG, State = #client_state{keep_alive_interval = KeepAliveInterval}) ->
    {ok, TimerRef} = timer:send_after(KeepAliveInterval, self(), ?PING),
    {ok, State#client_state{keep_alive_ref = TimerRef}};
handle_internal(Message, State) ->
    ?MODULE:handle_internal(list_to_atom(binary_to_list(Message)), State).

uri_client_module(URI = #ex_uri{scheme = "http"})     -> {URI, hello_http_client};
uri_client_module(URI = #ex_uri{scheme = "https"})    -> {URI, hello_http_client};
uri_client_module(URI = #ex_uri{scheme = "zmq-tcp"})  -> {URI, hello_zmq_client};
uri_client_module(URI = #ex_uri{scheme = "zmq-tcp6"}) -> {URI, hello_zmq_client};
uri_client_module(_) ->
    badscheme.

notification(#response{response = Response}, _State = #client_state{notification_sink = NotificationSink}) ->
    if
        is_pid(NotificationSink) ->
            NotificationSink ! {notification, Response};
        is_function(NotificationSink) ->
            erlang:apply(NotificationSink, [Response]);
        true ->
            ignore
    end.

request_reply(#response{id = RequestId, response = Response}, _State = #client_state{async_request_map = AsyncMap}) ->
    case gb_trees:lookup(RequestId, AsyncMap) of
        {value, {CallRef, _Request}} ->
            gen_server:reply(CallRef, {ok, Response}),
            gb_trees:delete(RequestId, AsyncMap);
        none ->
            lager:warning("get not existing request_id ~p", [RequestId]),
            AsyncMap
    end.
