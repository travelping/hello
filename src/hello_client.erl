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

-export([start_link/2, start_link/3, start_link/4, start_link/5,
         start/4, start/5, stop/1,
         start_supervised/4, start_supervised/5, stop_supervised/1,
         call/2, call/3]).

%% for tests
-export([handle_internal/2]).

-include("hello.hrl").
-include("hello_log.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-define(DEFAULT_TIMEOUT, 10000).

-type client_name()  :: {local, atom()} | {global, atom()} | {via, atom(), term()}.
-type start_result() :: {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
-type call() :: {Method :: binary(), Args :: list(), Options :: [proplists:property()]}.
-type batch_call() :: list(call()).

%% Behaviour callbacks
-callback init_transport(#ex_uri{}, trans_opts()) -> 
    {ok, ClientState :: term()} | {error, Reason :: term()}.

-callback send_request(Reqquest :: binary(), signature(), ClientState :: term()) -> 
    {ok, NewClietnState :: term()} | {error, Reason :: term(), ClietnState :: term()}.

-callback terminate_transport(Reason :: term(), ClientState :: term()) -> ok.


%% API to start without supervisor
-spec start(URI :: string(), trans_opts(), protocol_opts(), client_opts()) -> start_result(). 
start(URI, TransportOpts, ProtocolOpts, ClientOpts) ->
    gen_server:start(?MODULE, {URI, TransportOpts, ProtocolOpts, ClientOpts}, []).

-spec start(client_name(), URI :: string(), trans_opts(), protocol_opts(), client_opts()) -> start_result(). 
start(Name, URI, TransportOpts, ProtocolOpts, ClientOpts) ->
    gen_server:start(Name, ?MODULE, {URI, TransportOpts, ProtocolOpts, ClientOpts}, []).

-spec stop(client_name()) -> term().
stop(Client) ->
    gen_server:call(Client, terminate).

%% callbacks for supervisor
% @deprecated
start_link(URI, {TransportOpts, ProtocolOpts, ClientOpts}) ->
    gen_server:start_link(?MODULE, {URI, TransportOpts, ProtocolOpts, ClientOpts}, []).

% @deprecated
start_link(Name, URI, {TransportOpts, ProtocolOpts, ClientOpts}) ->
    gen_server:start_link(Name, ?MODULE, {URI, TransportOpts, ProtocolOpts, ClientOpts}, []).

-spec start_link(URI :: string(), trans_opts(), protocol_opts(), client_opts()) -> start_result().
start_link(URI, TransportOpts, ProtocolOpts, ClientOpts) ->
    gen_server:start_link(?MODULE, {URI, TransportOpts, ProtocolOpts, ClientOpts}, []).

-spec start_link(client_name(), URI :: string(), trans_opts(), protocol_opts(), client_opts()) -> start_result(). 
start_link(Name, URI, TransportOpts, ProtocolOpts, ClientOpts) ->
    gen_server:start_link(Name, ?MODULE, {URI, TransportOpts, ProtocolOpts, ClientOpts}, []).

%% API to start with hello supervisor
-spec start_supervised(URI :: string(), trans_opts(), protocol_opts(), client_opts()) -> 
    supervisor:startchild_ret(). 
start_supervised(URI, TransportOpts, ProtocolOpts, ClientOpts) ->
    hello_client_sup:start_client(URI, {TransportOpts, ProtocolOpts, ClientOpts}).

-spec start_supervised(atom(), URI :: string(), trans_opts(), protocol_opts(), client_opts()) -> 
    {ok, pid()}. 
start_supervised(Name, URI, TransportOpts, ProtocolOpts, ClientOpts) ->
    hello_client_sup:start_named_client(Name, URI, {TransportOpts, ProtocolOpts, ClientOpts}).

-spec stop_supervised(client_name()) -> ok | {error, Reason :: term()}. 
stop_supervised(Client) ->
    hello_client_sup:stop_client(Client).

-spec call(client_name(), Call :: call() | batch_call()) ->  term().
call(Client, Call) ->
    timeout_call(Client, {call, Call}, ?DEFAULT_TIMEOUT).

-spec call(client_name(), Call :: call() | batch_call(), Timeout :: integer()) -> term().
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
    protocol_opts :: list(),
    protocol_state ::term(),
    async_request_map = gb_trees:empty() :: gb_trees:tree(),
    keep_alive_interval :: number(),
    keep_alive_ref :: term(),
    notification_sink :: pid() | function()
}).

%% @hidden
init({URI, TransportOpts, ProtocolOpts, ClientOpts}) ->
    process_flag(trap_exit, true),
    case (catch ex_uri:decode(URI)) of
        {ok, URIRec = #ex_uri{}, _} ->
            case uri_client_module(URIRec) of
                {NewURIRec, TransportModule} ->
                    init_transport(TransportModule, NewURIRec, TransportOpts, ProtocolOpts, ClientOpts);
                badscheme -> {stop, badscheme}
            end;
        _Other -> {stop, badurl}
    end.

%% @hidden
handle_call({call, Call}, From, State = #client_state{protocol_mod = ProtocolMod, protocol_state = ProtocolState}) ->
    case hello_proto:build_request(Call, ProtocolMod, ProtocolState) of
        {ok, Request, NewProtocolState} ->
            State1 = State#client_state{protocol_state = NewProtocolState},
            case outgoing_message(Request, From, State1) of
                {ok, State2} -> {noreply, State2};
                {ok, Reply, State2} -> {reply, Reply, State2};
                {error, Reason, State2} ->
                    ?LOG_ERROR("unsuccessful send request: ~p, reason: ~p", [Request, Reason]),
                    {reply, Reason, State2}
            end;
        {error, Reason, NewProtocolState} ->
            ?LOG_ERROR("unsuccessful build request for call: ~p, reason: ~p", [Call, Reason]),
            {reply, Reason, State#client_state{protocol_state = NewProtocolState}}
    end;
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

%% @hidden
handle_info({?INCOMING_MSG, Message}, State) ->
    incoming_message(Message, State);
handle_info(?PING, State = #client_state{transport_mod=TransportModule, transport_state=TransportState,
                                         keep_alive_interval = KeepAliveInterval, keep_alive_ref = TimerRef}) ->
    {ok, NewTransportState} = TransportModule:send_request(?PING, ?INTERNAL_SIGNATURE, TransportState),
    timer:cancel(TimerRef),
    {ok, NewTimerRef} = timer:send_after(KeepAliveInterval, self(), ?PING),
    {noreply, State#client_state{transport_state = NewTransportState, keep_alive_ref = NewTimerRef}};

handle_info(Info, State = #client_state{transport_mod=TransportModule, transport_state=TransportState}) ->
    case TransportModule:handle_info(Info, TransportState) of
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
init_transport(TransportModule, URIRec, TransportOpts, ProtocolOpts, ClientOpts) ->
    case TransportModule:init_transport(URIRec, TransportOpts) of
        {ok, TransportState} ->
            NotificationSink = proplists:get_value(notification_sink, ProtocolOpts, undefined),
            ProtocolMod = proplists:get_value(protocol, ProtocolOpts, hello_proto_jsonrpc),
            case hello_proto:init_client(ProtocolMod, ProtocolOpts) of
                {ok, ProtocolState} ->
                    State = #client_state{transport_mod = TransportModule,
                                          transport_state = TransportState,
                                          protocol_mod = ProtocolMod,
                                          protocol_opts = ProtocolOpts,
                                          protocol_state = ProtocolState,
                                          notification_sink = NotificationSink},
                    evaluate_client_options(ClientOpts, State);
                {error, Reason} -> {stop, Reason}
            end;
        {error, Reason} -> {stop, Reason}
    end.

evaluate_client_options(ClientOpts, State) ->
    KeepAliveInterval = proplists:get_value(keep_alive_interval, ClientOpts, -1),
    if
        KeepAliveInterval =< 0 ->
            {ok, State#client_state{keep_alive_interval = -1}};
        KeepAliveInterval > 0 ->
            {ok, TimerRef} = timer:send_after(KeepAliveInterval, self(), ?PING),
            {ok, State#client_state{keep_alive_interval = KeepAliveInterval, keep_alive_ref = TimerRef}}
    end.

incoming_message({error, _Reason, NewTransportState}, State) -> %%will be logged later
    {noreply, State#client_state{transport_state = NewTransportState}};
incoming_message({ok, Signature, BinResponse, NewTransportState},
                 State = #client_state{async_request_map = AsyncMap, protocol_mod = ProtocolMod, protocol_opts = ProtocolOpts}) ->
    case hello_proto:decode(ProtocolMod, ProtocolOpts, Signature, BinResponse, response) of
        {ok, #response{id = null, response = Response}} ->
            notification(Response, State),
            {noreply, State#client_state{transport_state = NewTransportState}};
        {ok, Response = #response{}} ->
            NewAsyncMap = request_reply(Response, AsyncMap),
            {noreply, State#client_state{transport_state = NewTransportState, async_request_map = NewAsyncMap}};
        {ok, Responses = [{ok, #response{}} | _]} ->
            NotificationRespopses = [R || {_, #response{id = null} = R} <- Responses],
            NotificationRespopses /= [] andalso notification([R || #response{response = R} <- NotificationRespopses], State),
            Responses1 = [R || {_, R} <- Responses] -- NotificationRespopses,
            NewAsyncMap = request_reply(Responses1, AsyncMap),
            {noreply, State#client_state{transport_state = NewTransportState, async_request_map = NewAsyncMap}};
        {error, _Reason} ->
            {noreply, State#client_state{transport_state = NewTransportState}};
        ignore ->
            {noreply, State#client_state{transport_state = NewTransportState}};
        {internal, Message} ->
            {ok, State1} = ?MODULE:handle_internal(Message, State),
            {noreply, State1}
    end.

outgoing_message(Request, From, State = #client_state{protocol_mod = ProtocolMod, protocol_opts = ProtocolOpts,
                                                      transport_mod = TransportModule, transport_state = TransportState, 
                                                      async_request_map = AsyncMap}) ->
    case hello_proto:encode(ProtocolMod, ProtocolOpts, Request) of
        {ok, BinRequest} ->
            Signature = hello_proto:signature(ProtocolMod, ProtocolOpts),
            case TransportModule:send_request(BinRequest, Signature, TransportState) of
                {ok, NewTransportState} ->
                    maybe_noreply(NewTransportState, Request, From, AsyncMap, State);
                {error, Reason, NewTransportState} ->
                    {error, Reason, State#client_state{transport_state = NewTransportState}}
            end;
        {error, Reason, State} -> {error, Reason, State};
        ignore -> {ok, State}
    end.

update_map(Requests, From, AsyncMap0) when is_list(Requests) -> 
    lists:foldl(fun(Request, AsyncMap) ->
                        update_map(Request, From, AsyncMap)
                end, AsyncMap0, Requests);
update_map(#request{id = undefined}, _From, AsyncMap) -> AsyncMap;
update_map(#request{id = RequestId} = Request, From, AsyncMap) -> gb_trees:enter(RequestId, {From, Request}, AsyncMap).

handle_internal(?PONG, State = #client_state{keep_alive_interval = KeepAliveInterval, keep_alive_ref = TimerRef}) ->
    timer:cancel(TimerRef),
    {ok, NewTimerRef} = timer:send_after(KeepAliveInterval, self(), ?PING),
    {ok, State#client_state{keep_alive_ref = NewTimerRef}};
handle_internal(_Message, State) ->
    %?MODULE:handle_internal(list_to_atom(binary_to_list(Message)), State).
    {ok, State}.

maybe_noreply(NewTransportState, [#request{type = async} | _], _, _, State) ->
    {ok, ok, State#client_state{transport_state = NewTransportState}};
maybe_noreply(NewTransportState, #request{type = async}, _, _, State) ->
    {ok, ok, State#client_state{transport_state = NewTransportState}};
maybe_noreply(NewTransportState, Request, From, AsyncMap, State) ->
    {ok, State#client_state{transport_state = NewTransportState, async_request_map = update_map(Request, From, AsyncMap)}}.

uri_client_module(URI = #ex_uri{scheme = "http"})     -> {URI, hello_http_client};
uri_client_module(URI = #ex_uri{scheme = "https"})    -> {URI, hello_http_client};
uri_client_module(URI = #ex_uri{scheme = "zmq-tcp"})  -> {URI, hello_zmq_client};
uri_client_module(URI = #ex_uri{scheme = "zmq-tcp6"}) -> {URI, hello_zmq_client};
uri_client_module(_) ->
    badscheme.

notification(Response, _State = #client_state{notification_sink = NotificationSink}) ->
    if
        is_pid(NotificationSink) ->
            NotificationSink ! {notification, Response};
        is_function(NotificationSink) ->
            erlang:apply(NotificationSink, [Response]);
        true ->
            ignore
    end.

request_reply(#response{} = Response, AsyncMap) ->
    request_reply([Response], AsyncMap, []);
request_reply(Responses, AsyncMap) ->
    request_reply(Responses, AsyncMap, []).

request_reply([], AsyncMap, [{CallRef, Response}]) -> 
    gen_server:reply(CallRef, {ok, Response}),
    AsyncMap;
request_reply([], AsyncMap, Responses) ->
    Refs = proplists:get_keys(Responses),
    lists:map(fun(CallRef) -> 
                NewAcc = lists:filtermap(fun({Ref, Response}) when Ref =:= CallRef -> 
                                                 {true, Response}; 
                                            (_) -> false end, lists:reverse(Responses)),
                gen_server:reply(CallRef, {ok, NewAcc})
              end, Refs),
    AsyncMap;
request_reply([#response{id = RequestId, response = Response} | Tail], AsyncMap, Responses) ->
    case gb_trees:lookup(RequestId, AsyncMap) of
        {value, {CallRef, _Request}} ->
            NewAsyncMap = gb_trees:delete(RequestId, AsyncMap),
            request_reply(Tail, NewAsyncMap, [{CallRef, Response} | Responses]);
        none ->
            ?LOG_WARNING("get not existing request_id ~p", [RequestId]),
            AsyncMap
    end.
