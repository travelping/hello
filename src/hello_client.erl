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
-export([gen_meta_fields/1]).

-include("hello.hrl").
-include("hello_log.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").

-define(HELLO_CLIENT_DEFAULT_META(ClientId, Url), [{hello_client, ClientId}, {hello_transport_url, Url}]).
-define(DEFAULT_TIMEOUT, application:get_env(hello, client_timeout, 10000)).

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
    gen_server:start_link(Name, ?MODULE, {URI, TransportOpts, ProtocolOpts, ClientOpts ++ [{name, Name}]}, []).

-spec start_link(URI :: string(), trans_opts(), protocol_opts(), client_opts()) -> start_result().
start_link(URI, TransportOpts, ProtocolOpts, ClientOpts) ->
    gen_server:start_link(?MODULE, {URI, TransportOpts, ProtocolOpts, ClientOpts}, []).

-spec start_link(client_name(), URI :: string(), trans_opts(), protocol_opts(), client_opts()) -> start_result(). 
start_link(Name, URI, TransportOpts, ProtocolOpts, ClientOpts) ->
    gen_server:start_link(Name, ?MODULE, {URI, TransportOpts, ProtocolOpts, ClientOpts ++ [{name, Name}]}, []).

%% API to start with hello supervisor
-spec start_supervised(URI :: string(), trans_opts(), protocol_opts(), client_opts()) -> 
    supervisor:startchild_ret(). 
start_supervised(URI, TransportOpts, ProtocolOpts, ClientOpts) ->
    hello_client_sup:start_client(URI, {TransportOpts, ProtocolOpts, ClientOpts}).

-spec start_supervised(atom(), URI :: string(), trans_opts(), protocol_opts(), client_opts()) -> 
    {ok, pid()}. 
start_supervised(Name, URI, TransportOpts, ProtocolOpts, ClientOpts) ->
    hello_client_sup:start_named_client(Name, URI, {TransportOpts, ProtocolOpts, ClientOpts ++ [{name, {Name}}]}).

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
    id :: term(),
    uri_rec :: #ex_uri{},
    transport_opts :: term(),
    transport_mod :: module(),
    transport_state :: term(),
    protocol_mod :: atom(),
    protocol_opts :: list(),
    protocol_state :: term(),
    client_opts :: term(),
    async_request_map = gb_trees:empty() :: gb_trees:tree(),
    keep_alive_interval :: number(),
    keep_alive_ref :: timer:tref(),
    waiting_for_pong = false :: boolean(),
    last_pong :: erlang:timestamp(),
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
handle_call({call, Call}, From, State = #client_state{protocol_mod = ProtocolMod,
                                        protocol_state = ProtocolState, id = ClientId}) ->
    case hello_proto:build_request(Call, ProtocolMod, ProtocolState) of
        {ok, Request, NewProtocolState} ->
            State1 = State#client_state{protocol_state = NewProtocolState},
            case outgoing_message(Request, From, State1) of
                {ok, State2} -> {noreply, State2};
                {ok, Reply, State2} -> {reply, Reply, State2};
                {error, Reason, State2} ->
                    ?LOG_INFO("Request from hello client '~p' on method(s) '~p' failed with reason '~p'.",
                        [ClientId, hello_log:get_method(Request), Reason], gen_meta_fields(Request, State2), ?LOGID00),
                    {reply, Reason, State2}
            end;
        {error, Reason, NewProtocolState} ->
            NewState = State#client_state{protocol_state = NewProtocolState},
            ?LOG_INFO("Creation of request from hello client '~p' failed for a call with reason '~p'.",
                        [ClientId, Reason], gen_meta_fields(Call, NewState), ?LOGID01),
            {reply, Reason, NewState}
    end;
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

%% @hidden
handle_info({?INCOMING_MSG, Message}, State) ->
    incoming_message(Message, State);
handle_info(?PING, State = #client_state{waiting_for_pong = true, keep_alive_interval = KeepAliveInterval,
                                         uri_rec = URIRec, transport_mod = TransportModule, 
                                         transport_opts = TransportOpts, protocol_opts = ProtocolOpts,
                                         transport_state = TransportState, client_opts = ClientOpts, 
                                         last_pong = LastPong, id = ClientId}) ->
    ?LOG_INFO("Error in hello client '~p': There is no PONG answer on PING for '~p' msec. Connection will be reestablished.",
               [ClientId, last_pong(LastPong, KeepAliveInterval)], gen_meta_fields(State), ?LOGID20),
    TransportModule:terminate_transport(lost_connection, TransportState),
    case init_transport(TransportModule, URIRec, TransportOpts, ProtocolOpts, ClientOpts, State) of
        {ok, NewState} -> {noreply, NewState};
        {stop, Reason} -> {stop, Reason, State}
    end;
handle_info(?PING, State = #client_state{transport_mod=TransportModule, transport_state=TransportState,
                                         keep_alive_interval = KeepAliveInterval,
                                         keep_alive_ref = TimerRef, id = ClientId}) ->
    {ok, NewTransportState} = TransportModule:send_request(?PING, ?INTERNAL_SIGNATURE, TransportState),
    ?LOG_DEBUG("Hello client '~p' sent PING request. Pinging server again in '~p' milliseconds.",
                [ClientId, KeepAliveInterval], gen_meta_fields(State), ?LOGID21),
    timer:cancel(TimerRef),
    {ok, NewTimerRef} = timer:send_after(KeepAliveInterval, self(), ?PING),
    {noreply, State#client_state{transport_state = NewTransportState, keep_alive_ref = NewTimerRef,
                                 waiting_for_pong = true}};

handle_info(Info, State = #client_state{transport_mod=TransportModule, transport_state=TransportState}) ->
    case TransportModule:handle_info(Info, TransportState) of
        {?INCOMING_MSG, Message} ->
            incoming_message(Message, State);
        {noreply, NewTransportState} ->
            {noreply, State#client_state{transport_state = NewTransportState}}
    end.

%% @hidden
terminate(Reason, #client_state{transport_mod = TransportModule, transport_state = TransportState, keep_alive_ref = TimerRef}) ->
    hello_metrics:client(-1),
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
    case init_transport(TransportModule, URIRec, TransportOpts, ProtocolOpts, ClientOpts, #client_state{}) of
        {ok, _} = OK -> hello_metrics:client(1), OK;
        Other -> Other
    end.

init_transport(TransportModule, URIRec, TransportOpts, ProtocolOpts, ClientOpts, State0) ->
    ClientId = get_client_id(ClientOpts),
    Url = ex_uri:encode(URIRec),
    ?LOG_INFO("Initializing hello client '~p' on '~p' ...", [ClientId, Url],
                ?HELLO_CLIENT_DEFAULT_META(ClientId, Url), ?LOGID02),
    case TransportModule:init_transport(URIRec, TransportOpts) of
        {ok, TransportState} ->
            NotificationSink = proplists:get_value(notification_sink, ProtocolOpts, undefined),
            ProtocolMod = proplists:get_value(protocol, ProtocolOpts, hello_proto_jsonrpc),
            case hello_proto:init_client(ProtocolMod, ProtocolOpts) of
                {ok, ProtocolState} ->
                    State = State0#client_state{
                                          uri_rec = URIRec,
                                          transport_opts = TransportOpts,
                                          transport_mod = TransportModule,
                                          transport_state = TransportState,
                                          protocol_mod = ProtocolMod,
                                          protocol_opts = ProtocolOpts,
                                          protocol_state = ProtocolState,
                                          notification_sink = NotificationSink, 
                                          waiting_for_pong = false},
                    evaluate_client_options(ClientOpts, State);
                {error, Reason} ->
                    ?LOG_INFO("Hello client '~p' is unable to initialize protocol because of reason '~p'.",
                                [ClientId, Reason], ?HELLO_CLIENT_DEFAULT_META(ClientId, Url), ?LOGID03),
                    {stop, Reason}
            end;
        {error, Reason} ->
            ?LOG_INFO("Hello client '~p' unable to initialize transport because of reason '~p'.",
                        [ClientId, Reason], ?HELLO_CLIENT_DEFAULT_META(ClientId, Url), ?LOGID04),
            {stop, Reason}
    end.

evaluate_client_options(ClientOpts, State0) ->
    State = State0#client_state{client_opts = ClientOpts},
    KeepAliveInterval = proplists:get_value(keep_alive_interval, ClientOpts, -1),
    ClientId = get_client_id(ClientOpts),
    if
        KeepAliveInterval =< 0 ->
            State1 = State#client_state{keep_alive_interval = -1, id = ClientId},
            ?LOG_DEBUG("Hello client '~p' initialized successfully.", [ClientId],
                        gen_meta_fields(State1), ?LOGID05),
            {ok, State1};
        KeepAliveInterval > 0 ->
            {ok, TimerRef} = timer:send_after(KeepAliveInterval, self(), ?PING),
            State1 = State#client_state{keep_alive_interval = KeepAliveInterval, keep_alive_ref = TimerRef, id = ClientId},
            ?LOG_DEBUG("Hello client '~p' initialized successfully with keep alive.", [ClientId], gen_meta_fields(State1), ?LOGID06),
            {ok, State1}
    end.

get_client_id(ClientOpts) ->
    ClientName = proplists:get_value(name, ClientOpts, {self()}),
    element(tuple_size(ClientName), ClientName).

incoming_message({error, Reason, NewTransportState}, State = #client_state{id = ClientId}) -> %%will be logged later
    ?LOG_DEBUG("Hello client '~p' received error notification from transport handler with reason '~p'.",
                [ClientId, Reason], gen_meta_fields(State), ?LOGID07),
    {noreply, State#client_state{transport_state = NewTransportState}};
incoming_message({ok, Signature, BinResponse, NewTransportState}, State = #client_state{async_request_map = AsyncMap,
                    protocol_mod = ProtocolMod, protocol_opts = ProtocolOpts, id = ClientId}) ->
    case hello_proto:decode(ProtocolMod, ProtocolOpts, Signature, BinResponse, response) of
        {ok, #response{id = null, response = Response}} ->
            ?LOG_DEBUG("Hello client '~p' received notification.", [ClientId],
                        gen_meta_fields(Response, State), ?LOGID08),
            notification(Response, State),
            {noreply, State#client_state{transport_state = NewTransportState}};
        {ok, Response = #response{}} ->
            ?LOG_DEBUG("Hello client '~p' received single response.", [ClientId],
                        gen_meta_fields(Response, State), ?LOGID09),
            request_reply(Response, AsyncMap, State);
        {ok, Responses = [{ok, #response{}} | _]} ->
            Responses1 = [R || {_, R} <- Responses],
            ?LOG_DEBUG("Hello client '~p' received batch response.", [ClientId],
                        gen_meta_fields(Responses1, State), ?LOGID10),
            NotificationResponses = [R || {_, #response{id = null} = R} <- Responses],
            NotificationResponses /= [] andalso notification([R || #response{response = R} <- NotificationResponses], State),
            Responses2 = Responses1 -- NotificationResponses,
            request_reply(Responses2, AsyncMap, State#client_state{transport_state = NewTransportState});
        {error, Reason} ->
            ?LOG_INFO("Hello client '~p' failed to decode response with reason '~p'.", [ClientId, Reason],
                        gen_meta_fields(BinResponse, State), ?LOGID11),
            {noreply, State#client_state{transport_state = NewTransportState}};
        ignore ->
            ?LOG_DEBUG("Hello client '~p' ignored decoding binary response.", [ClientId],
                        gen_meta_fields(BinResponse, State), ?LOGID12),
            {noreply, State#client_state{transport_state = NewTransportState}};
        {internal, Message} ->
            ?LOG_DEBUG("Hello client '~p' received internal message.", [ClientId],
                        gen_meta_fields(Message, State), ?LOGID13),
            {ok, State1} = ?MODULE:handle_internal(Message, State),
            {noreply, State1}
    end.

outgoing_message(Request, From, State = #client_state{protocol_mod = ProtocolMod, protocol_opts = ProtocolOpts,
                                                      transport_mod = TransportModule, transport_state = TransportState, 
                                                      async_request_map = AsyncMap, id = ClientId}) ->
    case hello_proto:encode(ProtocolMod, ProtocolOpts, Request) of
        {ok, BinRequest} ->
            Signature = hello_proto:signature(ProtocolMod, ProtocolOpts),
            case TransportModule:send_request(BinRequest, Signature, TransportState) of
                {ok, NewTransportState} ->
                    ?LOG_DEBUG("Hello client '~p' sent request on method(s) '~p'.", [ClientId, hello_log:get_method(Request)],
                                gen_meta_fields(Request, State), ?LOGID14),
                    maybe_noreply(NewTransportState, Request, From, AsyncMap, State);
                {error, Reason, NewTransportState} ->
                    ?LOG_INFO("Hello client '~p' attempted to send binary request on method(s) '~p' but failed with reason '~p'.",
                                [ClientId, hello_log:get_method(Request), Reason], gen_meta_fields(BinRequest, State), ?LOGID15),
                    {error, Reason, State#client_state{transport_state = NewTransportState}}
            end;
        {error, Reason, State} ->
            ?LOG_INFO("Hello client '~p' attempted to encode request on method '~p' but failed with reason '~p'.",
                        [ClientId, hello_log:get_method(Request), Reason], gen_meta_fields(Request, State), ?LOGID16),
            {error, Reason, State};
        ignore ->
            ?LOG_DEBUG("Hello client '~p' attempted to encode request on method '~p' but ignored sending request.", 
                        [ClientId, hello_log:get_method(Request), Request], gen_meta_fields(Request, State), ?LOGID17),
            {ok, State}
    end.

update_map(Requests, From, AsyncMap0) when is_list(Requests) -> 
    lists:foldl(fun(Request, AsyncMap) ->
                        update_map(Request, From, AsyncMap)
                end, AsyncMap0, Requests);
update_map(#request{id = undefined}, _From, AsyncMap) -> AsyncMap;
update_map(#request{id = RequestId} = Request, From, AsyncMap) -> gb_trees:enter(RequestId, {From, Request}, AsyncMap).

handle_internal(?PONG, State = #client_state{keep_alive_interval = KeepAliveInterval, keep_alive_ref = TimerRef, id = ClientId}) ->
    ?LOG_DEBUG("Hello client '~p' received PONG response. Pinging server again in '~p' milliseconds.",
                [ClientId, KeepAliveInterval], gen_meta_fields(?PONG, State), ?LOGID18),
    timer:cancel(TimerRef),
    {ok, NewTimerRef} = timer:send_after(KeepAliveInterval, self(), ?PING),
    {ok, State#client_state{keep_alive_ref = NewTimerRef, waiting_for_pong = false, last_pong = os:timestamp()}};
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

request_reply(Response, AsyncMap, State) ->
    case request_reply1(Response, AsyncMap) of
        {ok, NewAsyncMap} ->
            {noreply, State#client_state{async_request_map = NewAsyncMap}};
        {not_found, RequestId, NewAsyncMap} ->
            ?LOG_INFO("Hello client '~p' got response for non-existing request id '~p'.",
                            [RequestId], gen_meta_fields(Response, State), ?LOGID19),
            {noreply, State#client_state{async_request_map = NewAsyncMap}}
    end.

request_reply1(#response{} = Response, AsyncMap) ->
    request_reply1([Response], AsyncMap, []);
request_reply1(Responses, AsyncMap) ->
    request_reply1(Responses, AsyncMap, []).

request_reply1([], AsyncMap, [{CallRef, Response}]) ->
    gen_server:reply(CallRef, {ok, Response}),
    {ok, AsyncMap};
request_reply1([], AsyncMap, Responses) ->
    Refs = proplists:get_keys(Responses),
    lists:map(fun(CallRef) -> 
                NewAcc = lists:filtermap(fun({Ref, Response}) when Ref =:= CallRef -> 
                                                 {true, Response}; 
                                            (_) -> false end, lists:reverse(Responses)),
                gen_server:reply(CallRef, {ok, NewAcc})
              end, Refs),
    {ok, AsyncMap};
request_reply1([#response{id = RequestId, response = Response} | Tail], AsyncMap, Responses) ->
    case gb_trees:lookup(RequestId, AsyncMap) of
        {value, {CallRef, _Request}} ->
            NewAsyncMap = gb_trees:delete(RequestId, AsyncMap),
            request_reply1(Tail, NewAsyncMap, [{CallRef, Response} | Responses]);
        none ->
            {not_found, RequestId, AsyncMap}
    end.

gen_meta_fields(Requests = [ #request{} | _ ], State) ->
    lists:append(gen_meta_fields(State), [  {hello_request, hello_log:format(Requests)}, 
                                            {hello_request_method, hello_log:get_method(Requests)},
                                            {hello_request_id, hello_log:get_id(Requests)}]);
gen_meta_fields(Request = #request{}, State) ->
    lists:append(gen_meta_fields(State), [  {hello_request, hello_log:format(Request)},
                                            {hello_request_method, hello_log:get_method(Request)},
                                            {hello_request_id, hello_log:get_id(Request)}]);
gen_meta_fields(Responses = [ #response{} | _ ], State) ->
    lists:append(gen_meta_fields(State), [  {hello_response, hello_log:format(Responses)},
                                            {hello_request_id, hello_log:get_id(Responses)}]);
gen_meta_fields(Response = #response{}, State) ->
    lists:append(gen_meta_fields(State), [  {hello_response, hello_log:format(Response)},
                                            {hello_request_id, hello_log:get_id(Response)}]);
gen_meta_fields(Msg, State) ->
    lists:append(gen_meta_fields(State), [{hello_message, Msg}]).

gen_meta_fields(#client_state{transport_mod = TransportModule, transport_state = TransportState, id = ClientId}) ->
    lists:append([{hello_client_id, ClientId}], TransportModule:gen_meta_fields(TransportState)).

last_pong(undefined, Interval) -> Interval;
last_pong(Start, _) -> timer:now_diff(os:timestamp(), Start) / 1000.
