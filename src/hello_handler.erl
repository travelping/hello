% Copyright (c) 2011-2014 by Travelping GmbH <info@travelping.com>

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


% -callback init( binding() -> {ok, State :: term()}.
%
% -callback handle_request(context(), method(), params(), State :: term()) -> {reply, reply(), NewState}
%                                                                        | {noreply, NewState}
%                                                                        | {stop, Reason, NewState}
%                                                                        | {stop, Reason, reply(), NewState}.
%
% -callback handle_info(context(), Info :: term(), State :: term()) -> term().
%
% -callback terminate(context(), Reason :: term(), State :: term()) -> term().

%% @doc RPC handler behaviour.
-module(hello_handler).
-export([get_handler/4, process/2,
         set_idle_timeout/1,
         set_idle_timeout/2,
         reply/2
         ]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("hello.hrl").
-include("hello_log.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").

%% ----------------------------------------------------------------------------------------------------
%% -- internal records
-record(state, {
    id                      :: term(),
    mod                     :: module(),
    state                   :: term(),
    context                 :: #context{},
    protocol                :: module(),
    async_reply_map         :: gb_trees:tree(),
    timer = #timer{}        :: #timer{},
    url                     :: #ex_uri{}
}).

%% ----------------------------------------------------------------------------------------------------
%% -- Callback Module API
-type handle_request_response() :: {error, Reason :: term()} | {error, integer(), iodata(), term()} | {ok, term()}.

-callback init(Identifier :: term(), handler_opts()) -> 
    {ok, State :: term()} | term().

-callback handle_request(Context :: context(), Method :: binary(), Args :: list(), State :: term()) -> 
    {reply, Response :: handle_request_response(), NewState :: term()} |
    {noreply, NewState :: term()} |
    {stop, Reason :: term(), Response :: handle_request_response(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()} |
    {stop, NewState :: term()} |
    {ignore, NewState :: term()}.

-callback handle_info(Context :: context(), Message :: term(), State :: term()) -> 
    {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-callback terminate(Context :: context(), Reason :: term(), State :: term()) -> ok.


get_handler(Name, Identifier, HandlerMod, HandlerArgs) ->
    case hello_registry:lookup({handler, Name, Identifier}) of
        {error, not_found} ->
            ?LOG_DEBUG("Handler for service ~p and identifier ~p not found. Starting handler.",
                        [Name, Identifier], [], ?LOGID22),
            start_handler(Identifier, HandlerMod, HandlerArgs);
        {ok, _, Handler} ->
            ?LOG_DEBUG("Found handler ~p for service ~p and identifier ~p.", [Handler, Name, Identifier], [], ?LOGID23),
            Handler
    end.

start_handler(Identifier, HandlerMod, HandlerArgs) ->
    {ok, Handler} = gen_server:start(?MODULE, {Identifier, HandlerMod, HandlerArgs}, []),
    Handler.

process(Handler, Request) ->
    gen_server:cast(Handler, {request, Request}).

%% @equiv set_idle_timeout(self(), Timeout)
-spec set_idle_timeout(timeout()) -> ok.
set_idle_timeout(Timeout) ->
    set_idle_timeout(self(), Timeout).

%% @doc Set the idle timeout of a handler.
%%   The handler will exit when no request arrives before the given timeout.
%%   This only needs to be set once, any incoming request will restart the timer.
-spec set_idle_timeout(pid(), timeout()) -> ok.
set_idle_timeout(HandlerPid, Timeout) ->
    gen_server:cast(HandlerPid, {set_idle_timeout, Timeout}).

%% @doc Send an asynchronous reply to an RPC request.
%%   If you want to use asynchronous replies,  your handler module must
%%   return ``{noreply, State}'' from ``handle_request/4''.
%%   ``reply/2'' can be called from any process, not just the handler process.
reply(ReqContext = #context{handler_pid = HandlerPid}, Result) ->
    gen_server:cast(HandlerPid, {async_reply, ReqContext, Result}).

%% --------------------------------------------------------------------------------
%% -- jsonrpc specific stuff
%% @doc Send an RPC notification with positional or named (given as proplist) parameters to the client.
%-spec notify(context(), hello_client:method(), [hello_json:value()]) -> ok.
%notify(Context, Method, Params) when is_list(Params) ->
    %hello_proto_jsonrpc:send_notification(Context, Method, Params).

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
%% @hidden
init({Identifier, HandlerMod, HandlerArgs}) ->
    case HandlerMod:init(Identifier, HandlerArgs) of
        {ok, Handlerstate} ->
            {ok, #state{mod = HandlerMod, state = Handlerstate,
                        async_reply_map = gb_trees:empty(), id = Identifier}};
        Other ->
            Other
    end.

%% @hidden
handle_cast({request, Request = #request{context = Context}},
                State = #state{mod = Mod, state = _HandlerState, id = Id}) ->
    % TODO: fix logging, request should be loged with answer. If no answer, should be logged, no answer
    try do_request(Request, State)
    catch
        Error:Reason ->
            send(Context, {error, {server_error, "handler error", undefined}}),
            ?LOG_REQUEST_bad_request(Mod, Id, Request, {Error, Reason, erlang:get_stacktrace()}, ?LOGID24),
            {stop, normal, State}
    end;
handle_cast({set_idle_timeout, Timeout}, State = #state{timer = Timer}) ->
    NewTimer = Timer#timer{idle_timeout = Timeout},
    {noreply, reset_idle_timeout(State#state{timer = NewTimer})};
handle_cast({async_reply, ReqContext, Result}, State = #state{id = Id, async_reply_map = AsyncMap, mod = Mod}) ->
    #context{req_ref = ReqRef} = ReqContext,
    case gb_trees:lookup(ReqRef, AsyncMap) of
        {value, Request} ->
            ?LOG_REQUEST_async_reply(Mod, Id, Request, Result, ?LOGID25),
            send(ReqContext, {ok, Result}),
            {noreply, State#state{async_reply_map = gb_trees:delete(ReqRef, AsyncMap)}};
        none ->
            ?LOG_WARNING_reason(Mod, Id, "Hello handler with callback module '~p' and service id '~p' got unknown async reply.",
                [Mod, Id], {unknown_async_reply, {ReqRef, Result}}, ?LOGID26),
            {noreply, State}
    end.

%% @hidden
handle_call(_Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @hidden
handle_info({?IDLE_TIMEOUT_MSG, TimerRef}, State = #state{timer = Timer, id = Id, mod = Mod})
    when Timer#timer.idle_timeout_ref == TimerRef ->
    ?LOG_WARNING_reason(Mod, Id, "Hello handler with callback module '~p' and service id '~p' is going to stop due to idle timeout.",
        [Mod, Id], {error, idle_timeout}, ?LOGID27),
    NewTimer = Timer#timer{stopped_because_idle = true},
    {stop, normal, State#state{timer = NewTimer}};
handle_info({?IDLE_TIMEOUT_MSG, OtherRef}, State = #state{mod = Mod, id = Id}) ->
    ?LOG_WARNING_reason(Mod, Id, "Hello handler with callback module '~p' and service id '~p' received unknown idle timeout message.",
        [Mod, Id], {error, {unknown_timeout_message, OtherRef}}, ?LOGID28),
    {noreply, State};
handle_info({?INCOMING_MSG, Request = #request{context = Context}}, State0) ->
    State = reset_idle_timeout(State0),
    do_request(Request, State#state{context = Context});
handle_info({terminate, normal}, State) ->
    {stop, normal, State};
handle_info(InfoMsg, State = #state{mod = Mod, state = ModState, context = Context}) ->
    case Mod:handle_info(Context, InfoMsg, ModState) of
        {noreply, NewModState} ->
            {noreply, State#state{state = NewModState}};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#state{state = NewModState}}
    end.

%% @hidden
terminate(Reason, _State = #state{mod = Mod, state = ModState, context = Context, timer = Timer}) ->
    case {Reason, Timer#timer.stopped_because_idle} of
        {normal, true} ->
            Mod:terminate(Context, idle_timeout, ModState);
        {_, _} ->
            Mod:terminate(Context, Reason, ModState)
    end.

%% TODO: code_change
%% @hidden
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

%% --------------------------------------------------------------------------------
%% -- internal functions
%% @hidden
do_request(Request = #request{context = Context},
            State = #state{async_reply_map = AsyncMap, mod = Mod, state = HandlerState, id = Id}) ->
    ReqRef = make_ref(),
    HandlerPid = self(),
    Context1 = Context#context{req_ref = ReqRef, handler_pid = HandlerPid},
    case hello_validate:validate_request(Request, Mod) of
        {ok, ValMethod, ValParams} ->
            {Time, Value} = timer:tc(Mod, handle_request, [Context1, ValMethod, ValParams, HandlerState]),
            TimeMS = Time / 1000, % in ms
            hello_metrics:handle_request_time(TimeMS),
            case Value of
                {reply, Response, NewHandlerState} ->
                    ?LOG_REQUEST_request(Mod, Id, Request, Response, TimeMS, ?LOGID29),
                    send(Context1, Response),
                    {noreply, State#state{state = NewHandlerState}};
                {noreply, NewModState} ->
                    ?LOG_REQUEST_request_no_reply(Mod, Id, Request, TimeMS, ?LOGID30),
                    {noreply, State#state{state = NewModState, async_reply_map = gb_trees:enter(ReqRef, Request, AsyncMap)}};
                {stop, Reason, Response, NewModState} ->
                    ?LOG_REQUEST_request_stop(Mod, Id, Request, Response, Reason, TimeMS, ?LOGID31),
                    send(Context1, Response),
                    {stop, Reason, State#state{state = NewModState}};
                {stop, Reason, NewModState} ->
                    ?LOG_REQUEST_request_stop_no_reply(Mod, Id, Request, Reason, TimeMS, ?LOGID32),
                    {stop, Reason, State#state{mod = NewModState}};
                {stop, NewModState} ->
                    ?LOG_REQUEST_request_stop_no_reply(Mod, Id, Request, TimeMS, ?LOGID33),
                    {stop, State#state{mod=NewModState}};
                {ignore, NewModState} ->
                    ?LOG_REQUEST_request(Mod, Id, Request, ignore, TimeMS, ?LOGID29),
                    {noreply, State#state{state = NewModState}}
            end;
        {error, {_Code, _Message, _Data} = Reason} ->
            ?LOG_REQUEST_bad_request(Mod, Id, Request, Reason, ?LOGID24),
            send(Context1,  {error, Reason}),
            {stop, normal, State};
        _FalseAnswer ->
            ?LOG_REQUEST_bad_request(Mod, Id, Request, _FalseAnswer, ?LOGID24),
            send(Context1, {error, {server_error, "validation returned wrong error format", null}}),
            {stop, normal, State}
    end.

%do_request(Request, _State = #state{mod = Mod, state = ModState}) ->
%    Context = Request#request.context,
%    ReqContext = #request_context{req_ref = make_ref(), handler_pid = self(), context = Context, protocol_info = undefined},
%    hello_proto:do_request(?MODULE, Mod, ModState, ReqContext, Request).

%% --------------------------------------------------------------------------------
%% -- finally used to send back a response message to hello_binding
%% @hidden
send(Context, Response) ->
    hello_service:outgoing_message(Context, Response).

%% --------------------------------------------------------------------------------
%% -- helpers for timer functionality
%% @hidden
reset_idle_timeout(State = #state{timer = Timer}) ->
    case Timer#timer.idle_timer of
        undefined ->
            start_idle_timeout(State);
        OldTimer ->
            _ = erlang:cancel_timer(OldTimer),
            start_idle_timeout(State)
    end.

%% @hidden
start_idle_timeout(State = #state{timer = Timer}) ->
    case Timer#timer.idle_timeout of
        infinity ->
            NewTimer = Timer#timer{idle_timer = undefined, idle_timeout_ref = undefined},
            State#state{timer = NewTimer};
        IdleTimeout ->
            IdleTimeoutRef = make_ref(),
            IdleTimer = erlang:send_after(IdleTimeout, self(), {?IDLE_TIMEOUT_MSG, IdleTimeoutRef}),
            NewTimer = Timer#timer{idle_timer = IdleTimer, idle_timeout_ref = IdleTimeoutRef},
            State#state{timer = NewTimer}
    end.
