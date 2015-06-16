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
-export([notify/3]).
-export([behaviour_info/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("hello.hrl").
-include("hello_log.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").

%% ----------------------------------------------------------------------------------------------------
%% -- internal records
-record(state, {
    mod                     :: module(),
    state                   :: term(),
    context                 :: #context{},
    protocol                :: module(),
    async_reply_map         :: gb_tree:tree(),
    timer = #timer{}        :: #timer{},
    url                     :: #ex_uri{}
}).

%% ----------------------------------------------------------------------------------------------------
%% -- Callback Module API
-spec behaviour_info(callbacks) -> [{atom(), integer()}].
behaviour_info(callbacks) ->
    [{init,1},
     {handle_request,4},
     {handle_info,3},
     {terminate,3}
     ];
behaviour_info(_Other) ->
    undefined.

get_handler(Name, Identifier, HandlerMod, HandlerArgs) ->
    case hello_registry:lookup({handler, Name, Identifier}) of
        {error, not_found} ->
            ?LOG_DEBUG("Hadler for service ~p and identifier ~p not found, "
                      "it will have been started", [Name, Identifier]),
            start_handler(Identifier, HandlerMod, HandlerArgs);
        {ok, Handler} ->
            ?LOG_DEBUG("Found hadler ~p for service ~p and identifier ~p", [Handler, Name, Identifier]),
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
-spec notify(context(), hello_client:method(), [hello_json:value()]) -> ok.
notify(Context, Method, Params) when is_list(Params) ->
    hello_proto_jsonrpc:send_notification(Context, Method, Params).

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
%% @hidden
init({Identifier, HandlerMod, HandlerArgs}) ->
    case HandlerMod:init(Identifier, HandlerArgs) of
        Result when is_tuple(Result) and (element(1, Result) == ok) ->
            setelement(2, Result, #state{mod = HandlerMod, state = element(2, Result), async_reply_map = gb_trees:empty()});
        Other ->
            Other
    end.

%% @hidden
handle_cast({request, Request = #request{context = Context}}, State = #state{mod = Mod, state = _HandlerState}) ->
    % TODO: fix logging, request should be loged with answer. If no answer, should be logged, no answer
    try do_request(Request, State)
    catch
        Error:Reason ->
            send(Context, {error, {server_error, "handler error", undefined}}),
            ?LOG_REQUEST_bad_request(Mod, self(), Request, {Error, Reason, erlang:get_stacktrace()}),
            {stop, normal, State}
    end;
handle_cast({set_idle_timeout, Timeout}, State = #state{timer = Timer}) ->
    NewTimer = Timer#timer{idle_timeout = Timeout},
    {noreply, reset_idle_timeout(State#state{timer = NewTimer})};
handle_cast({async_reply, ReqContext, Result}, State = #state{async_reply_map = AsyncMap, mod = Mod}) ->
    #context{req_ref = ReqRef} = ReqContext,
    case gb_trees:lookup(ReqRef, AsyncMap) of
        {value, Request} ->
            ?LOG_REQUEST_async_reply(Mod, self(), Request, {ok, Result}),
            send(ReqContext, {ok, Result}),
            {noreply, State#state{async_reply_map = gb_trees:delete(ReqRef, AsyncMap)}};
        none ->
            error_logger:warning_report([{hello_handler, State#state.mod},
                                         {unknown_async_reply, ReqRef, Result}]),
            {noreply, State}
    end.

%% @hidden
handle_call(_Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @hidden
handle_info({?IDLE_TIMEOUT_MSG, TimerRef}, State = #state{timer = Timer}) when Timer#timer.idle_timeout_ref == TimerRef ->
    NewTimer = Timer#timer{stopped_because_idle = true},
    {stop, normal, State#state{timer = NewTimer}};
handle_info({?IDLE_TIMEOUT_MSG, OtherRef}, State = #state{mod = Mod}) ->
    error_logger:warning_report([{hello_handler, Mod}, {unknown_timeout_msg, OtherRef}]),
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
do_request(Request = #request{context = Context}, State = #state{async_reply_map = AsyncMap, mod = Mod, state = HandlerState}) ->
    ReqRef = make_ref(),
    HandlerPid = self(),
    Context1 = Context#context{req_ref = ReqRef, handler_pid = HandlerPid},
    case hello_validate:validate_request(Request, Mod) of
        {ok, ValMethod, ValParams} ->
            {Time, Value} = timer:tc(Mod, handle_request, [Context1, ValMethod, ValParams, HandlerState]),
            TimeMS = Time / 1000, % in ms
            case Value of
                {reply, Response, NewHandlerState} ->
                    ?LOG_REQUEST_request(Mod, HandlerPid, Request, Response, TimeMS),
                    send(Context1, Response),
                    {noreply, State#state{state = NewHandlerState}};
                {noreply, NewModState} ->
                    ?LOG_REQUEST_request_no_reply(Mod, HandlerPid, Request, TimeMS),
                    {noreply, State#state{state = NewModState, async_reply_map = gb_trees:enter(ReqRef, Request, AsyncMap)}};
                {stop, Reason, Response, NewModState} ->
                    ?LOG_REQUEST_request_stop(Mod, HandlerPid, Request, Response, Reason, TimeMS),
                    send(Context1, Response),
                    {stop, Reason, State#state{state = NewModState}};
                {stop, Reason, NewModState} ->
                    ?LOG_REQUEST_request_stop_no_reply(Mod, HandlerPid, Request, Reason, TimeMS),
                    {stop, Reason, State#state{mod = NewModState}};
                {stop, NewModState} ->
                    ?LOG_REQUEST_request_stop_no_reply(Mod, HandlerPid, Request, TimeMS),
                    {stop, State#state{mod=NewModState}};
                {ignore, NewModState} ->
                    ?LOG_REQUEST_request(Mod, HandlerPid, Request, ignore, TimeMS),
                    {noreply, State#state{state = NewModState}}
            end;
        {error, {_Code, _Message, _Data} = Reason} ->
            ?LOG_REQUEST_bad_request(Mod, HandlerPid, Request, Reason),
            send(Context1,  {error, Reason}),
            {stop, normal, State};
        _FalseAnswer ->
            ?LOG_REQUEST_bad_request(Mod, HandlerPid, Request, _FalseAnswer),
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
