% Copyright (c) 2011-2012 by Travelping GmbH <info@travelping.com>

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


% -callback init(context(), InitArgs :: term()) -> {ok, State :: term()}
%                                                | {error, hello_proto:error_reply()}.
%
% -callback handle_request(from(), method(), params(), State :: term()) -> {reply, reply(), NewState}
%                                                                        | {noreply, NewState}
%                                                                        | {stop, Reason, NewState}
%                                                                        | {stop, Reason, reply(), NewState}.
%
% -callback handle_info(context(), InfoMsg :: term(), State :: term()) -> {noreply, NewState}
%                                                                       | {stop, Reason, NewState}
%
% -callback terminate(context(), Reason :: term(), State :: term()) -> term().

%% @doc Stateful RPC handler behaviour.
-module(hello_stateful_handler).
-export([behaviour_info/1, set_idle_timeout/1, set_idle_timeout/2, notify/3,
         notify_np/3, reply/2, transport_param/2, transport_param/3]).
-export_type([context/0, from_context/0]).

%% from listener
-export([start_link/4]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("internal.hrl").
-include("hello.hrl").

-record(context, {
    protocol  :: module(),
    peer      :: term(),
    transport :: pid(),
    transport_params :: hello:transport_params()
}).

-record(from_context, {
    handler_pid :: pid(),
    req_ref     :: reference() | notification,
    reqid       :: any(),
    context     :: #context{}
}).

-opaque context() :: #context{}.
-opaque from_context() :: #from_context{}.

%% ----------------------------------------------------------------------------------------------------
%% -- Callback Module API
-spec behaviour_info(callbacks) -> [{atom(), integer()}].
behaviour_info(callbacks) ->
    [{init,2}, {handle_request,4}, {handle_info,3}, {terminate,3}];
behaviour_info(_Other) ->
    undefined.

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

%% @doc Send an RPC notification with positional parameters to the client.
-spec notify(context() | from_context(), hello_client:method(), [hello_json:value()]) -> ok.
notify(Context, Method, Args) when is_list(Args) ->
    send_notification(Context, Method, Args).

%% @doc Send an RPC notification with named parameters to the client.
-spec notify_np(context() | from_context(), hello_client:method(), [{string(), hello_json:value()}] | hello_json:json_object()) -> ok.
notify_np(Context, Method, Args) when is_list(Args) ->
    notify_np(Context, Method, {Args});
notify_np(Context, Method, Args = {_}) ->
    send_notification(Context, Method, Args).

%% @doc Send an asynchronous reply to an RPC request.
%%   If you want to use asynchronous replies,  your handler module must
%%   return ``{noreply, State}'' from ``handle_request/4''.
%%   ``reply/2'' can be called from any process, not just the handler process.
-spec reply(from_context(), Reply) -> ok
    when Reply :: {ok, hello_json:value()} | Error,
         Error :: {error, ErrorCode} | {error, ErrorCode, ErrorMessage} | {error, ErrorCode, ErrorMessage, ErrorData},
         ErrorCode :: integer(),
         ErrorMessage :: binary(),
         ErrorData :: hello_json:value().
reply(#from_context{reqid = undefined}, _Result) ->
    ok;
reply(#from_context{handler_pid = Pid, req_ref = ReqRef, reqid = ReqId}, Result) ->
    gen_server:cast(Pid, {async_reply, ReqRef, ReqId, Result}).

%% @equiv transport_param(Key, Context, undefined)
-spec transport_param(context() | from_context(), atom()) -> any().
transport_param(Key, Context) ->
    transport_param(Key, Context, undefined).

%% @doc Get the value of a transport parameter.
%%   The list of supported transport parameters will vary between transport implementations.<br/>
%%   If you rely on transport parameters, your RPC handler <b>will no longer be transport-agnostic</b>.
%%   Please take this into consideration when designing your system.
-spec transport_param(Key::atom(), context() | from_context(), Default::any()) -> any().
transport_param(Key, #context{transport_params = Params}, Default) ->
    proplists:get_value(Key, Params, Default);
transport_param(Key, #from_context{context = #context{transport_params = Params}}, Default) ->
    proplists:get_value(Key, Params, Default).

%% ----------------------------------------------------------------------------------------------------
%% -- for listener communication
%% @private
-spec start_link(#binding{}, term(), pid(), hello:transport_params()) -> {ok, pid()}.
start_link(Binding, Peer, Transport, TransportParams) ->
    gen_server:start_link(?MODULE, {Binding, Peer, Transport, TransportParams}, []).

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
-define(IDLE_TIMEOUT_MSG, '$hello_idle_timeout').
-record(state, {
    context                 :: #context{},
    mod                     :: module(),
    mod_state               :: term(),
    async_reply_map         :: gb_tree(),
    log_url                 :: binary(),
    idle_timer              :: term(), %% the current timer
    idle_timeout_ref        :: reference(),
    idle_timeout = infinity :: timeout(),
    stopped_because_idle = false :: boolean()
}).

%% @hidden
init({Binding, Peer, TransportPid, TransportParams}) ->
    link(TransportPid),
    #binding{callbacks=Callbacks} = Binding,
    [{_, [Callback]} | _] = dict:to_list(Callbacks),
    CallbackMod = Callback#callback.mod,
    CallbackArgs = Callback#callback.args,
    Context = #context{protocol = hello_proto_jsonrpc,
                       peer = Peer,
                       transport = TransportPid,
                       transport_params = TransportParams},
    State0 = #state{mod = CallbackMod,
                    log_url = Binding#binding.log_url,
                    context = Context,
                    async_reply_map = gb_trees:empty()},
    State1 = start_idle_timeout(State0),
    case CallbackMod:init(Context, CallbackArgs) of
        {ok, HandlerState} ->
            {ok, State1#state{mod_state = HandlerState}}
    end.

%% @hidden
handle_cast({set_idle_timeout, Timeout}, State) ->
    {noreply, reset_idle_timeout(State#state{idle_timeout = Timeout})};
handle_cast({async_reply, ReqRef, ReqId, Result}, State = #state{async_reply_map = ARMap}) ->
    case gb_trees:lookup(ReqRef, ARMap) of
        {value, {incomplete_request, Req}} ->
            Resp = to_response_record(Result, Req),
            proto_send_log(Req, Resp, State),
            {noreply, State#state{async_reply_map = gb_trees:delete(ReqRef, ARMap)}};
        {value, {incomplete_batch, BatchReq = #batch_request{requests = Reqs}, Responses}} ->
            case lists:partition(fun (#request{reqid = Id}) -> Id == ReqId end, Reqs) of
                {[LastReq], []} ->
                    BatchResp = hello_proto:batch_response(BatchReq, [to_response_record(Result, LastReq) | Responses]),
                    proto_send_log(BatchReq, BatchResp, State),
                    {noreply, State#state{async_reply_map = gb_trees:delete(ReqRef, ARMap)}};
                {[Req], StillWaiting} ->
                    NewBatchReq = BatchReq#batch_request{requests = StillWaiting},
                    NewIncompleteBatch = {incomplete_batch, NewBatchReq, [to_response_record(Result, Req) | Responses]},
                    NewARMap = gb_trees:enter(ReqRef, NewIncompleteBatch, ARMap),
                    {noreply, State#state{async_reply_map = NewARMap}}
            end;
        none ->
            error_logger:warning_report([{hello_stateful_handler, State#state.mod},
                                         {unknown_reply, ReqRef, ReqId, Result}]),
            {noreply, State}
    end.

%% @hidden
handle_call(_Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @hidden
handle_info({?IDLE_TIMEOUT_MSG, Ref}, State = #state{idle_timeout_ref = Ref}) ->
    {stop, normal, State#state{stopped_because_idle = true}};
handle_info({?IDLE_TIMEOUT_MSG, _OtherRef}, State) ->
    %% ignore timeouts other than the current one
    {noreply, State};
handle_info({?INCOMING_MSG_MSG, Message}, State0) ->
    State = reset_idle_timeout(State0),
    case hello_proto:decode(hello_proto_jsonrpc, Message) of
        Req = #request{} ->
            do_single_request(Req, State);
        Req = #batch_request{} ->
            do_batch(Req, State);
        #error{} ->
            {noreply, State};
        #response{} ->
            {noreply, State};
        {proto_reply, Response} ->
            hello_request_log:bad_request(State#state.mod, self(), State#state.log_url, Message, Response),
            proto_send(Response, State),
            {noreply, State}
    end;
handle_info(InfoMsg, State = #state{mod = Mod, mod_state = ModState, context = Context}) ->
    case Mod:handle_info(Context, InfoMsg, ModState) of
        {noreply, NewModState} ->
            {noreply, State#state{mod_state = NewModState}};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#state{mod_state = NewModState}}
    end.

%% @hidden
terminate(Reason, State = #state{mod = Mod, mod_state = ModState, context = Context}) ->
    %% there is a nasty error report if we exit with reason /= normal, that's why we fake the reason for idle_timeout
    case {Reason, State#state.stopped_because_idle} of
        {normal, true} ->
            Mod:terminate(Context, idle_timeout, ModState);
        {_, _} ->
            Mod:terminate(Context, Reason, ModState)
    end,
    send_closed(Context).

%% TODO: code_change
%% @hidden
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

%% --------------------------------------------------------------------------------
%% -- internal functions
do_single_request(Req, State = #state{mod_state = ModState, mod = Mod}) ->
    case hello_validate:request({Mod, ModState}, Req) of
	{error, Error} ->
            proto_send_log(Req, Error, State),
            {noreply, State};
	{ok, Method, ValidatedParams} when is_list(ValidatedParams) ->
	    ReqRef  = make_ref(),
	    Context = make_from_context(ReqRef, Req, State),
	    case Mod:handle_request(Context, Method, ValidatedParams, ModState) of
		%% with reply
		{reply, Reply, NewModState} ->
		    proto_send_log(Req, to_response_record(Reply, Req), State),
		    {noreply, State#state{mod_state = NewModState}};
		{stop, Reason, Reply, NewModState} ->
		    proto_send_log(Req, to_response_record(Reply, Req), State),
		    {stop, Reason, State#state{mod_state = NewModState}};
		%% without reply
		{noreply, NewModState} ->
		    case Req#request.reqid of
			undefined ->
			    proto_send_log(Req, ignore, State),
			    %% no need to wait for an async reply if request is a notification
			    {noreply, State#state{mod_state = NewModState}};
			_ ->
			    NewARMap = gb_trees:enter(ReqRef, {incomplete_request, Req}, State#state.async_reply_map),
			    {noreply, State#state{mod_state = NewModState, async_reply_map = NewARMap}}
		    end;
		{stop, Reason, NewModState} ->
		    {stop, Reason, State#state{mod_state = NewModState}}
	    end
    end.

do_batch(BatchReq = #batch_request{requests = Requests}, State = #state{async_reply_map = ReplyMap}) ->
    BatchRef = make_ref(),
    case do_batch_requests(Requests, BatchRef, State, [], []) of
        {stop, Reason, Responses, NewState} ->
            proto_send_log(BatchReq, hello_proto:batch_response(BatchReq, Responses), NewState),
            {stop, Reason, NewState};
        {reply, Responses, [], NewState} ->
            proto_send_log(BatchReq, hello_proto:batch_response(BatchReq, Responses), NewState),
            {noreply, NewState};
        {reply, Responses, AsyncReplies, NewState} ->
            StoredReq = BatchReq#batch_request{requests = AsyncReplies},
            NewARMap = gb_trees:enter(BatchRef, {incomplete_batch, StoredReq, Responses}, ReplyMap),
            {noreply, NewState#state{async_reply_map = NewARMap}}
    end.

maybe_async_reply(AsyncReplies, #request{reqid = undefined}) ->
    AsyncReplies;
maybe_async_reply(AsyncReplies, Req = #request{}) ->
    [Req | AsyncReplies].

do_batch_requests([], _BR, State, Responses, NeedAsyncReply) ->
    {reply, Responses, NeedAsyncReply, State};
do_batch_requests([Req = #request{} | Rest], BatchRef, State = #state{mod = Mod}, Responses, AsyncReplies) ->
    ModState = State#state.mod_state,
    case hello_validate:request({Mod, ModState}, Req) of
	{error, ErrorResp} ->
            do_batch_requests(Rest, BatchRef, State, [ErrorResp | Responses], AsyncReplies);
	{ok, Method, ValidatedParams} when is_list(ValidatedParams) ->
	    Context = make_from_context(BatchRef, Req, State),
	    case Mod:handle_request(Context, Method, ValidatedParams, ModState) of
		%% with reply
		{reply, Reply, NewModState} ->
		    NewResps = [to_response_record(Reply, Req) | Responses],
		    do_batch_requests(Rest, BatchRef, State#state{mod_state = NewModState},
				      NewResps, AsyncReplies);
		{stop, Reason, Reply, NewModState} ->
		    NewResps = [to_response_record(Reply, Req) | Responses],
		    stop_in_batch(Reason, State#state{mod_state = NewModState},
				  NewResps, AsyncReplies);
		%% without reply
		{noreply, NewModState} ->
		    do_batch_requests(Rest, BatchRef, State#state{mod_state = NewModState},
				      Responses, maybe_async_reply(AsyncReplies, Req));
		{stop, Reason, NewModState} ->
		    stop_in_batch(Reason, State#state{mod_state = NewModState},
				  Responses, maybe_async_reply(AsyncReplies, Req))
            end
    end.

stop_in_batch(Reason, State, Responses, []) ->
    {stop, Reason, Responses, State};
stop_in_batch(Reason, State, Responses, [LeftoverReq | Rest]) ->
    ErrorResp = hello_proto:error_response(LeftoverReq, server_error, <<"handler stopped">>),
    stop_in_batch(Reason, State, [ErrorResp | Responses], Rest).

to_response_record({ok, Reply}, Request) ->
    hello_proto:success_response(Request, Reply);
to_response_record({error, Error}, Request) ->
    hello_proto:error_response(Request, Error);
to_response_record({error, Code, Message}, Request) ->
    hello_proto:error_response(Request, {Code, Message});
to_response_record({error, Code, Message, Data}, Request) ->
    hello_proto:error_response(Request, {Code, Message, Data}).

-spec proto_send_log(#state{}, hello_proto:request(), hello_proto:response()) -> any().
proto_send_log(Req, Resp, State) ->
    hello_request_log:request(State#state.mod, self(), State#state.log_url, Req, Resp),
    proto_send(Resp, State).

proto_send(ignore, _State) ->
    ok;
proto_send(Result, #state{context = Context}) ->
    send_binary(Context, hello_proto:encode(Result)).

send_notification(#from_context{context = Context}, Method, Params) ->
    send_notification(Context, Method, Params);
send_notification(Context = #context{protocol = Protocol}, Method, Params) ->
    Request = hello_proto:new_notification(Protocol, Method, Params),
    send_binary(Context, hello_proto:encode(Request)).

send_binary(#context{transport = TPid, peer = Peer}, Message) when is_binary(Message) ->
    TPid ! {hello_msg, self(), Peer, Message}.

send_closed(#context{transport = TPid, peer = Peer}) ->
    TPid ! {hello_closed, self(), Peer}.

-spec make_from_context(reference(), #request{}, #state{}) -> #from_context{}.
make_from_context(_ReqRef, #request{reqid = undefined}, State) ->
    #from_context{reqid = undefined, context = State#state.context};
make_from_context(ReqRef, #request{reqid = ReqId}, State) ->
    #from_context{handler_pid = self(), req_ref = ReqRef, reqid = ReqId, context = State#state.context}.

reset_idle_timeout(State) ->
    case State#state.idle_timer of
        undefined ->
            start_idle_timeout(State);
        OldTimer ->
            _ = erlang:cancel_timer(OldTimer),
            start_idle_timeout(State)
    end.

start_idle_timeout(State) ->
    case State#state.idle_timeout of
        infinity ->
            State#state{idle_timer = undefined, idle_timeout_ref = undefined};
        IdleTimeout ->
            IdleTimeoutRef = make_ref(),
            NewTimer = erlang:send_after(IdleTimeout, self(), {?IDLE_TIMEOUT_MSG, IdleTimeoutRef}),
            State#state{idle_timer = NewTimer, idle_timeout_ref = IdleTimeoutRef}
    end.
