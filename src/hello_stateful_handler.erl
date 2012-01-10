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

-module(hello_stateful_handler).
-export([behaviour_info/1, incoming_message/2]).
-export([start_link/3, set_idle_timeout/1, set_idle_timeout/2, notify/3, notify_np/3, reply/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("internal.hrl").
-include("hello.hrl").

-record(context, {
    transport :: pid(),
    peer      :: term()
}).

-record(from_context, {
    handler_pid :: pid(),
    req_ref     :: reference() | notification,
    reqid       :: any(),
    context     :: #context{}
}).

-spec behaviour_info(callbacks) -> [{atom(), integer()}].
behaviour_info(callbacks) ->
    [{method_info,1}, {param_info,2}, {init,2}, {handle_request,4}, {handle_info,3}, {terminate,3}];
behaviour_info(_Other) ->
    undefined.

-spec start_link(#binding{}, term(), pid()) -> {ok, pid()}.
start_link(#binding{callback_mod = Mod, callback_args = Args}, Peer, Transport) ->
    gen_server:start_link(?MODULE, {Transport, Peer, Mod, Args}, []).

%% @private
-spec incoming_message(pid(), binary()) -> ok.
incoming_message(HandlerPid, Message) ->
    gen_server:cast(HandlerPid, {incoming_message, Message}).

set_idle_timeout(Timeout) ->
    set_idle_timeout(self(), Timeout).

set_idle_timeout(HandlerPid, Timeout) ->
    gen_server:cast(HandlerPid, {set_idle_timeout, Timeout}).

notify(Context, Method, Args) when is_list(Args) ->
    send_notification(Context, Method, Args).

notify_np(Context, Method, Args) when is_list(Args) ->
    notify_np(Context, Method, {Args});
notify_np(Context, Method, Args = {_}) ->
    send_notification(Context, Method, Args).

reply(#from_context{reqid = undefined}, _Result) ->
    ok;
reply(#from_context{handler_pid = Pid, req_ref = ReqRef, reqid = ReqId}, Result) ->
    gen_server:cast(Pid, {async_reply, ReqRef, ReqId, Result}).

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
-define(IDLE_TIMEOUT_MSG, '$hello_idle_timeout').
-record(state, {
    context                 :: #context{},
    mod                     :: module(),
    mod_state               :: term(),
    async_reply_map         :: gb_tree(),
    idle_timer              :: term(), %% the current timer
    idle_timeout_ref        :: reference(),
    idle_timeout = infinity :: timeout(),
    stopped_because_idle = false :: boolean()
}).

init({TransportPid, Peer, HandlerModule, Args}) ->
    link(TransportPid),
    Context = #context{transport = TransportPid, peer = Peer},
    State = start_idle_timeout(#state{mod = HandlerModule, context = Context, async_reply_map = gb_trees:empty()}),
    case HandlerModule:init(Context, Args) of
        {ok, HandlerState} ->
            {ok, State#state{mod_state = HandlerState}}
    end.

handle_cast({set_idle_timeout, Timeout}, State) ->
    {noreply, reset_idle_timeout(State#state{idle_timeout = Timeout})};
handle_cast({incoming_message, Message}, State0) ->
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
            proto_send(Response, State),
            {noreply, State}
    end;
handle_cast({async_reply, ReqRef, ReqId, Result}, State = #state{async_reply_map = ARMap}) ->
    case gb_trees:lookup(ReqRef, ARMap) of
        {value, {incomplete_request, Req}} ->
            proto_send(to_response_record(Result, Req), State),
            {noreply, State#state{async_reply_map = gb_trees:delete(ReqRef, ARMap)}};
        {value, {incomplete_batch, BatchReq = #batch_request{requests = Reqs}, Responses}} ->
            case lists:partition(fun (#request{reqid = Id}) -> Id == ReqId end, Reqs) of
                {[LastReq], []} ->
                    BatchResp = hello_proto:batch_response(BatchReq, [to_response_record(Result, LastReq) | Responses]),
                    proto_send(BatchResp, State),
                    {noreply, State#state{async_reply_map = gb_trees:delete(ReqRef, ARMap)}};
                {[Req], StillWaiting} ->
                    NewBatchReq = BatchReq#batch_request{requests = StillWaiting},
                    NewIncompleteBatch = {incomplete_batch, NewBatchReq, [to_response_record(Result, Req) | Responses]},
                    NewARMap = gb_trees:enter(ReqRef, NewIncompleteBatch, ARMap),
                    {noreply, State#state{async_reply_map = NewARMap}}
            end;
        error ->
            error_logger:warning_report([{hello_stateful_handler, State#state.mod},
                                         {unknown_reply, ReqRef, ReqId, Result}]),
            {noreply, State}
    end.

handle_call(_Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_info({?IDLE_TIMEOUT_MSG, Ref}, State = #state{idle_timeout_ref = Ref}) ->
    {stop, normal, State#state{stopped_because_idle = true}};
handle_info({?IDLE_TIMEOUT_MSG, _OtherRef}, State) ->
    %% ignore timeouts other than the current one
    {noreply, State};

handle_info(InfoMsg, State = #state{mod = Mod, mod_state = ModState, context = Context}) ->
    case Mod:handle_info(Context, InfoMsg, ModState) of
        {noreply, NewModState} ->
            {noreply, State#state{mod_state = NewModState}};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#state{mod_state = NewModState}}
    end.

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
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

%% --------------------------------------------------------------------------------
%% -- internal functions
do_single_request(Req, State = #state{mod_state = ModState, mod = Mod}) ->
    case hello_validate:find_method(Mod:method_info(ModState), Req#request.method) of
        undefined ->
            proto_send(hello_proto:error_response(Req, method_not_found), State),
            {noreply, State};
        ValidatedMethod = #rpc_method{name = MethodName} ->
            case hello_validate:request_params(ValidatedMethod, Mod:param_info(MethodName, ModState), Req) of
                {error, Msg} ->
                    proto_send(hello_proto:error_response(Req, invalid_params, Msg), State),
                    {noreply, State};
                {ok, ValidatedParams} ->
                    ReqRef  = make_ref(),
                    Context = make_from_context(ReqRef, Req, State),
                    case Mod:handle_request(Context, MethodName, ValidatedParams, ModState) of
                        %% with reply
                        {reply, Reply, NewModState} ->
                            proto_send(to_response_record(Reply, Req), State),
                            {noreply, State#state{mod_state = NewModState}};
                        {stop, Reason, Reply, NewModState} ->
                            proto_send(to_response_record(Reply, Req), State),
                            {stop, Reason, State#state{mod_state = NewModState}};
                        %% without reply
                        {noreply, NewModState} ->
                            case Req#request.reqid of
                                undefined ->
                                    % no need to wait for an async reply if request is a notification
                                    {noreply, State#state{mod_state = NewModState}};
                                _ ->
                                    NewARMap = gb_trees:enter(ReqRef, {incomplete_request, Req}, State#state.async_reply_map),
                                    {noreply, State#state{mod_state = NewModState, async_reply_map = NewARMap}}
                            end;
                        {stop, Reason, NewModState} ->
                            {stop, Reason, State#state{mod_state = NewModState}}
                    end
            end
    end.

do_batch(BatchReq = #batch_request{requests = Requests}, State = #state{async_reply_map = ReplyMap}) ->
    BatchRef = make_ref(),
    case do_batch_requests(Requests, BatchRef, State, [], []) of
        {stop, Reason, Responses, NewState} ->
            proto_send(hello_proto:batch_response(BatchReq, Responses), NewState),
            {stop, Reason, NewState};
        {reply, Responses, [], NewState} ->
            proto_send(hello_proto:batch_response(BatchReq, Responses), NewState),
            {noreply, NewState};
        {reply, Responses, AsyncReplies, NewState} ->
            StoredReq = BatchReq#batch_request{requests = AsyncReplies},
            NewARMap = gb_trees:enter(BatchRef, {incomplete_batch, StoredReq, Responses}, ReplyMap),
            {noreply, NewState#state{async_reply_map = NewARMap}}
    end.

do_batch_requests([], _BR, State, Responses, NeedAsyncReply) ->
    {reply, Responses, NeedAsyncReply, State};
do_batch_requests([Req = #request{} | Rest], BatchRef, State = #state{mod = Mod}, Responses, AsyncReplies) ->
    ModState = State#state.mod_state,
    case hello_validate:find_method(Mod:method_info(ModState), Req#request.method) of
        undefined ->
            ErrorResp = hello_proto:error_response(Req, method_not_found),
            do_batch_requests(Rest, BatchRef, State, [ErrorResp | Responses], AsyncReplies);
        ValidatedMethod = #rpc_method{name = MethodName} ->
            case hello_validate:request_params(ValidatedMethod, Mod:param_info(MethodName, ModState), Req) of
                {error, Msg} ->
                    ErrorResp = hello_proto:error_response(Req, invalid_params, Msg),
                    do_batch_requests(Rest, BatchRef, State, [ErrorResp | Responses], AsyncReplies);
                {ok, ValidatedParams} ->
                    Context = make_from_context(BatchRef, Req, State),
                    case Mod:handle_request(Context, MethodName, ValidatedParams, ModState) of
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
                            case Req#request.reqid of
                                undefined ->
                                    do_batch_requests(Rest, BatchRef, State#state{mod_state = NewModState},
                                                      Responses, AsyncReplies);
                                _Id ->
                                    NewAsyncReplies = [Req | AsyncReplies],
                                    do_batch_requests(Rest, BatchRef, State#state{mod_state = NewModState},
                                                      Responses, NewAsyncReplies)
                            end;
                        {stop, Reason, NewModState} ->
                            case Req#request.reqid of
                                undefined ->
                                    stop_in_batch(Reason, State#state{mod_state = NewModState},
                                                  Responses, AsyncReplies);
                                _Id ->
                                    NewAsyncReplies = [Req | AsyncReplies],
                                    stop_in_batch(Reason, State#state{mod_state = NewModState},
                                                  Responses, NewAsyncReplies)
                            end
                    end
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
    hello_proto:error_response(Request, Error).

proto_send(ignore, _State) ->
    ok;
proto_send(Result, #state{context = Context}) ->
    send_binary(Context, hello_proto:encode(Result)).

send_notification(#from_context{context = Context}, Method, Params) ->
    send_notification(Context, Method, Params);
send_notification(Context = #context{}, Method, Params) ->
    send_binary(Context, hello_json:encode({[{'jsonrpc', <<"2.0">>}, {'method', Method}, {'params', Params}]})).

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
