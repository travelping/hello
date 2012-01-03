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
-export([start_link/3, do_binary_request/2, behaviour_info/1,
         set_idle_timeout/1, set_idle_timeout/2, notify_np/3, reply/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("internal.hrl").
-include("hello.hrl").

-record(context, {
    transport :: pid(),
    peer      :: term()
}).

-record(from_context, {
    from      :: {reference(), pid()},
    request   :: #request{},
    context   :: #context{}
}).

-spec behaviour_info(callbacks) -> [{atom(), integer()}].
behaviour_info(callbacks) ->
    [{method_info,1}, {param_info,2}, {init,2}, {handle_request,4}, {handle_info,3}, {terminate,3}];
behaviour_info(_Other) ->
    undefined.

-spec start_link(#binding{}, term(), pid()) -> {ok, pid()}.
start_link(#binding{callback_mod = Mod, callback_args = Args}, Peer, Transport) ->
    gen_server:start_link(?MODULE, {Transport, Peer, Mod, Args}, []).

do_binary_request(HandlerPid, Request) ->
    gen_server:cast(HandlerPid, {do_binary_request, Request}).

set_idle_timeout(Timeout) ->
    set_idle_timeout(self(), Timeout).

set_idle_timeout(HandlerPid, Timeout) ->
    gen_server:cast(HandlerPid, {set_idle_timeout, Timeout}).

notify_np(Context, Method, Args) when is_list(Args) ->
    notify_np(Context, Method, {Args});
notify_np(Context, Method, Args = {_}) ->
    send_notification(Context, Method, Args).

reply(#from_context{from = From, request = Req}, Result) ->
    gen_server:reply(From, to_response_record(Result, Req)).

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(state, {
    context                 :: #context{},
    mod                     :: module(),
    mod_state               :: term(),
    idle_timer              :: term(), %% the current timer
    idle_timeout_ref        :: reference(),
    idle_timeout = infinity :: timeout()
}).

init({TransportPid, Peer, HandlerModule, Args}) ->
    link(TransportPid),
    Context = #context{transport = TransportPid, peer = Peer},
    State0 = #state{mod = HandlerModule, context = Context},
    State1 = start_idle_timeout(State0),
    case HandlerModule:init(Context, Args) of
        {ok, HandlerState} ->
            {ok, State1#state{mod_state = HandlerState}};
        {ok, HandlerState, Timeout} ->
            {ok, State1#state{mod_state = HandlerState}, Timeout};
        {error, ErrorReply} ->
            {stop, {error_reply, ErrorReply}}
    end.

handle_cast({set_idle_timeout, Timeout}, State) ->
    cancel_idle_timeout(State),
    {noreply, start_idle_timeout(State#state{idle_timeout = Timeout})};

handle_cast({idle_timeout, Ref}, State = #state{idle_timeout_ref = Ref}) ->
    {stop, normal, State};

handle_cast({idle_timeout, _OtherRef}, State) ->
    %% ignore timeouts other than the current one
    {noreply, State};

handle_cast({do_binary_request, JSONRequest}, State) ->
    cancel_idle_timeout(State),
    case hello_proto:request_json(JSONRequest) of
        {ok, Request} ->
            {Result, NewState} = handle_request(Request, State);
        {batch, GoodReqs, InvalidReplies} ->
            {GoodRes, NewState} = lists:foldl(fun (Request, {ResultAcc, StateAcc}) ->
                                                      {Res, NewStateAcc} = handle_request(Request, StateAcc),
                                                      {[Res | ResultAcc], NewStateAcc}
                                              end, {[], State}, GoodReqs),
            Result = InvalidReplies ++ GoodRes;
        {error, ErrorReply} ->
            Result = ErrorReply,
            NewState = State
    end,
    {noreply, start_idle_timeout(send_result(Result, NewState))}.

handle_call(_Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_info(InfoMsg, State = #state{mod = Mod, mod_state = ModState, context = Context}) ->
    Result = Mod:handle_info(Context, InfoMsg, ModState),
    {_, NewState} = handle_result(Result, State),
    {noreply, NewState}.

terminate(Reason, #state{mod = Mod, mod_state = ModState, context = Context}) ->
    Mod:terminate(Context, Reason, ModState),
    send_closed(Context).

%% TODO: code_change
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

%% --------------------------------------------------------------------------------
%% -- internal functions
handle_request(Req = #request{method = MethodName}, State = #state{mod_state = ModState, mod = Mod}) ->
    case hello_validate:find_method(Mod:method_info(ModState), MethodName) of
        undefined ->
            {hello_proto:std_error(Req, method_not_found), State};
        ValidatedMethod ->
            case hello_validate:request_params(ValidatedMethod, Mod:param_info(ValidatedMethod, ModState), Req) of
                {ok, ValidatedParams} ->
                    Context = State#state.context,
                    Result  = Mod:handle_request(Context, ValidatedMethod#rpc_method.name, ValidatedParams, ModState),
                    handle_reply_result(Result, Req, State);
                {error, Msg} ->
                    {hello_proto:std_error(Req, {invalid_params, Msg}), State}
            end
    end.

handle_reply_result({reply, Reply, NewModState}, Request, State) ->
    Response = to_response_record(Reply, Request),
    {Response, State#state{mod_state = NewModState}};
handle_reply_result({stop, Reason, Reply, NewModState}, Request, State) ->
    send_result(State#state.context, to_response_record(Reply, Request)),
    exit(Reason);
handle_reply_result(OtherResult, _Request, State) ->
    handle_result(OtherResult, State).

handle_result({noreply, NewModState}, State) ->
    {empty_response, State#state{mod_state = NewModState}};
handle_result({stop, Reason, NewModState}, State) ->
    exit(Reason);
handle_result(Result, _State) ->
    error({bad_return, Result}).

to_response_record({ok, Reply}, Request) ->
    hello_proto:response(Request, Reply);
to_response_record({error, Error}, Request) ->
    hello_proto:error_response(Request, Error).

send_result(Result, State = #state{context = Context}) ->
    case hello_proto:response_json(Result) of
        <<>> ->
            State;
        BinResp ->
            send_binary(Context, BinResp),
            State
    end.

send_notification(#from_context{context = Context}, Method, Params) ->
    send_notification(Context, Method, Params);
send_notification(Context = #context{}, Method, Params) ->
    send_binary(Context, hello_json:encode({[{'jsonrpc', <<"2.0">>}, {'method', Method}, {'params', Params}]})).

send_binary(#context{transport = TPid, peer = Peer}, Message) when is_binary(Message) ->
    TPid ! {hello_msg, self(), Peer, Message}.

send_closed(#context{transport = TPid, peer = Peer}) ->
    TPid ! {hello_closed, self(), Peer}.

make_from_context(From, State, Request) ->
    #from_context{from = From, request = Request, context = State#state.context}.

cancel_idle_timeout(State) ->
    case State#state.idle_timer of
        undefined ->
            ok;
        OldTimer ->
            {ok, cancel} = timer:cancel(OldTimer)
    end.

start_idle_timeout(State) ->
    case State#state.idle_timeout of
        infinity ->
            State#state{idle_timer = undefined, idle_timeout_ref = undefined};
        IdleTimeout ->
            IdleTimeoutRef = make_ref(),
            {ok, NewTimer} = timer:apply_after(IdleTimeout, gen_server, cast, [self(), {idle_timeout, IdleTimeoutRef}]),
            State#state{idle_timer = NewTimer, idle_timeout_ref = IdleTimeoutRef}
    end.
