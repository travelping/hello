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
-export([start_link/4, do_binary_request/2, behaviour_info/1,
         set_idle_timeout/1, set_idle_timeout/2, notify_np/3, reply/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("internal.hrl").
-include("hello.hrl").

-record(from_context, {from, request, context}).
-record(context, {transport_pid, peer}).

% -type reply() :: {ok, hello_json:value()} | {error, hello_proto:error_reply()}.

-spec behaviour_info(callbacks) -> [{atom(), integer()}].
behaviour_info(callbacks) ->
    [{method_info,1}, {param_info,2}, {init,2}, {handle_request,4}, {handle_info,3}, {terminate,3}];
behaviour_info(_Other) ->
    undefined.

start_link(TransportPid, Peer, HandlerMod, HandlerArgs) ->
    gen_server:start_link(?MODULE, {TransportPid, Peer, HandlerMod, HandlerArgs}, []).

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
-record(state, {context, mod, mod_state, idle_timer, idle_timeout_ref, idle_timeout = infinity}).

init({TransportPid, Peer, HandlerModule, Args}) ->
    Context = #context{transport_pid = TransportPid, peer = Peer},
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
    {stop, idle_timeout, State};

handle_cast({idle_timeout, _OtherRef}, State) ->
    %% ignore timeouts other than the current one
    {noreply, State};

handle_cast({do_binary_request, JSONRequest}, State = #state{context = Context}) ->
    Server = self(),
    spawn(fun () ->
                  case hello_proto:request_json(JSONRequest) of
                      {ok, Request} ->
                          Result = gen_server:call(Server, Request, infinity);
                      {batch, GoodReqs, InvalidReplies} ->
                          Result = InvalidReplies ++ [gen_server:call(Server, Request, infinity) || Request <- GoodReqs];
                      {error, ErrorReply} ->
                          Result = ErrorReply
                  end,
                  case hello_proto:response_json(Result) of
                      <<>> -> ignore;
                      Resp -> send_binary(Context, Resp)
                  end
          end),
    {noreply, State}.

handle_call(Req = #request{method = MethodName}, From, State = #state{mod_state = ModState, mod = Mod}) ->
    cancel_idle_timeout(State),
    case hello_validate:find_method(Mod:method_info(ModState), MethodName) of
        undefined ->
            {reply, hello_proto:std_error(Req, method_not_found), start_idle_timeout(State)};
        ValidatedMethod ->
            case hello_validate:request_params(ValidatedMethod, Mod:param_info(ValidatedMethod, ModState), Req) of
                {ok, ValidatedParams} ->
                    Context = make_from_context(From, State, Req),
                    Result  = Mod:handle_request(Context, ValidatedMethod#rpc_method.name, ValidatedParams, ModState),
                    handle_reply_result(Result, Req, State);
                {error, Msg}    ->
                    {reply, hello_proto:std_error(Req, {invalid_params, Msg}), start_idle_timeout(State)}
            end
    end;

handle_call(_Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_info(InfoMsg, State = #state{mod = Mod, mod_state = ModState, context = Context}) ->
    Result = Mod:handle_info(Context, InfoMsg, ModState),
    handle_result(Result, State).

terminate(Reason, #state{mod = Mod, mod_state = ModState, context = Context}) ->
    Mod:terminate(Context, Reason, ModState),
    send_closed(Context).

%% TODO: code_change
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

%% --------------------------------------------------------------------------------
%% -- internal functions
handle_reply_result({reply, Reply, NewModState}, Request, State) ->
    handle_reply_result({reply, Reply, NewModState, infinity}, Request, State);
handle_reply_result({reply, Reply, NewModState, Timeout}, Request, State) ->
    Response = to_response_record(Reply, Request),
    {reply, Response, start_idle_timeout(State#state{mod_state = NewModState}), Timeout};
handle_reply_result({stop, Reason, Reply, NewModState}, Request, State) ->
    Response = to_response_record(Reply, Request),
    {stop, Reason, Response, State#state{mod_state = NewModState}};
handle_reply_result(OtherResult, _Request, State) ->
    handle_result(OtherResult, State).

handle_result({noreply, NewModState}, State) ->
    handle_result({noreply, NewModState, infinity}, State);
handle_result({noreply, NewModState, Timeout}, State) ->
    {noreply, State#state{mod_state = NewModState}, Timeout};
handle_result({stop, Reason, NewModState}, State) ->
    {stop, Reason, State#state{mod_state = NewModState}};
handle_result(Result, _State) ->
    error({bad_return, Result}).

to_response_record({ok, Reply}, Request) ->
    hello_proto:response(Request, Reply);
to_response_record({error, Error}, Request) ->
    hello_proto:error_response(Request, Error).

send_notification(#from_context{context = Context}, Method, Params) ->
    send_notification(Context, Method, Params);
send_notification(Context = #context{}, Method, Params) ->
    send_binary(Context, hello_json:encode({[{'jsonrpc', <<"2.0">>}, {'method', Method}, {'params', Params}]})).

send_binary(#context{transport_pid = TMod, peer = Peer}, Message) when is_binary(Message) ->
    TMod ! {hello_msg, self(), Peer, Message}.

send_closed(#context{transport_pid = TMod, peer = Peer}) ->
    TMod ! {hello_closed, self(), Peer}.

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
