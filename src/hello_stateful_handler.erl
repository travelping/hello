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
-export([start/4, do_binary_request/2, behaviour_info/1,
         set_idle_timeout/1, set_idle_timeout/2, notify_np/3]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("internal.hrl").

-record(context, {transport_mod, transport_state, request}).

% -type reply() :: {ok, hello_json:value()} | {error, hello_proto:error_reply()}.

-spec behaviour_info(callbacks) -> [{atom(), integer()}].
behaviour_info(callbacks) -> [{init,2}, {handle_request,4}, {handle_info,3}, {terminate,3}];
behaviour_info(_Other)    -> undefined.

start(TransportMod, TransportState, HandlerMod, HandlerArgs) ->
    gen_server:start(?MODULE, {TransportMod, TransportState, HandlerMod, HandlerArgs}, []).

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

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(state, {transport_mod, transport_state, mod, mod_state, idle_timer, idle_timeout = infinity}).

init({TransportMod, Transport, HandlerModule, Args}) ->
    State0 = #state{mod = HandlerModule, transport_mod = TransportMod, transport_state = Transport},
    State1 = refresh_idle_timeout(State0),
    case HandlerModule:init(make_context(State1), Args) of
        {ok, HandlerState} ->
            {ok, State1#state{mod_state = HandlerState}};
        {ok, HandlerState, Timeout} ->
            {ok, State1#state{mod_state = HandlerState}, Timeout};
        {error, ErrorReply} ->
            {stop, {error_reply, ErrorReply}}
    end.

handle_cast({set_idle_timeout, Timeout}, State) ->
    {noreply, refresh_idle_timeout(State#state{idle_timeout = Timeout})};

handle_cast(idle_timeout, State) ->
    {stop, idle_timeout, State};

handle_cast({do_binary_request, JSONRequest}, State = #state{mod = Mod, mod_state = ModState}) ->
    case hello_proto:request_json(JSONRequest) of
        {ok, Request} ->
            From = make_from_context(State, Request),
            Result = Mod:handle_request(From, Request#request.method, Request#request.params, ModState),
            NewState = refresh_idle_timeout(State),
            handle_result(Result, true, From, NewState);
        {error, ErrorResponse} ->
            send_binary(make_context(State), ErrorResponse),
            {stop, badrequest, State}
    end.

handle_info(InfoMsg, State = #state{mod = Mod, mod_state = ModState}) ->
    Result = Mod:handle_info(make_context(State), InfoMsg, ModState),
    handle_result(Result, false, undefined, State).

terminate(Reason, State = #state{mod = Mod, mod_state = ModState}) ->
    Mod:terminate(make_context(State), Reason, ModState).

%% unused callbacks
handle_call(_Call, _From, State) ->
    {reply, {error, unknown_call}, State}.
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

%% --------------------------------------------------------------------------------
%% -- internal functions

handle_result({reply, Reply, NewModState}, true, From, State) ->
    handle_result({reply, Reply, NewModState, infinity}, true, From, State);
handle_result({reply, Reply, NewModState, Timeout}, true, From, State) ->
    ok = send_reply(From, Reply),
    handle_result({noreply, NewModState, Timeout}, true, From, State);

handle_result({noreply, NewModState}, AllowReply, From, State) ->
    handle_result({noreply, NewModState, infinity}, AllowReply, From, State);
handle_result({noreply, NewModState, Timeout}, _AllowReply, _From, State) ->
    {noreply, State#state{mod_state = NewModState}, Timeout};

handle_result({stop, Reason, Reply, NewModState}, true, From, State) ->
    ok = send_reply(From, Reply),
    handle_result({stop, Reason, NewModState}, true, From, State);
handle_result({stop, Reason, NewModState}, _AllowReply, _From, State) ->
    {stop, Reason, State#state{mod_state = NewModState}};

handle_result(Result, _AllowReply, _From, _State) ->
    error({bad_return, Result}).

send_reply(Context = #context{request = Request}, {ok, Result}) ->
    send_binary(Context, hello_proto:response_json(hello_proto:response(Request, Result)));
send_reply(Context = #context{request = Request}, {error, ErrorReply}) ->
    send_binary(Context, hello_proto:response_json(hello_proto:error_response(Request, ErrorReply))).

send_notification(Context, Method, Params) ->
    send_binary(Context, hello_json:encode({[{'jsonrpc', <<"2.0">>}, {'method', Method}, {'params', Params}]})).

send_binary(#context{transport_mod = TMod, transport_state = TState}, Resp) when is_binary(Resp) ->
    TMod:send(TState, Resp).

make_context(#state{transport_mod = TMod, transport_state = TState}) ->
    #context{transport_mod = TMod, transport_state = TState}.
make_from_context(#state{transport_mod = TMod, transport_state = TState}, Request) ->
    #context{transport_mod = TMod, transport_state = TState, request = Request}.

refresh_idle_timeout(State) ->
    case State#state.idle_timer of
        undefined ->
            ok;
        OldTimer ->
            {ok, cancel} = timer:cancel(OldTimer)
    end,

    case State#state.idle_timeout of
        infinity ->
            State#state{idle_timer = undefined};
        IdleTimeout ->
            {ok, NewTimer} = timer:apply_after(IdleTimeout, gen_server, cast, [self(), idle_timeout]),
            State#state{idle_timer = NewTimer}
    end.
