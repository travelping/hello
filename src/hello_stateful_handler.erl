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
-behaviour(gen_server).
-export([start/4, do_binary_request/2, behaviour_info/1,
         set_session_timeout/1, set_session_timeout/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("internal.hrl").

-record(context, {transport_mod, transport_state, request}).

% -type reply() :: {ok, hello_json:value()} | {error, hello_proto:error_reply()}.

-spec behaviour_info(callbacks) -> [{atom(), integer()}].
behaviour_info(callbacks) -> [{init,2}, {handle_request,4}, {handle_info,3}, {terminate,3}];
behaviour_info(_Other)    -> undefined.

start(PeerIdentity, HandlerModule, Socket, Args) ->
    gen_server:start(?MODULE, {PeerIdentity, HandlerModule, Socket, Args}, [{debug, [trace]}]).

do_binary_request(HandlerPid, Request) ->
    gen_server:cast(HandlerPid, {do_binary_request, Request}).

set_session_timeout(Timeout) ->
    set_session_timeout(self(), Timeout).

set_session_timeout(HandlerPid, Timeout) ->
    gen_server:cast(HandlerPid, {set_session_timeout, Timeout}).

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(state, {transport_mod, transport_state, mod, mod_state, session_timeout = infinity}).

init({TransportMod, Transport, HandlerModule, Args}) ->
    ServerState = #state{mod = HandlerModule,
                         transport_mod = TransportMod,
                         transport_state = Transport},
    case HandlerModule:init(make_context(ServerState), Args) of
        {ok, HandlerState} ->
            {ok, ServerState#state{mod_state = HandlerState}};
        {ok, HandlerState, Timeout} ->
            {ok, ServerState#state{mod_state = HandlerState}, Timeout};
        {error, ErrorReply} ->
            {stop, {error_reply, ErrorReply}}
    end.

handle_cast({set_session_timeout, Timeout}, State) ->
    {noreply, State#state{session_timeout = Timeout}, Timeout};

handle_cast({do_binary_request, JSONRequest}, State = #state{mod = Mod, mod_state = ModState}) ->
    case hello_proto:request_json(JSONRequest) of
        {ok, Request} ->
            From = make_from_context(State, Request),
            Result = Mod:handle_request(From, Request#request.method, Request#request.params, ModState),
            handle_result(Result, true, From, State);
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
    handle_result({reply, Reply, NewModState, State#state.session_timeout}, true, From, State);
handle_result({reply, Reply, NewModState, Timeout}, true, From, State) ->
    ok = send_reply(From, Reply),
    handle_result({noreply, NewModState, Timeout}, true, From, State);

handle_result({noreply, NewModState}, AllowReply, From, State) ->
    handle_result({noreply, NewModState, State#state.session_timeout}, AllowReply, From, State);
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

send_binary(#context{transport_mod = TMod, transport_state = TState}, Resp) when is_binary(Resp) ->
    TMod:send(TState, Resp).

make_context(#state{transport_mod = TMod, transport_state = TState}) ->
    #context{transport_mod = TMod, transport_state = TState}.
make_from_context(#state{transport_mod = TMod, transport_state = TState}, Request) ->
    #context{transport_mod = TMod, transport_state = TState, request = Request}.
