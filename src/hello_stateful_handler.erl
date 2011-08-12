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
-export([start/4, do_binary_request/2, behaviour_info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("internal.hrl").

-record(context, {transport_mod, transport_state, request}).

% -type reply() :: {ok, hello_json:value()} | {error, hello_proto:error_reply()}.

-spec behaviour_info(callbacks) -> [{atom(), integer()}].
behaviour_info(callbacks) -> [{init,2},
                              {handle_request,4},
                              {handle_info,3},
                              {terminate,3}];
behaviour_info(_Other)    -> undefined.

start(PeerIdentity, HandlerModule, Socket, Args) ->
    gen_server:start(?MODULE, {PeerIdentity, HandlerModule, Socket, Args}, []).

do_binary_request(HandlerPid, Request) ->
    gen_server:cast(HandlerPid, {do_binary_request, Request}).

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(state, {transport_mod, transport_state, mod, mod_state}).

init({TransportMod, Transport, HandlerModule, Args}) ->
    ServerState = #state{mod = HandlerModule,
                         transport_mod = TransportMod,
                         transport_state = Transport},
    case HandlerModule:init(make_context(ServerState), Args) of
        {ok, HandlerState} ->
            {ok, #state{mod_state = HandlerState}};
        {error, ErrorReply} ->
            {stop, {error_reply, ErrorReply}}
    end.

handle_cast({do_binary_request, JSONRequest}, State = #state{mod = Mod, mod_state = ModState}) ->
    case hello_proto:request_json(JSONRequest) of
        {ok, Request} ->
            From = make_from_context(State, Request),
            case Mod:handle_request(From, Request#request.method, Request#request.params, ModState) of
                {reply, Reply, NewModState} ->
                    ok = send_reply(From, Request, Reply),
                    {noreply, State#state{mod_state = NewModState}};
                {noreply, NewModState} ->
                    {noreply, State#state{mod_state = NewModState}};
                {stop, Reason, Reply, NewModState} ->
                    ok = send_reply(From, Request, Reply),
                    {stop, Reason, State#state{mod_state = NewModState}};
                {stop, Reason, NewModState} ->
                    {stop, Reason, State#state{mod_state = NewModState}}
            end;
        {error, ErrorResponse} ->
            send_binary(make_context(State), ErrorResponse),
            {noreply, State}
    end.

handle_info(InfoMsg, State = #state{mod = Mod, mod_state = ModState}) ->
    case Mod:handle_info(make_context(State), InfoMsg, ModState) of
        {noreply, NewModState} ->
            {noreply, State#state{mod_state = NewModState}};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#state{mod_state = NewModState}}
    end.

terminate(Reason, State = #state{mod = Mod, mod_state = ModState}) ->
    Mod:terminate(make_context(State), Reason, ModState).

%% unused callbacks
handle_call(_Call, _From, State) ->
    {reply, {error, unknown_call}, State}.
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

%% --------------------------------------------------------------------------------
%% -- internal functions
send_reply(Context, Request, {ok, Result}) ->
    send_binary(Context, hello_proto:response_json(hello_proto:response(Request, Result)));
send_reply(Context, Request, {error, ErrorReply}) ->
    send_binary(Context, hello_proto:response_json(hello_proto:error_response(Request, ErrorReply))).

send_binary(#context{transport_mod = TMod, transport_state = TState}, Resp) when is_binary(Resp) ->
    TMod:send(TState, Resp).

make_context(#state{transport_mod = TMod, transport_state = TState}) ->
    #context{transport_mod = TMod, transport_state = TState}.
make_from_context(#state{transport_mod = TMod, transport_state = TState}, Request) ->
    #context{transport_mod = TMod, transport_state = TState, request = Request}.
