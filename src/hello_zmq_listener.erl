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

% @private
-module(hello_zmq_listener).
-export([start_link/1]).

-behaviour(hello_binding).
-export([listener_specification/2, send_response/3, close/1, listener_termination/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("internal.hrl").
-define(SHUTDOWN_TIMEOUT, 500).

%% --------------------------------------------------------------------------------
%% -- hello_binding
listener_specification(ExUriUrl, _TransportOpts) ->
    StartFun = {?MODULE, start_link, [ExUriUrl]},
    Specs = {{?MODULE, ExUriUrl}, StartFun, transient, ?SHUTDOWN_TIMEOUT, worker, [?MODULE]},
    {make_child, Specs}.

send_response(#context{transport_pid = TPid, transport_params = TParams, peer = Peer}, EncInfo, BinResp) ->
    gen_server:call(TPid, {hello_msg, TParams, Peer, EncInfo, BinResp}).

close(_Context) ->
    ok.

listener_termination(_ListenerID) ->
    child.

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(state, {
    url     :: #ex_uri{},
    context :: erlzmq:erlzmq_context(),
    socket  :: erlzmq:erlzmq_socket(),
    lastmsg_peer :: binary(),
    encode_info :: binary()
}).

start_link(ExUriUrl) ->
    gen_server:start_link(?MODULE, ExUriUrl, []).

init(ExUriUrl) ->
    process_flag(trap_exit, true),
    Endpoint      = url_for_zmq(ExUriUrl),
    {ok, Context} = erlzmq:context(),
    {ok, Socket}  = erlzmq:socket(Context, [router, {active, true}]),
    case erlzmq:bind(Socket, Endpoint) of
        ok ->
            State = #state{socket = Socket, context = Context, url = ExUriUrl},
            {ok, State};
        {error, Error} ->
            {stop, Error}
    end.

handle_info({zmq, Socket, Message, [rcvmore]}, State = #state{socket = Socket, lastmsg_peer = undefined}) ->
    %% first message part is peer identity
    {noreply, State#state{lastmsg_peer = Message}};
handle_info({zmq, Socket, <<>>, [rcvmore]}, State = #state{socket = Socket, lastmsg_peer = Peer}) when is_binary(Peer) ->
    %% empty message part separates envelope from data
    {noreply, State};
handle_info({zmq, Socket, EncInfo, [rcvmore]}, State = #state{socket = Socket}) ->
    %% first actual data frame is an info about the encoding of the message
    {noreply, State#state{encode_info = EncInfo}};
handle_info({zmq, Socket, Message, []}, State = #state{socket = Socket, encode_info = EncInfo, lastmsg_peer = Peer, url = ExUriUrl}) ->
    %% second data part is the actual request
    Context = #context{ transport=?MODULE,
                        transport_pid = self(),
                        transport_params = undefined,
                        peer = Peer
                        },
    hello_binding:incoming_message(Context, ExUriUrl, binary_to_atom(EncInfo, latin1), Message),
    {noreply, State#state{encode_info = undefined, lastmsg_peer = undefined}};

handle_info(hello_closed, State) ->
    {noreply, State};
handle_info({'EXIT', _Reason}, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    erlzmq:close(State#state.socket),
    erlzmq:term(State#state.context).

handle_call({hello_msg, _TParams, Peer, EncInfo, Message}, _From, State = #state{socket = Socket}) ->
    ok = erlzmq:send(Socket, Peer, [sndmore]),
    ok = erlzmq:send(Socket, <<>>, [sndmore]),
    ok = erlzmq:send(Socket, EncInfo, [sndmore]),
    ok = erlzmq:send(Socket, Message),
    {reply, ok, State};
handle_call(_Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% unused callbacks
handle_cast(_Cast, State) ->
    {noreply, State}.
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

%% helpers
url_for_zmq(URI = #ex_uri{scheme = "zmq-tcp"}) -> ex_uri:encode(URI#ex_uri{scheme = "tcp"});
url_for_zmq(URI = #ex_uri{scheme = "zmq-ipc"}) -> ex_uri:encode(URI#ex_uri{scheme = "ipc"}).
