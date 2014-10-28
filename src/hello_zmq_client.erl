% Copyright 2012, Travelping GmbH <info@travelping.com>

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
-module(hello_zmq_client).

-behaviour(hello_client).
-export([init_transport/2, send_request/2, terminate_transport/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_info/2, terminate/2, handle_cast/2, code_change/3]).

-include("internal.hrl").
-record(zmq_state, {
    client          :: pid(),
    encode_info     :: binary(),
    context         :: erlzmq:erlzmq_context(),
    socket          :: erlzmq:erlzmq_socket()
}).

%% hello_cllient callbacks
init_transport(URIRec, _Options) ->
    gen_server:start_link(?MODULE, {self(), URIRec}, []).

send_request(Message = {_Request, _EncodeInfo}, TransportPid) ->
    gen_server:call(TransportPid, Message),
    {ok, TransportPid};
send_request(_, TransportPid) ->
    {error, no_valid_request, TransportPid}.

terminate_transport(_Reason, TransportPid) ->
    gen_server:cast(TransportPid, stop).

%% gen_server callbacks
init({Client, URLRec}) ->
    URL = ex_uri:encode(URLRec),
    {ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, [dealer, {active, true}]),
    case erlzmq:connect(Socket, URL) of
        ok ->
            State = #zmq_state{client = Client, context = Context, socket = Socket},
            {ok, State};
        {error, Error} ->
            {stop, Error}
    end.

handle_call({Request, EncodeInfo}, _From, State = #zmq_state{socket = Socket}) ->
    erlzmq:send(Socket, <<>>, [sndmore]),
    erlzmq:send(Socket, EncodeInfo, [sndmore]),
    erlzmq:send(Socket, Request),
    {reply, ok, State}.

handle_info({zmq, _Socket, <<>>, [rcvmore]}, State) ->
    {noreply, State};
handle_info({zmq, _Socket, EncInfo, [rcvmore]}, State) ->
    {noreply, State#zmq_state{encode_info = EncInfo}};
handle_info({zmq, _Socket, BinResponse, []}, State = #zmq_state{client = Client, encode_info = EncInfo}) ->
    Client ! {?INCOMING_MSG, {ok, binary_to_atom(EncInfo, latin1), BinResponse, self()}},
    {noreply, State#zmq_state{encode_info = undefined}};
handle_info(_, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State = #zmq_state{socket = Socket, context = Context}) ->
    erlzmq:close(Socket),
    erlzmq:term(Context).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.