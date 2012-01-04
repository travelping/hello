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
-export([validate_options/1, init/2, send_request/3, handle_info/2, terminate/2]).

-include("internal.hrl").

-record(zmq_state, {
    context  :: erlzmq:context(),
    socket   :: erlzmq:socket(),
    pending  :: gb_tree()
}).

-record(zmq_options, {options :: list(erlzmq:option())}).

validate_options([_|R]) ->
    validate_options(R);
validate_options([]) ->
    {ok, #zmq_options{}}.

init(URLRec, Options) ->
    URL = ex_uri:encode(URLRec),
    {ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, [dealer, {active, true}]),
    ok = erlzmq:connect(Socket, URL),
    {ok, #zmq_state{pending = gb_trees:empty(), context = Context, socket = Socket}}.

send_request(Request, From, State = #zmq_state{socket = Socket, pending = Pending}) ->
    erlzmq:send(Socket, <<>>, [sndmore]),
    erlzmq:send(Socket, hello_proto:encode(Request)),
    case Request of
        #request{reqid = undefined} ->
            {reply, ok, State};
        #request{reqid = ReqId} ->
            {noreply, State#zmq_state{pending = gb_trees:enter(ReqId, From, Pending)}};
        #batch_request{requests = [FirstReq | _R]} ->
            {noreply, State#zmq_state{pending = gb_trees:enter(FirstReq#request.reqid, From, Pending)}}
    end.

handle_info({zmq, _Socket, _Msgpart, [rcvmore]}, State) ->
    {noreply, State};
handle_info({zmq, _Socket, Msg, []}, State) ->
    case hello_proto:decode(hello_proto_jsonrpc, Msg) of
        #response{reqid = ReqId, result = Result} ->
            {noreply, reply_pending_req([ReqId], {ok, Result}, State)};
        Resp = #error{reqid = ReqId} ->
            {noreply, reply_pending_req([ReqId], {error, hello_proto:error_resp_to_error_reply(Resp)}, State)};
        #batch_response{responses = Resps} ->
            {ReqIds, Results} = lists:unzip(lists:keysort(1, lists:map(
                fun (#response{reqid = Id, result = Result}) -> {Id, {ok, Result}};
                    (Error = #error{reqid = Id}) -> {Id, {error, hello_proto:error_resp_to_error_reply(Error)}}
                end, Resps))),
            {noreply, reply_pending_req(ReqIds, Results, State)};
        _ ->
            {noreply, State}
    end.

terminate(_Reason, #zmq_state{context = Context, socket = Socket}) ->
    erlzmq:close(Socket),
    erlzmq:term(Context).

reply_pending_req([], _Resp, State) ->
    State;
reply_pending_req([ReqId | Rest], Resp, State = #zmq_state{pending = Pending}) ->
    case gb_trees:lookup(ReqId, Pending) of
        none ->
            reply_pending_req(Rest, Resp, State);
        {value, From} ->
            gen_server:reply(From, Resp),
            State#zmq_state{pending = gb_trees:delete(ReqId, Pending)}
    end.
