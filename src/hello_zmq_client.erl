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
    case Request#request.reqid of
        undefined ->
            {reply, ok, State};
        ReqId ->
            {noreply, State#zmq_state{pending = gb_trees:enter(ReqId, From, Pending)}}
    end.

handle_info({zmq, _Socket, _Msgpart, [rcvmore]}, State) ->
    {noreply, State};
handle_info({zmq, _Socket, Msg, []}, State) ->
    case hello_proto:decode(hello_proto_jsonrpc, Msg) of
        #response{reqid = ReqId, result = Result} ->
            {noreply, reply_pending_req(ReqId, {ok, Result}, State)};
        Resp = #error{reqid = ReqId} ->
            {noreply, reply_pending_req(ReqId, {error, hello_proto:error_resp_to_error_reply(Resp)}, State)};
        _ ->
            State
    end.

terminate(_Reason, #zmq_state{context = Context, socket = Socket}) ->
    erlzmq:close(Socket),
    erlzmq:term(Context).

reply_pending_req(ReqId, Resp, State = #zmq_state{pending = Pending}) ->
    case gb_trees:lookup(ReqId, Pending) of
        none ->
            State;
        {value, From} ->
            gen_server:reply(From, Resp),
            State#zmq_state{pending = gb_trees:delete(ReqId, Pending)}
    end.
