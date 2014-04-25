% Copyright (c) 2010-2011 by Travelping GmbH <info@travelping.com>

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
-module(hello_sockjs_listener).
-behaviour(hello_binding).
-export([listener_specification/2, listener_key/1, binding_key/1, url_for_log/1, listener_termination/1]).

-export([init/3, handle/2, terminate/3]).

-export([session_init/4]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("internal.hrl").
-include("sockjs_static.hrl").

-define(SOCKJS_HEARTBEAT_DELAY, 25000).

%% ----------------------------------------------------------------------------------------------------
%% -- hello_binding callbacks
listener_specification(ChildID, #binding{ip = IP, port = Port}) ->
    Dispatch = cowboy_router:compile([{'_', [{'_', ?MODULE, []}]}]),
    %% Copied from cowboy.erl because it doesn't provide an API that
    %% allows supervising the listener from the calling application yet.
    Acceptors = 100,
    TransportOpts = [{port, hello_http_listener:default_port(Port)}, {ip, IP}],
    ProtocolOpts = [{env, [{dispatch, Dispatch}]}],
    Result = cowboy:start_http({ChildID, ?MODULE}, Acceptors, TransportOpts, ProtocolOpts),
    {started, Result}.

listener_key(#binding{ip = IP, port = Port}) ->
    hello_registry:listener_key(IP, hello_http_listener:default_port(Port)).

binding_key(#binding{host = Host, port = Port, path = Path}) ->
    {list_to_binary(Host), hello_http_listener:default_port(Port), hello_http_listener:unslash(Path)}.

url_for_log(Binding) ->
    hello_http_listener:url_for_log(Binding).

listener_termination(ChildID) ->
    ranch:stop_listener({ChildID, ?MODULE}).

%% ----------------------------------------------------------------------------------------------------
%% -- request handling (callbacks for cowboy_http_handler)
init({tcp, http}, Req, _) ->
    {ok, Req, undefined}.

handle(Req, _State) ->
    {Method, Req1} = cowboy_req:method(Req),
    {Port, Req2} = cowboy_req:port(Req1),
    {PathList, Req3} = cowboy_req:path_info(Req2),
    {Host, Req4} = cowboy_req:host(Req3),
    case find_binding(Host, Port, lists:reverse(PathList), [], 4) of
        not_found ->
            {ok, ReplyReq} = cowboy_req:reply(404, hello_http_listener:server_header(), Req4),
            {ok, ReplyReq, undefined};
        {PathRest, Binding} ->
            dispatch(Binding, Req4, Method, PathRest)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

find_binding(_Host, _Port, _PathList, _Acc, 0) ->
    not_found;
find_binding(Host, Port, PathList, Acc, N) ->
    RevPath = lists:reverse(PathList),
    case hello_http_listener:lookup_binding(?MODULE, Host, Port, RevPath) of
        {ok, Binding} ->
            {Acc, Binding};
        {error, not_found} ->
            case PathList of
                [] ->
                    not_found;
                [PH | PRest] ->
                    find_binding(Host, Port, PRest, [PH | Acc], N - 1)
            end
    end.

dispatch(_Binding, Req, <<"GET">>, []) ->
    reply(200, [{<<"Content-Type">>, <<"text/plain; charset=UTF-8">>}], ?SOCKJS_WELCOME, Req);
dispatch(_Binding, Req, <<"GET">>, [<<"iframe">>, <<IFRest/binary>>]) ->
    %% TODO: caching headers
    %% TODO: serve correct script location
    case re:run(IFRest, ".*\\.html$", [{capture, none}]) of
        match ->
            reply(200, [{'Content-Type', <<"text/html; charset=UTF-8">>}], ?SOCKJS_IFRAME, Req);
        nomatch ->
            reply(404, [], <<>>, Req)
    end;
dispatch(Binding, Req, <<"POST">>, [_Server, SessionID, <<"xhr">>]) ->
    {CORS, Req2} = cors_headers_and_jsessionid(Req),
    case lookup_session(SessionID) of
        {ok, SessionPid, _Handler} ->
            case session_connect(SessionPid, self()) of
                ok ->
                    SessionMsg = session_get_final_message(SessionPid),
                    xhr_reply(SessionMsg, CORS, Req2);
                {error, already_connected} ->
                    xhr_reply(already_connected, CORS, Req2)
            end;
        {error, not_found} ->
            {TransportParams, Req3} = hello_http_listener:req_transport_params(Req2),
            {ok, _SessionPid} = start_session(Binding, SessionID, TransportParams),
            xhr_reply(started, CORS, Req3)
    end;
dispatch(_Binding, Req, <<"POST">>, [_Server, SessionID, <<"xhr_send">>]) ->
    case lookup_session(SessionID) of
        {ok, _Session, Handler} ->
            {ok, Body, Req2} = cowboy_req:body(Req),
            case hello_json:decode(Body) of
                {ok, Text, _} when is_list(Text) ->
                    lists:foreach(fun (T) -> hello_binding:incoming_message(Handler, T) end, Text),
                    {CORS, Req3} = cors_headers_and_jsessionid(Req2),
                    reply(204, [{<<"Content-Type">>, <<"text/plain">>} | CORS], <<>>, Req3);
                _Other ->
                    {CORS, Req3} = cors_headers_and_jsessionid(Req2),
                    reply(400, CORS, <<>>, Req3)
            end;
        {error, not_found} ->
            reply(404, [], <<>>, Req)
    end;
dispatch(_Binding, Req, <<"GET">>, _) ->
    reply(404, [], <<>>, Req);
dispatch(_Binding, Req, _, _) ->
    reply(404, [], <<>>, Req).

xhr_reply({session_messages, _, Messages}, Headers, Req) ->
    reply(200, [{<<"Content-Type">>, <<"application/javascript; charset=UTF-8">>} | Headers], ["a", hello_json:encode(Messages), "\n"], Req);
xhr_reply({session_heartbeat, _}, Headers, Req) ->
    reply(200, [{<<"Content-Type">>, <<"application/javascript; charset=UTF-8">>} | Headers], <<"h\n">>, Req);
xhr_reply({session_closed, _}, Headers, Req) ->
    reply(200, [{<<"Content-Type">>, <<"application/javascript; charset=UTF-8">>} | Headers], <<"c[9000,\"hello handler terminated\"]\n">>, Req);
xhr_reply(started, Headers, Req) ->
    reply(200, [{<<"Content-Type">>, <<"application/javascript; charset=UTF-8">>} | Headers], <<"o\n">>, Req);
xhr_reply(already_connected, Headers, Req) ->
    reply(200, [{<<"Content-Type">>, <<"application/javascript; charset=UTF-8">>} | Headers], <<"c[300,\"Go Away\"]\n">>, Req).

reply(Code, Headers, Content, Req) ->
    {ok, ReplyReq} = cowboy_req:reply(Code, hello_http_listener:server_header() ++ Headers, Content, Req),
    {ok, ReplyReq, undefined}.

cors_headers_and_jsessionid(Req) ->
    case cowboy_req:header(<<"Origin">>, Req) of
        {undefined, Req2} ->
            {[{<<"Set-Cookie">>, <<"JSESSIONID=dummy;path=/">>}], Req2};
        {Origin, Req2} ->
            {[{<<"Access-Control-Allow-Origin">>, Origin},
              {<<"Access-Control-Allow-Credentials">>, <<"true">>},
              {<<"Set-Cookie">>, <<"JSESSIONID=dummy;path=/">>}], Req2}
    end.

cache_headers(ETag) ->
    [{<<"Cache-Control">>, <<"public, max-age=31536000">>},
     {<<"ETag">>, ETag}].

%% ----------------------------------------------------------------------------------------------------
%% -- Session Process API
start_session(Binding, SessionID, TransportParams) ->
    SessionPid = spawn(?MODULE, session_init, [Binding, self(), SessionID, TransportParams]),
    receive
        {started, SessionPid} ->
            {ok, SessionPid};
        {already_registered, SessionPid} ->
            {error, already_registered}
    end.

lookup_session(SessionID) ->
    hello_registry:lookup({sockjs_session, SessionID}).

session_connect(SessionPid, Conn) ->
    SessionPid ! {connect, Conn},
    receive
        {session_connected, SessionPid, false} -> ok;
        {session_connected, SessionPid, true}  -> {error, already_connected}
    end.

session_get_next_message(SessionPid) ->
    receive
        Msg when element(2, Msg) == SessionPid ->
            SessionPid ! {ack, self()}, Msg
    end.

session_get_final_message(SessionPid) ->
    receive
        Msg when element(2, Msg) == SessionPid ->
            SessionPid ! {ack_disconnect, self()}, Msg
    end.

%% ----------------------------------------------------------------------------------------------------
%% -- Session Process Implementation
-record(session_state, {handler :: hello_binding:handler(), queue = queue:new()}).

session_init(Binding, Conn, SessionID, TransportParams) ->
    Handler = hello_binding:start_handler(Binding, SessionID, self(), TransportParams),
    case hello_registry:register({sockjs_session, SessionID}, Handler, self()) of
        ok ->
            Conn ! {started, self()},
            session_loop_queueing(#session_state{handler = Handler});
        {already_registered, RegPid, RegHandler} ->
            Conn ! {already_registered, RegPid, RegHandler},
            exit(normal)
    end.

session_loop_queueing(State = #session_state{queue = Queue}) ->
    receive
        {hello_msg, _Handler, _Peer, Message} ->
            session_loop_queueing(State#session_state{queue = queue:in(Message, Queue)});
        {hello_closed, _Handler, _Peer} ->
            session_loop_queueing(State#session_state{queue = queue:in(closed, Queue)});
        {connect, Conn} ->
            Conn ! {session_connected, self(), false},
            session_enter_connected(Conn, State)
    end.

session_enter_connected(Conn, State) ->
    ConnMref = monitor(process, Conn),
    Heartbeat = erlang:send_after(?SOCKJS_HEARTBEAT_DELAY, self(), heartbeat),
    NewState = #session_state{queue = queue:new()},
    session_send_ackd(Heartbeat, ConnMref, Conn, NewState, {session_messages, self(), queue:to_list(State#session_state.queue)}).

session_exit_connected(HeartbeatTimer, ConnMref, State, NextStateFun) ->
    erlang:cancel_timer(HeartbeatTimer),
    erlang:demonitor(ConnMref),
    NextStateFun(State).

session_loop_connected(HeartbeatTimer, ConnMref, Conn, State) ->
    receive
        {hello_msg, _Handler, _Peer, Message} ->
            session_send_ackd(HeartbeatTimer, ConnMref, Conn, State, {session_messages, self(), [Message]});
        {hello_closed, _Handler, _Peer} ->
            session_send_ackd(HeartbeatTimer, ConnMref, Conn, State, {session_closed, self()});
        {connect, Conn} ->
            Conn ! {session_connected, self(), true},
            session_loop_connected(HeartbeatTimer, ConnMref, Conn, State);
        heartbeat ->
            NewTimer = erlang:send_after(?SOCKJS_HEARTBEAT_DELAY, self(), heartbeat),
            session_send_ackd(NewTimer, ConnMref, Conn, State, {session_heartbeat, self()});
        {'DOWN', ConnMref, process, Conn, _} ->
            session_exit_connected(HeartbeatTimer, ConnMref, State, fun session_loop_queueing/1)
    end.

session_send_ackd(HeartbeatTimer, ConnMref, Conn, State, Msg) ->
    Conn ! Msg,
    receive
        {ack, Conn} ->
            session_loop_connected(HeartbeatTimer, ConnMref, Conn, State);
        {ack_disconnect, Conn} ->
            session_exit_connected(HeartbeatTimer, ConnMref, State, fun session_loop_queueing/1);
        {'DOWN', ConnMref, process, Conn, _} ->
            disconnected
    end.
