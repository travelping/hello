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

-behaviour(hello_listener).
-export([listener_specification/2, send_response/3, close/1, port/2, listener_termination/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("hello.hrl").
-include("hello_log.hrl").
-define(SHUTDOWN_TIMEOUT, 500).

%% --------------------------------------------------------------------------------
%% -- hello_binding
listener_specification(ExUriUrl, _TransportOpts) ->
    StartFun = {?MODULE, start_link, [ExUriUrl]},
    Specs = {{?MODULE, ExUriUrl}, StartFun, transient, ?SHUTDOWN_TIMEOUT, worker, [?MODULE]},
    {make_child, Specs}.

send_response(#context{transport_pid = TPid, transport_params = TParams, peer = Peer}, Signature, BinResp) ->
    TPid ! {hello_msg, TParams, Peer, Signature, BinResp}, ok.

close(_Context) ->
    ok.

listener_termination(_ExUri, _ListenerID) ->
    child.

port(_ExUriUrl, ListenerId) ->
    gen_server:call(ListenerId, get_port).

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(state, {
    url     :: #ex_uri{},
    lastmsg_peer :: binary(),
    encode_info :: binary(),
    socket  :: ezmq:socket()
}).

start_link(URL) ->
    gen_server:start_link(?MODULE, URL, []).

init(URL) ->
    process_flag(trap_exit, true),
    {ok, Socket}  = ezmq:socket([{type, router}, {active, true}]),
    case ezmq_bind_url(Socket, URL) of
        ok ->
            State = #state{socket = Socket, url = URL},
            {ok, State};
        {error, Error} ->
            ?LOG_INFO("Hello ZeroMQ listener was unable to bind on '~p' because of reason '~p'.", [URL, Error],
                        [{hello_transport, zmtp}, {hello_transport_url, ex_uri:encode(URL)}], ?LOGID47),
            {stop, Error}
    end.

handle_info({zmq, Socket, {Peer, [<<>>, Signature, Msg]}}, State) ->
    handle_info({zmq, Socket, {Peer, [Signature, Msg]}}, State);
handle_info({zmq, Socket, {Peer, [Signature, Msg]}}, State = #state{url = URL, socket = Socket}) ->
    Context = #context{ transport=?MODULE,
                        transport_pid = self(),
                        transport_params = undefined,
                        peer = Peer
                        },
    hello_listener:async_incoming_message(Context, URL, Signature, Msg),
    {noreply, State};

handle_info({zmq, _Socket, {Peer, Msg}}, State) ->
    ?LOG_INFO("Hello ZeroMQ listener received bad message '~p' from '~p'.", [Msg, Peer],
                gen_meta_fields(State), ?LOGID48),
    {noreply, State};

handle_info({hello_msg, _Handler, Peer, Signature, Message}, State = #state{socket = Socket}) ->
    ok = ezmq:send(Socket, {Peer, [<<>>, Signature, Message]}),
    {noreply, State};

handle_info({hello_closed, _HandlerPid, _Peer}, State) ->
    {noreply, State};
handle_info({'EXIT', _Reason}, State) ->
    {noreply, State};
handle_info({dnssd, _Ref, Msg}, State) ->
    ?LOG_DEBUG("Hello ZeroMQ listener ignored message '~p' from DNS discovery service.", [Msg],
                gen_meta_fields(State), ?LOGID49),
    {noreply, State}.

terminate(_Reason, State) ->
    ezmq:close(State#state.socket).

%% unused callbacks
handle_call(get_port, _From, State = #state{socket = Socket}) ->
    {ok, [{_, _, Port}|_]} = ezmq:sockname(Socket),
    {reply, Port, State};
handle_call(_Call, _From, State) ->
    {reply, not_supported, State}.
handle_cast(_Cast, State) ->
    {noreply, State}.
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

%% --------------------------------------------------------------------------------
%% -- helpers
default_port(undefined) -> 0;
default_port(Port) -> Port.

zmq_protocol(#ex_uri{scheme = "zmq-tcp"})  -> inet;
zmq_protocol(#ex_uri{scheme = "zmq-tcp6"}) -> inet6.

ezmq_bind_url(Socket, URI = #ex_uri{authority = #ex_uri_authority{host = Host, port = Port}}) ->
    Protocol = zmq_protocol(URI),
    case ezmq_ip(Protocol, Host) of
        {ok, IP} ->
            ezmq:bind(Socket, tcp, default_port(Port), [Protocol, {reuseaddr, true}, {ip, IP}]);
        Other ->
            Other
    end.

ezmq_ip(inet, "*") -> {ok, {0,0,0,0}};
ezmq_ip(inet, Host) -> inet:parse_ipv4_address(Host);

ezmq_ip(inet6, "*") -> {ok, {0,0,0,0,0,0,0,0}};
ezmq_ip(inet6, Host) ->
    case re:run(Host, "^\\[(.*)\\]$", [{capture, all, list}]) of
        {match, ["[::1]", IP]} ->
            inet:parse_ipv6_address(IP);
        _ ->
            inet:parse_ipv6_address(Host)
    end.

gen_meta_fields(#state{url = Url}) ->
    [{hello_transport, zmtp}, {hello_transport_url, ex_uri:encode(Url)}].