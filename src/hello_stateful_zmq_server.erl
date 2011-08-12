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
-module(hello_stateful_zmq_server).
-behaviour(gen_server).
-export([start_link/3, start_supervised/3, send/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("ex_uri/include/ex_uri.hrl").

%% --------------------------------------------------------------------------------
%% -- API
start_supervised(URI, Module, Args) ->
    case hello_listener_supervisor:start_listener(worker, transient, ?MODULE, [URI, Module,Args]) of
        {ok, _Pid}               -> ok;
        {error, {Error, _Child}} -> {error, Error}
    end.

start_link(URI, Module, Args) ->
    gen_server:start_link(?MODULE, {URI, Module, Args}, []).

send({zmq, Socket, PeerIdentity}, Message) when is_binary(Message) ->
    ok = erlzmq:send(Socket, PeerIdentity, [sndmore]),
    ok = erlzmq:send(Socket, <<>>, [sndmore]),
    ok = erlzmq:send(Socket, Message).

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(state, {socket, lastmsg_peer, uri, context, mod, mod_args}).

init({URI = #ex_uri{}, CallbackModule, Args}) ->
    {ListenURI, ListenerKey, BindingKey} = reg_details(URI),

    case hello_registry:lookup_listener(ListenerKey) of
        {ok, _Pid, CallbackModule} -> {stop, already_started};
        {ok, _Pid, _OtherModule}   -> {stop, occupied};
        {error, not_found}         -> start_reg(ListenURI, ListenerKey, BindingKey, CallbackModule, Args)
    end.

start_reg(URI, ListenerKey, BindingKey, CallbackModule, CallbackModuleArgs) ->
    case hello_registry:multi_register([{ListenerKey, CallbackModule}, {BindingKey, CallbackModule}], self()) of
        ok ->
            Endpoint = ex_uri:encode(URI),
            {ok, Context} = erlzmq:context(),
            {ok, Socket} = erlzmq:socket(Context, [router, {active, true}]),
            case erlzmq:bind(Socket, Endpoint) of
                ok ->
                    {ok, _Log} = hello_request_log:open(CallbackModule, self()),
                    State      = #state{socket = Socket,
                                        uri = list_to_binary(ex_uri:encode(uri_for_log(URI))),
                                        context = Context,
                                        mod = CallbackModule,
                                        mod_args = CallbackModuleArgs},
                    {ok, State};
                {error, Error} ->
                    {stop, {transport, Error}}
            end;
        {already_registered, _Pid, CallbackModule} ->
            {stop, already_started};
        {already_registered, _Pid, _OtherModule} ->
            {stop, occupied}
    end.

handle_info({zmq, Socket, Message, [rcvmore]}, State = #state{socket = Socket, lastmsg_peer = undefined}) ->
    %% first message part is peer identity
    {noreply, State#state{lastmsg_peer = Message}};
handle_info({zmq, Socket, <<>>, [rcvmore]}, State = #state{socket = Socket, lastmsg_peer = Peer}) when is_binary(Peer) ->
    %% empty message part separates envelope from data
    {noreply, State};
handle_info({zmq, Socket, Message, []}, State = #state{socket = Socket, lastmsg_peer = Peer}) ->
    %% second message part is the actual request
    case lookup_handler(Peer) of
        {error, not_found} ->
            {ok, HandlerPid} = start_handler(Peer, State),
            hello_stateful_handler:do_binary_request(HandlerPid, Message);
        {ok, HandlerPid, _Data} ->
            hello_stateful_handler:do_binary_request(HandlerPid, Message)
    end,
    {noreply, State#state{lastmsg_peer = undefined}}.

terminate(_Reason, State) ->
    hello_request_log:close(State#state.mod),
    erlzmq:close(State#state.socket),
    erlzmq:term(State#state.context).

%% unused callbacks
handle_call(_Call, _From, State) ->
    {reply, {error, unknown_call}, State}.
handle_cast(_Cast, State) ->
    {noreply, State}.
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

%% --------------------------------------------------------------------------------
%% -- helpers
lookup_handler(PeerIdentity) ->
    hello_registry:lookup({stateful_zmq_handler, PeerIdentity}).

start_handler(PeerIdentity, #state{mod = HandlerModule, mod_args = Args, socket = Socket}) ->
    {ok, HandlerPid} = hello_stateful_handler:start(?MODULE, {zmq, Socket, PeerIdentity}, HandlerModule, Args),
    ok = hello_registry:register({stateful_zmq_handler, PeerIdentity}, undefined, HandlerPid),
    {ok, HandlerPid}.

reg_details(URI = #ex_uri{scheme = "tcp", authority = #ex_uri_authority{host = Host, port = Port}}) ->
    IP = ensure_ip(Host),
    StringIP = inet_parse:ntoa(IP),
    ListenURI = URI#ex_uri{authority = #ex_uri_authority{host = StringIP, port = Port}},
    ListenerKey = hello_registry:listener_key(IP, Port),
    BindingKey  = hello_registry:binding_key('zmq-tcp', list_to_binary(StringIP), Port, []),
    {ListenURI, ListenerKey, BindingKey};

reg_details(URI = #ex_uri{scheme = "ipc"}) ->
    Path = filename:absname(ipc_path(URI)),
    ListenerKey = hello_registry:listener_key({ipc, Path}, undefined),
    BindingKey  = hello_registry:binding_key('zmq-ipc', <<>>, undefined, hello_stateless_httpd:unslash(Path)),

    {URI, ListenerKey, BindingKey}.

uri_for_log(URI = #ex_uri{scheme = "tcp"}) ->
    URI#ex_uri{scheme = "tcp"};
uri_for_log(URI = #ex_uri{scheme = "ipc"}) ->
    URI#ex_uri{scheme = "zmq-ipc", path = filename:absname(ipc_path(URI)), authority = #ex_uri_authority{host = ""}}.

ipc_path(#ex_uri{path = Path, authority = #ex_uri_authority{host = Host}}) ->
    case Host of
        undefined -> Path;
        _         -> filename:join(Host, Path)
    end.

ensure_ip("*")  ->
    {0,0,0,0};
ensure_ip(Host) ->
    case inet_parse:address(Host) of
        {ok, Addr}      -> Addr;
        {error, einval} -> error({transport, badaddress})
    end.
