-module(hello_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("hello_test.hrl").
-include("../include/internal.hrl").

% ---------------------------------------------------------------------
% -- test cases
bind_http(_Config) ->
    bind_url(?HTTP).

bind_zmq_tcp(_Config) ->
    bind_url(?ZMQ_TCP).

bind_zmq_ipc(_Config) ->
    bind_url(?ZMQ_IPC).

unbind_all(_Config) ->
    Bindings = hello:bindings(),
    [ hello:unbind(Url, CallbackMod) || {Url, CallbackMod, _, _} <- Bindings],
    [] = hello:bindings().

start_supervised(_Config) ->
    [ start_client(Transport) || Transport <- ?TRANSPORTS ],
    [ hello_client_sup:stop_client(Url) || {Url, _} <- ?TRANSPORTS ],
    [] = hello_client_sup:clients(),
    ok.

start_named_supervised(_Config) ->
    [ start_named_client(Transport) || Transport <- ?TRANSPORTS ],
    [ hello_client_sup:stop_client(Url) || {Url, _} <- ?TRANSPORTS ],
    [] = hello_client_sup:clients(),
    ok.

keep_alive(_Config) ->
    bind_url(?HTTP),
    {Url, TransportOpts} = ?HTTP,
    meck:new(hello_client, [passthrough]),
    true = meck:validate(hello_client),
    ok = meck:expect(hello_client, handle_internal, fun(Message, State) -> ct:log("got pong"), meck:passthrough([Message, State]) end),
    {ok, _Client} = hello_client:start_supervised(Url, TransportOpts, [{protocol, hello_proto_jsonrpc}], [{keep_alive_interval, 100}] ),
    timer:sleep(310), %% lets wait for some pongs, should be around 3-4; look them up in the ct log
    meck:unload(hello_client),
    ok.

% ---------------------------------------------------------------------
% -- common_test callbacks
all() ->
    [bind_http,
     bind_zmq_tcp,
     bind_zmq_ipc,
     unbind_all,
     start_supervised,
     start_named_supervised,
     keep_alive
     ].

init_per_suite(Config) ->
    hello:start(),
    [ code:ensure_loaded(Callback) || Callback <- ?CALLBACK_MODS ],
    Config.

end_per_suite(_Config) ->
    application:stop(hello).

% ---------------------------------------------------------------------
% -- helpers
bind_url(Transport) ->
    [ ok = bind_url1(Transport, Handler, Protocol) || Handler <- ?HANDLER, Protocol <- ?PROTOCOLS ].

bind_url1({Url, TransportOpts}, Handler, Protocol) ->
    [FirstCallback, SecondCallback] = proplists:get_value(Handler, ?CALLBACKS),
    HandlerOpts = proplists:get_value(Handler, ?HANDLER_ARGS),
    ProtocolOpts = proplists:get_value(Protocol, ?PROTOCOL_ARGS),

    %% bind the first callback module
    ok = hello:bind(Url, TransportOpts, FirstCallback, Handler, HandlerOpts, Protocol, ProtocolOpts),

    %% binding the same module returns already_started
    {error, callback_already_defined} = hello:bind(Url, TransportOpts, FirstCallback, Handler, HandlerOpts, Protocol, ProtocolOpts),

    %% binding a different (the second) callback should also be possible 
    ok = hello:bind(Url, TransportOpts, SecondCallback, Handler, HandlerOpts, Protocol, ProtocolOpts).

start_client(Transport) ->
    {Url, TransportOpts} = Transport, 
    ProtocolOpts = [{protocol, hello_proto_jsonrpc}],
    {ok, Pid} = hello_client:start_supervised(Url, TransportOpts, ProtocolOpts, []),
    Pid.

start_named_client(Transport) ->
    Name = proplists:get_value(Transport, ?CLIENT_NAMES),
    {Url, TransportOpts} = Transport, 
    ProtocolOpts = [{protocol, hello_proto_jsonrpc}],
    {ok, _Pid} = hello_client:start_supervised(Name, Url, TransportOpts, ProtocolOpts, []),
    Name.
