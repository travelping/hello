-module(hello_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("hello_test.hrl").
-include("../include/hello.hrl").
-include("../include/jsonrpc_internal.hrl").

% ---------------------------------------------------------------------
% -- test cases
bind_http(_Config) ->
    [bind_url(?HTTP, Protocol) || Protocol <- ?PROTOCOLS].

bind_zmq_tcp(_Config) ->
    [bind_url(?ZMQ_TCP, Protocol) || Protocol <- ?PROTOCOLS].

unbind_all(_Config) ->
    timer:sleep(300), %% needed for metrics
    Bindings = hello_binding:all(),
    true = (2 * length(?PROTOCOLS) * length(?CALLBACK_MODS)) == length(Bindings),
    [ begin 
          hello:unbind(Url, CallbackMod),
          hello:stop_listener(Url)
      end || {Url, _} <- ?TRANSPORTS, CallbackMod <- ?CALLBACK_MODS ],
    [] = hello_binding:all().

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
    [bind_url(?HTTP, Protocol) || Protocol <- ?PROTOCOLS],
    [bind_url(?ZMQ_TCP, Protocol) || Protocol <- ?PROTOCOLS],
    meck:new(hello_client, [passthrough]),
    true = meck:validate(hello_client),
    ok = meck:expect(hello_client, handle_internal, fun(Message, State) -> ct:log("got pong"), meck:passthrough([Message, State]) end),
    {ZMQUrl, ZMQTransportOpts} = ?ZMQ_TCP,
    {ok, ZMQClient} = hello_client:start_supervised(ZMQUrl, ZMQTransportOpts, 
                                                    [{protocol, hello_proto_jsonrpc}], [{keep_alive_interval, 200}] ),
    {HTTPUrl, HTTPTransportOpts} = ?HTTP,
    {ok, HTTPClient} = hello_client:start_supervised(HTTPUrl, HTTPTransportOpts, 
                                                     [{protocol, hello_proto_jsonrpc}], [{keep_alive_interval, 200}] ),
    timer:sleep(500), %% lets wait for some pongs, should be around 3-4; look them up in the ct log
    {_, [Arg], _} = ?REQ11,
    {ok, Arg} = hello_client:call(ZMQClient, ?REQ11),
    {ok, Arg} = hello_client:call(HTTPClient, ?REQ11),

    % server send no pong
    meck:new(hello_proto, [passthrough]),
    ok = meck:expect(hello_proto, handle_internal, fun(_, _) -> ct:log("no pong"), {ok, <<"NO_PONG">>} end),
    timer:sleep(500), 
    {ok, Arg} = hello_client:call(ZMQClient, ?REQ11),
    {ok, Arg} = hello_client:call(HTTPClient, ?REQ11),
    hello_client_sup:stop_client(ZMQUrl),
    hello_client_sup:stop_client(HTTPUrl),
    meck:unload(hello_proto),
    meck:unload(hello_client),
    ok.

request_metrics(_Config) ->
    {Url, _} = ?HTTP,
    hello:stop_listener(Url), % stop the listener on this url if present 
    {ok, _} = hello:start_listener(test_listener, Url, [], hello_proto_jsonrpc, [], hello_router),
    ok = hello:bind(Url, handler1, 0),
    ProtocolOpts = [{protocol, hello_proto_jsonrpc}],
    {ok, _Pid} = hello_client:start_supervised(test_client, Url, [], ProtocolOpts, []),
    {_, [Arg], _} = ?REQ11,

    % wait for metrics to come up
    timer:sleep(300),

    % metric ids for total request and package counting
    ListenerId = [hello,request,total,listener,total,test_listener,'127.0.0.1','6000',total,undefined,undefined,counter],
    HandlerId =  [hello,request,total,handler,'app/test',test_listener,'127.0.0.1','6000',total,undefined,undefined,counter],
    ClientId = [hello,request,total,client,undefined,test_client,undefined,undefined,undefined,'127.0.0.1','6000',counter],
    ListenerPacketInId = [hello,packet,in,listener,total,test_listener,'127.0.0.1','6000',total,undefined,undefined,counter],
    ListenerPacketOutId = [hello,packet,out,listener,total,test_listener,'127.0.0.1','6000',total,undefined,undefined,counter],
    IdList = [ListenerId, HandlerId, ClientId, ListenerPacketInId, ListenerPacketOutId],

    % first reset to prevent interference of previous tests
    [ exometer:reset(Id) || Id <- IdList ],

    % execute one request
    {ok, Arg} = hello_client:call(test_client, ?REQ11),

    % check if all have value 1
    [ {ok,[{value,1},_]} = exometer:get_value(Id) || Id <- IdList ],

    hello_client_sup:stop_client(test_client),
    hello:unbind(Url, handler1),
    hello:stop_listener(Url),
    ok.

time_metrics(_Config) ->
    {Url, _} = ?HTTP,

    ListenerLastResetId = [hello,time,last_reset,listener,total,test_listener,'127.0.0.1','6000',total,undefined,undefined,ticks],
    exometer:reset(ListenerLastResetId),

    {ok, _} = hello:start_listener(test_listener, Url, [], hello_proto_jsonrpc, [], hello_router),

    % first check last_reset
    {ok,[{value, LastReset},_]} = exometer:get_value(ListenerLastResetId),
    true = LastReset > 0,

    % uptime is special because it has the exometer 'function' type and it is only active when a subscription
    % is started on this metric. we just check here that the exometer callback function is working.
    timer:sleep(100),
    [{value, Uptime}] = hello_metrics:update_listener_uptime({test_listener,'127.0.0.1','6000'}),
    true = Uptime > 0,
    ok.

log_format(_Config) ->
    "internal" = hello_log:format_context(peer, #context{peer = make_ref()}),
    "127.0.0.1" = hello_log:format_context(peer, #context{peer = {{127, 0, 0, 1}, 8080}}),
    "12345" = hello_log:format_context(peer, #context{peer = <<"12345">>}),
    ok.

% ---------------------------------------------------------------------
% -- common_test callbacks
all() ->
    [bind_http,
     bind_zmq_tcp,
     unbind_all,
     start_supervised,
     start_named_supervised,
     keep_alive,
     request_metrics,
     time_metrics,
     log_format
    ].

init_per_suite(Config) ->
    hello:start(),
    [ code:ensure_loaded(Callback) || Callback <- ?CALLBACK_MODS ],
    Config.

end_per_suite(_Config) ->
    application:stop(hello).

% ---------------------------------------------------------------------
% -- helpers
bind_url({Url, _TransportOpts}, Protocol) ->
    spawn(fun() ->
        hello:start_listener(Url, Url, [], Protocol, [], hello_router),
        [ok = hello:bind(Url, Handler, proplists:get_value(Handler, ?HANDLER_ARGS)) || Handler <- ?CALLBACK_MODS],
        receive ok -> ok end
    end).

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
