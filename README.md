# Hello [![Build Status](https://travis-ci.org/travelping/hello.svg)](https://travis-ci.org/travelping/hello) [![Coverage Status](https://coveralls.io/repos/travelping/hello/badge.svg?branch=master&service=github)](https://coveralls.io/github/travelping/hello?branch=master)

Hello is a pragmatic RPC toolkit for Erlang.
It enables you to rapidly create rich RPC services.
Hello is also built for extensibility,
allowing new transport and RPC protocol implementations.

Out of the box, it supports the JSON-RPC 2.0 protocol
over HTTP and ZeroMQ transports.

# Building and installing

Use [rebar](https://github.com/rebar/rebar) for building:
    
    rebar get-deps compile

You can use hello in your own project:

    {deps, [
        {hello, ".*", {git, "https://github.com/travelping/hello.git", "hello_v3"}}
    ]}.

# RPC Server Model

## Bindings

RPC endpoints are called bindings.
A binding represents a mapping from a unique transport
URL to a handler module.

Example:

    hello:bind("http://127.0.0.1:8080", handler_module),

## Listeners

A listener usually wraps a network socket (e.g. a TCP acceptor).
Listeners are created on demand when the first binding that references
them is created. It is also automatically removed if no bindings
exists that could use it.

Quick example, starting HTTP listener like this:

    hello:start_listener("http://127.0.0.1:8080").

You can customize listener:

    TransportOpts = [],
    Protocol = hello_proto_jsonrpc,
    ProtocolOpts = [],
    Router = hello_router,
    hello:start_listener("http://127.0.0.1:8080", TransportOpts, Protocol, ProtocolOpts, Router).

will start an HTTP server automatically.

## Routes 

You can implement your own router (see [hello_router](/src/hello_router.erl)). 
For this you have to write module with function:
    
    route(content(), request(), #ex_uri{}) -> 
        {error, Reason :: term()} | 
        {ok, ServiceName :: binary(), Id : term()}.

## Handlers

Hello handlers are Erlang modules that implement an RPC service. A
handler can be started for every peer resulting in a 1-to-1 mapping.
This mechanism relies on transport specific implementations like the
ZeroMQ peer identit.
If started without that option the handler is available for N clients.

There is hello_handler:

    - sync replies
    - async replies
    - is stateless (or statefull if you use register handler)
    - a state to hold a rpc session
    - receiving messages from other processes of the same node
    - full jsonrpc support (e.g. two sided communication)
    - covers the most 'usecases'

# Client

Hello also contains an RPC client which is also capable of handling different
rpc protocols. Out of the box it supports jsonrpc as well.
Connecting to a server can be as simple as:


    TransportOpts = [],
    ProtocolOpts = [],
    ClientOpts = [],
	{ok, Client} = hello_client:start_link("http://127.0.0.1:8080/example", TransportOpts, ProtocolOpts, ClientOpts),
	hello_client:call(Client, {<<"my_method">>, [1, 2, <<"argument">>], []}).

You can start client with keep alive support:

    ClientOpts = [{keep_alive_interval, 3000}],
	{ok, Client} = hello_client:start_link("http://127.0.0.1:8080/example", [], [], ClientOpts).

# Logging

Hello logs with lager on level 'debug' and 'info'. There is a strong tracing support, e.g.
traces like {class, hello}, {hello_request_method, some_method} or {hello_request_status, ok | error} 
are available (see hello_log.hrl for more).

To write all bad request to a file use:

    {lager, [
        {handlers, [
            {lager_file_backend, [{file, "bad_request.log"}, {level, info}]}
        ]},
        {traces, [
            {{lager_file_backend, "bad_request.log"}, [{hello_request_status, error}], info}
            ]}
    ]}

To write all requests for a module hello_handler_example to a file use:

    {lager, [
        {handlers, [
            {lager_file_backend, [{file, "hello_handler_example.log"}, {level, none}]}
        ]},
        {traces, [
            {{lager_file_backend, "hello_handler_example.log"}, [{hello_handler_callback, hello_handler_example}], info}
        ]}
    ]}

# Metrics

Hello collects the following metrics via exometer_core and reports it to the each registered reporters: 

| Exometer ID                                       | Type      | Data Types | Report Time |
|---------------------------------------------------|-----------|------------|-------------|
| [hello, server, Name, packets_in, size]           | histogram | max, mean  |   1000      |
| [hello, server, Name, packets_in, per_sec]        | spiral    | one        |   1000      |
| [hello, server, Name, packets_out, size]          | histogram | max, mean  |   1000      |
| [hello, server, Name, packets_out, per_sec]       | spiral    | one        |   1000      |
| [hello, server, Name, requests, ReqType, per_sec] | spiral    | one        |   1000      |
| [hello, server, Name, request, handle_time]       | histogram | max, mean  |   1000      |
| [hello, client, Name, requests, ReqType, per_sec] | spiral    | one        |   1000      |
| [hello, client, Name, request, handle_time]       | histogram | max, mean  |   1000      |
| [hello, client, Name, ping_pong_latency]          | histogram | max, mean  |   1000      |
| [hello, services]                                 | counter   | value      |   2000      |
| [hello, bindings]                                 | counter   | value      |   2000      |
| [hello, listeners]                                | counter   | value      |   2000      |
| [hello, clients]                                  | counter   | value      |   2000      |

Name - server or client name
ReqType = [ok, error, internal]

# Elixir

You can use hello with Elixir environment via `Hello` and `Hello.Client` modules which are delegating `:hello` and `:hello_client`

# For Developers

If you want to implement server on another framework you have to know two things about protocols:

1. JSON RPC has signature:
    * For HTTP it is `application/json` in Content-Type header.
    * For ZMTP it is  0xAA, 0xFF as second frame (first frame should be empty).
2. Hello has keep alive mechanism via ping-pong messaging:
    * Client send `$PING` message and server reply with `$PONG` message.
    * HTTP signature for those messages is `application/octet-stream` in Content-Type header.
    * For ZMTP is  0xAA, 0xAA as second frame (first frame should be empty).
