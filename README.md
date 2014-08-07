Hello is a pragmatic RPC toolkit for Erlang.
It enables you to rapidly create rich RPC services.
Hello is also built for extensibility,
allowing new transport and RPC protocol implementations.

Out of the box, it supports the JSON-RPC 2.0 protocol
over HTTP and ZeroMQ transports.

# Building and installing

## Dependencies:

* [cowboy](https://github.com/extend/cowboy)
* [ex_uri](https://github.com/extend/ex_uri)
* [erlzmq2](https://github.com/zeromq/erlzmq2)
* [ibrowse](https://github.com/cmullaparthi/ibrowse)

## Building

We use [tetrapak](https://github.com/fjl/tetrapak) for
development. Hello follows standard OTP practice though, so you
can use any build tool that supports OTP applications.  

In order to build hello using tetrapak, simply execute

	tetrapak build

in your working copy.

Use

	tetrapak doc

to generate EDoc reference documentation.

# RPC Server Model

## Bindings

RPC endpoints are called bindings.
A binding represents a mapping from a unique transport
URL to a handler module.

## Listeners

A listener is a container for one or more bindings.
It usually wraps a network socket (e.g. a TCP acceptor).
Listeners are created on demand when the first binding that references
them is created. 

Quick example, creating a stateless HTTP binding like this:

	hello:bind_stateless("http://127.0.0.1:8080/example", my_handler).

will start an HTTP server automatically.

## Handlers

Hello handlers are Erlang modules that implement an RPC service.
There are two types of handlers, stateful and stateless.

Stateless handlers answer RPC requests independently of each
other. No state is kept in between requests. This behavior
is adequate for CRUD interfaces and other forms of classic RPC.

Stateful handlers keep session
state in between requests. The stateful handler behavior is very
similar to OTP's gen_server behavior. A distinct stateful handler
is spawned for every distinct peer. Peer distinction is handled by
the listener implementation and usually relies on transport-specific
mechanisms (like the ZeroMQ peer identity).
The stateful handler behavior is meant to be used when transport-level
sessions are required.

Hello's stateful handlers also support bi-directional
communication, that is, the server can initiate requests to a
connected client.

# Client

Hello also contains an RPC client.
Connecting to a server can be as simple as:

	{ok, Client} = hello_client:start_link("http://127.0.0.1:8080/example", []),
	hello_client:call(Client, "my_method", [1, 2, <<"argument">>]).

# Logging

Hello log request and responses through lager on level INFO. Lager metadata fields
'hello_request' and 'hello_handler' are set to support tracing.
'hello_request' is set to 'api' for normal request and to 'error' for undecoded (bad)
requests. 'hello_handler' is set to the name of the handler module.

To write all bad request to a file use:

  {lager, [
    {handlers, [
      {lager_file_backend, [{file, "bad_request.log"}, {level, none}]}
    ]},
    {traces, [
      {{lager_file_backend, "bad_request.log"}, [{hello_request, error}], info}]}
  ]}

To write all requests for a module hello_stateful_handler_example to a file use:

  {lager, [
    {handlers, [
      {lager_file_backend, [{file, "hello_stateful_handler_example.log"}, {level, none}]}
    ]},
    {traces, [
      {{lager_file_backend, "hello_stateful_handler_example.log"}, [{hello_handler, hello_stateful_handler_example}], info}]}
  ]}
