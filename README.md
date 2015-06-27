# Hello [![Build Status](https://travis-ci.org/travelping/hello.svg)](https://travis-ci.org/travelping/hello)

Hello is a pragmatic RPC toolkit for Erlang.
It enables you to rapidly create rich RPC services.
Hello is also built for extensibility,
allowing new transport and RPC protocol implementations.

Out of the box, it supports the JSON-RPC 2.0 protocol
over HTTP and ZeroMQ transports.

# Building and installing

## Dependencies:

You can build an install hello using tetrapak. It will
automatically download necessary dependencies if your
version is up to date.

If you don't want to use tetrapak here is the list of
dependencies. You can also lookup the commit id in the
list of (tested) dependencies for tetrapak in the file
<config.ini>.

* [jsx](https://some_link.com)
* [cowboy](https://github.com/extend/cowboy)
* [ex_uri](https://github.com/extend/ex_uri)
* [erlzmq2](https://github.com/zeromq/erlzmq2)
* [ibrowse](https://github.com/cmullaparthi/ibrowse)

## Building

We use [tetrapak](https://github.com/travelping/tetrapak) for
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

A listener usually wraps a network socket (e.g. a TCP acceptor).
Listeners are created on demand when the first binding that references
them is created. It is also automatically removed if no bindings
exists that could use it.

Quick example, creating a stateless HTTP binding like this:

	hello:bind_stateless("http://127.0.0.1:8080/example", my_handler).

will start an HTTP server automatically.

## Handlers

Hello handlers are Erlang modules that implement an RPC service. A
handler can be started for every peer resulting in a 1-to-1 mapping.
This mechanism relies on transport specific implementations like the
ZeroMQ peer identit.
If started without that option the handler is available for N clients.

There are two types of handlers:

- the hello_handler which has full capabilities including
    - async replies
    - a state to hold a rpc session
    - receiving messages from other processes of the same node
    - full jsonrpc support (e.g. two sided communication)

- the hello_simple_handler which has restricted capabilities:
    - can just handle pure request-response patterns
    - is stateless
    - no async replies are possible, an immediate response is
      expected
    - cannot be started in single-client mode
    - covers the most 'usecases'

# Client

Hello also contains an RPC client which is also capable of handling different
rpc protocols. Out of the box it supports jsonrpc as well.
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
