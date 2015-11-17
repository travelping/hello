defmodule Hello do
    defdelegate [
      start_listener(uri),
      start_listener(uri, trans_opts),
      start_listener(uri, trans_opts, protocol, proto_opts, router_mod),
      start_listener(name, uri, trans_opts, protocol, proto_opts, router_mod),

      stop_listener(uri),

      call_service(name, request),
      call_service(name, identifier, request),

      bind_handler(uri, callback_mod, callback_args),
      bind(uri, handler_mod),
      bind(uri, handler_mod, handler_args),
      unbind(uri, handler_mod)
    ], to: :hello
end

defmodule Hello.Client do
    defdelegate [
      start_link(uri, opts), 
      start_link(name, uri, opts), 
      start_link(uri, trans_opts, proto_opts, client_opts), 
      start_link(name, uri, trans_opts, proto_opts, client_opts),

      start(uri, trans_opts, proto_opts, client_opts), 
      start(name, uri, trans_opts, proto_opts, client_opts), 

      stop(client),

      start_supervised(uri, trans_opts, proto_opts, client_opts), 
      start_supervised(name, uri, trans_opts, proto_opts, client_opts), 

      stop_supervised(client),

      call(client, call), 
      call(client, call, timeout)
    ], to: :hello_client
end
