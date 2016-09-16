defmodule Hello do
  defdelegate start_listener(uri), to: :hello
  defdelegate start_listener(uri, trans_opts), to: :hello
  defdelegate start_listener(uri, trans_opts, protocol, proto_opts, router_mod), to: :hello
  defdelegate start_listener(name, uri, trans_opts, protocol, proto_opts, router_mod), to: :hello

  defdelegate stop_listener(uri), to: :hello

  defdelegate call_service(name, request), to: :hello
  defdelegate call_service(name, identifier, request), to: :hello

  defdelegate bind_handler(uri, callback_mod, callback_args), to: :hello
  defdelegate bind(uri, handler_mod), to: :hello
  defdelegate bind(uri, handler_mod, handler_args), to: :hello
  defdelegate unbind(uri, handler_mod), to: :hello
end

defmodule Hello.Client do
  defdelegate start_link(uri, opts), to: :hello_client
  defdelegate start_link(name, uri, opts), to: :hello_client
  defdelegate start_link(uri, trans_opts, proto_opts, client_opts), to: :hello_client
  defdelegate start_link(name, uri, trans_opts, proto_opts, client_opts),to: :hello_client

  defdelegate start(uri, trans_opts, proto_opts, client_opts), to: :hello_client
  defdelegate start(name, uri, trans_opts, proto_opts, client_opts), to: :hello_client

  defdelegate stop(client),to: :hello_client

  defdelegate start_supervised(uri, trans_opts, proto_opts, client_opts), to: :hello_client
  defdelegate start_supervised(name, uri, trans_opts, proto_opts, client_opts), to: :hello_client

  defdelegate stop_supervised(client), to: :hello_client

  defdelegate call(client, call), to: :hello_client
  defdelegate call(client, call, timeout), to: :hello_client
end
