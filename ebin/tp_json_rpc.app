% vim: filetype=erlang
{application, tp_json_rpc,
 [{description, "JSON-RPC server"},
  {vsn, "0.0.1"},
  {modules, [tpjrpc_app,
             tpjrpc_sup,

             tpjrpc_json,
             tpjrpc_proto,
             tp_json_rpc_service,

             tpjrpc_httpd,
             tpjrpc_inets,

             tpjrpc_example_service]},
  {applications, [kernel, stdlib, inets]},
  {registered, [tp_json_rpc_sup]},
  {mod, {tpjrpc_app, []}}
]}.
