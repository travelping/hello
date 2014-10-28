-define (JSONRPC_1, "jsonrpc 1.0").
-define (JSONRPC_2, "jsonrpc 2.0").

-record(jsonrpc_client_state, {
    next_reqid = 0 :: non_neg_integer(),
    notification_sink :: function() | pid() | atom(),
    jsonrpc :: term()
}).
-record(jsonrpc_request, {
    reqid      :: hello_json:value(),
    method     :: binary(),
    params     :: hello_json:json_object() | list(hello_json:value()),
    jsonrpc    :: term()
}).
-record(jsonrpc_error, {
    code       :: integer() | atom(),
    message    :: iodata(),
    data       :: hello_json:value()
}).
-record(jsonrpc_response, {
    reqid      :: hello_json:value(),
    result     :: hello_json:value(),
    error      :: #jsonrpc_error{},
    jsonrpc    :: string()
}).
-record(async_batch, {
    finished    :: [#jsonrpc_response{}],
    waiting     :: [#jsonrpc_request{}]
}).
