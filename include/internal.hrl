%% RPC records
-record(request,  {
    reqid      :: hello_json:value(),
    method     :: binary(),
    params     :: hello_json:json_object() | list(hello_json:value()),
    proto_mod  :: module(),
    proto_data :: term()
}).

-record(response, {
    reqid      :: hello_json:value(),
    result     :: hello_json:value(),
    proto_mod  :: module(),
    proto_data :: term()
}).

-record(error, {
    reqid      :: hello_json:value(),
    code       :: integer() | atom(),
    data       :: hello_json:value(),
    message    :: iodata(),
    proto_mod  :: module(),
    proto_data :: term()
}).

%% Other internal stuff
-record(binding, {url, host, port, ip, path, listener_mod, callback_mod, callback_args}).
