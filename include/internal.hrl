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

-record(batch_request, {
    proto_mod  :: module(),
    errors     :: list(#error{}), %% hack?
    requests   :: list(#request{})
}).

-record(batch_response, {
    proto_mod  :: module(),
    responses  :: list(#request{})
}).

-record(binding, {
    pid           :: pid(),
    url           :: ex_uri:uri(),
    host          :: string(),
    port          :: pos_integer(),
    ip            :: inet:ip_address(),
    path          :: [binary()],
    listener_mod  :: module(),
    callback_mod  :: module(),
    callback_type :: stateful | stateless,
    callback_args :: term()
}).
