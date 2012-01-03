-record(request,  {id, method, params, version = 2}).
-record(response, {id, result, error,  version = 2}).

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
