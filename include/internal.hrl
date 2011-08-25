-record(request,  {id, method, params, version = 2}).
-record(response, {id, result, error,  version = 2}).

-record(binding, {url, host, port, ip, path, listener_mod, callback_mod, callback_args}).
