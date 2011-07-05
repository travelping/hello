-record(request,  {id, method, params, version = 2}).
-record(response, {id, result, error,  version = 2}).

-define(HANDLER_TAB, hello_handlers).
