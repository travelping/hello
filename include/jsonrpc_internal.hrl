-define (JSONRPC_1, 1).
-define (JSONRPC_2, 2).

-record(jsonrpc_info, {
    reqid,
    version
}).

-record(async_batch, {
    finished,   % :: [#jsonrpc_response{}],
    waiting    % :: [#jsonrpc_request{}]
}).
