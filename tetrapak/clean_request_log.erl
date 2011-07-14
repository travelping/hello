-task({"clean:requestlog", "Delete the JSON-RPC request log"}).
-task({"start:sasl", "starts sasl"}).

run("clean:requestlog", _) ->
    tpk_file:delete(tetrapak:subdir("request-logs"));

run("start:sasl", _) ->
    application:start(sasl).
