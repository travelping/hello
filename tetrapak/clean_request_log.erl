-task({"clean:requestlog", "Delete the JSON-RPC request log"}).

run("clean:requestlog", _) ->
    tpk_file:delete(tetrapak:subdir("json_requests.log")).
