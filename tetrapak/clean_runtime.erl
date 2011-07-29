-task({"clean:runtimedata", "Delete stuff that's created at runtime."}).

run("clean:runtimedata", _) ->
    tpk_file:delete(tetrapak:subdir("request-logs")),
    tpk_file:delete(tetrapak:subdir("status.ipc")).
