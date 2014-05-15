-task {"build:yang", "compile yang models into Erlang headers"}.

check("build:yang") ->
    case application:load(yang) of
	ok ->
	    SrcDir = tetrapak:path("src"),
	    tpk_util:check_files_mtime(SrcDir, ".yang", SrcDir, ".hrl");
	_ ->
	    {done, [{modules, []}]}
    end.

run("build:yang", Files) ->
    run_foreach(fun yang_to_hrl/1, Files).


%% ------------------------------------------------------------
%% -- Helpers
run_foreach(Function, List) ->
    Res = lists:foldl(fun (Item, DoFail) ->
                              case Function(Item) of
                                  ok    -> DoFail;
                                  error -> true
                              end
                      end, false, List),
    if Res  -> tetrapak:fail("compilation failed");
       true -> ok
    end.

yang_to_hrl({In, Out}) ->
    BaseDir = tetrapak:dir(),
    io:format("Compiling ~s~n", [tpk_file:relative_path(In, BaseDir)]),
    case yang:deep_parse_file(In) of
	{ok, Yang} ->
	    Ts = yang:typespec(Yang),
	    file:write_file(Out, yang_typespec:hrl(Ts));
	{error, Error} ->
	    DisplayPath = tpk_file:relative_path(In, BaseDir),
	    tpk_util:show_error_info(DisplayPath, "Error: ", Error)
    end.
