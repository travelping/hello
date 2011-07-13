-module(hello_registry_SUITE).
-compile(export_all).

-include("ct.hrl").
-include_lib("proper/include/proper.hrl").
-define(Mod, hello_registry).

% ---------------------------------------------------------------------
% -- test cases
proper_statemachine(_Config) ->
    true = proper:quickcheck(prop_registry_works()).

% ---------------------------------------------------------------------
% -- proper STM
initial_state() ->
    undefined.

command(undefined) ->
    {call, erlang, spawn, [fun () -> receive please_die -> ok end end]};
command({registered, Name, Data, Pid}) ->
    {call, ?Mod, register, [Name, term(), Pid]};
command({unregistered, Pid}) ->
    {call, ?Mod, register, [term(), term(), Pid]};
command({cleanup, Name, Data, Pid}) ->
    {call, ?Mod, unregister, [Name]};
command(stop) ->
    [].

next_state(undefined, Pid, {call, erlang, spawn, _Fun}) ->
    {unregistered, Pid};
next_state({unregistered, Pid}, _, {call, ?Mod, register, [Name, Data, Pid]}) ->
    {registered, Name, Data, Pid};
next_state({registered, Name, Data, Pid}, _, {call, ?Mod, register, [Name, _Data, Pid]}) ->
    {cleanup, Name, Data, Pid};
next_state(State, _, _) ->
    State.

precondition(_, _) ->
    true.

postcondition({unregistered, Pid}, {call, ?Mod, register, [Name, Data, Pid]}, ok) ->
    {ok, RegNames} = ?Mod:lookup_pid(Pid),
    {ok, Pid, Data} == ?Mod:lookup(Name) andalso
    lists:member(Name, RegNames);
postcondition({registered, Name, Data, Pid}, {call, ?Mod, register, [Name, _Data, _Pid]}, {already_registered, Pid, Data}) ->
    true;
postcondition({registered, Name, Data, Pid}, {call, ?Mod, register, [Name, _Data, _Pid]}, _) ->
    false;
postcondition({cleanup, Name, Data, Pid}, {call, ?Mod, unregister, [Name]}, ok) ->
    {error, not_found} == ?Mod:lookup(Name);
postcondition(_,_,_) ->
    true.

prop_registry_works() ->
    ?FORALL(Cmds, commands(?MODULE), begin
                {History,State,Result} = run_commands(?MODULE, Cmds),
                ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~w\n", [History, State, Result]),
                          aggregate(command_names(Cmds), Result =:= ok))
            end).

% ---------------------------------------------------------------------
% -- common_test callbacks
all() ->
    [proper_statemachine].

init_per_testcase(_Case, Config) ->
    case hello_registry:start_link() of
        {ok, Pid} ->
            [{registry, Pid} | Config];
        {error, Error} ->
            {fail, {registry_startup, Error}}
    end.

end_per_testcase(_Case, Config) ->
    ok.
