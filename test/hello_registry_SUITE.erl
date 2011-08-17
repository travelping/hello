-module(hello_registry_SUITE).
-compile(export_all).

-include("ct.hrl").
-include_lib("proper/include/proper.hrl").
-define(Mod, hello_registry).

% ---------------------------------------------------------------------
% -- common_test callbacks
all() ->
    [proper_statemachine].

% ---------------------------------------------------------------------
% -- test cases
proper_statemachine(_Config) ->
    Output = fun (Format, Data) -> io:format(user, Format, Data) end,
    true   = proper:quickcheck(prop_registry_works(), [{numtests, 1000}, {on_output, Output}]).

% ---------------------------------------------------------------------
% -- proper STM
-record(state, {spawned = [], registrations = []}).

initial_state() ->
    #state{}.

command(#state{spawned = Spawned, registrations = Registrations}) ->
    SpawnFreq = max(max(length(Spawned), length(Registrations)), 1),
    frequency([{length(Spawned),       {call, ?Mod, register, [term(), term(), oneof(Spawned)]}},
               {length(Registrations), {call, ?MODULE, unregister_helper, [oneof(Registrations)]}},
               {length(Spawned),       {call, ?MODULE, exit_helper, [oneof(Spawned), normal]}},
               {SpawnFreq,             {call, ?MODULE, spawn_helper, []}}]).

next_state(State, Pid, {call, ?MODULE, spawn_helper, []}) ->
    State#state{spawned = [Pid | State#state.spawned]};

next_state(State, _V, {call, ?Mod, register, [Name, _, Pid]}) ->
    case proplists:get_value(Name, State#state.registrations) of
        undefined ->
            %% It's not registered, should be added now
            State#state{registrations = [{Name, Pid} | State#state.registrations]};
        _SomePid ->
            %% It was already registered, don't add it
            State
    end;

next_state(State, _V, {call, ?MODULE, unregister_helper, [{Name, _Pid}]}) ->
    State#state{registrations = proplists:delete(Name, State#state.registrations)};

next_state(State, _V, {call, ?MODULE, exit_helper, [Pid, _Reason]}) ->
    NewRegistrations = [{N, P} || {N, P} <- State#state.registrations, P /= Pid],
    NewSpawned = lists:delete(Pid, State#state.spawned),
    State#state{registrations = NewRegistrations, spawned = NewSpawned};

next_state(State, _, _C) ->
    io:format(user, "===>~p~p~n",[State,_C]),
    State.

precondition(_, _) ->
    true.

postcondition(#state{registrations = Registrations}, {call, ?Mod, register, [Name, Data, Pid]}, ok) ->
    case proplists:get_value(Name, Registrations) of
        undefined ->
            %% was not registered
            ShouldRegNames = [Name] ++ [N || {N, P} <- Registrations, P == Pid],
            {ok, ActualRegNames} = ?Mod:lookup_pid(Pid),

            RegNamesTest = (lists:sort(ActualRegNames) == lists:sort(ShouldRegNames)),
            LookupTest   = (?Mod:lookup(Name) == {ok, Pid, Data}),

            LookupTest and RegNamesTest;
        _ ->
            %% shouldn't happen
            false
    end;

postcondition(#state{registrations = Registrations}, {call, ?Mod, register, [Name, _Data, _Pid]}, {already_registered, OtherPid, OtherData}) ->
    case proplists:get_value(Name, Registrations) of
        undefined ->
            false;
        OtherPid ->
            (?Mod:lookup(Name) == {ok, OtherPid, OtherData})
    end;

postcondition(#state{}, {call, ?Mod, register, [_Name, _Data, _Pid]}, _) ->
    false;

postcondition(#state{}, {call, ?MODULE, unregister_helper, [{Name, Pid}]}, ok) ->
    LookupTest = (?Mod:lookup(Name) == {error, not_found}),

    case ?Mod:lookup_pid(Pid) of
        {error, not_found} ->
            RegNamesTest = true;
        {ok, ActualRegNames} ->
            RegNamesTest = (not lists:member(Name, ActualRegNames))
    end,

    LookupTest and RegNamesTest;

postcondition(#state{}, {call, ?MODULE, unregister_helper, [{_Name, _Pid}]}, _) ->
    false;

postcondition(#state{}, {call, ?MODULE, exit_helper, [Pid, _Reason]}, ok) ->
    ?Mod:lookup_pid(Pid) == {error, not_found};

postcondition(#state{}, {call, ?MODULE, exit_helper, [_Pid, _Reason]}, noproc) ->
    false;

postcondition(_,_,_) ->
    true.

prop_registry_works() ->
    ?FORALL(Cmds, commands(?MODULE),
            begin
                {ok, Pid} = hello_registry:start(),
                {History,State,Result} = run_commands(?MODULE, Cmds),
                %% cleanup
                exit(Pid, shutdown),
                ?WHENFAIL(io:format(user, "History: ~w\nState: ~w\nResult: ~w\n", [History, State, Result]),
                          aggregate(command_names(Cmds), Result =:= ok))
            end).

%% helpers
unregister_helper({Name, _Pid}) ->
    ?Mod:unregister(Name).

spawn_helper() ->
    spawn(fun () -> receive {please_die, Reason} -> exit(Reason) end end).

exit_helper(Pid, Reason) ->
    case erlang:is_process_alive(Pid) of
        false ->
            noproc;
        true ->
            Ref = monitor(process, Pid),
            Pid ! {please_die, Reason},
            receive
                {'DOWN', Ref, process, Pid, Reason} -> ok
            end
    end.
