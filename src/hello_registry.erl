% Copyright (c) 2010-2011 by Travelping GmbH <info@travelping.com>

% Permission is hereby granted, free of charge, to any person obtaining a
% copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
% DEALINGS IN THE SOFTWARE.

% @private
-module(hello_registry).

-behaviour(gen_server).

-compile({no_auto_import,[register/2, unregister/1]}).

-export([
    start/0, start_link/0, all/1, register_link/2, unregister_link/1,
    register/2, register/3, unregister/1, lookup/1
]).

%% gen_server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("internal.hrl").

-define(TABLE, hello_registry_tab).
-define(SERVER, hello_registry).

%% --------------------------------------------------------------------------------
%% -- API
-spec start() -> {ok, pid()}.
start() ->
    gen_server:start({local, hello_registry}, ?MODULE, {}, []).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, hello_registry}, ?MODULE, {}, []).

%% --------------------------------------------------------------------------------
%% -- general API to register, unregister, update or lookup stuff
register_link(Key, Value) ->
    register(Key, Value),
    link(whereis(?SERVER)).

register(Key, Value) ->
    gen_server:call(?SERVER, {register, Key, undefined, Value}).

register(Key, Pid, Value) ->
    gen_server:call(?SERVER, {register, Key, Pid, Value}).

unregister_link(Name) ->
    unregister(Name),
    unlink(whereis(?SERVER)).

unregister(Name) ->
    gen_server:call(?SERVER, {unregister, Name}).

lookup(Key) ->
    case ets:lookup(?TABLE, Key) of
        [] ->
            {error, not_found};
        [{Key, Pid, Value}] ->
            {ok, Pid, Value}
    end.

all(Type) ->
    Table = ?TABLE,
    [{Name, Pid, Args} || [Name, Pid, Args] <- ets:match(Table, {{Type, '$1'}, '$2', '$3'})].

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
init({}) ->
    process_flag(trap_exit, true),
    Table = ets:new(?TABLE, [protected, ordered_set, named_table, {read_concurrency, true}]),
    {ok, Table}.

handle_call({register, Key, Pid, Data}, _From, Table) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            erlang:monitor(process, Pid),
            ets:insert(Table, {Key, Pid, Data}),
            {reply, ok, Table};
        false ->
            {reply, {error, pid_not_alive}, Table}
    end;
handle_call({register, Key, undefined, Data}, _From, Table) ->
    ets:insert(Table, {Key, undefined, Data}),
    {reply, ok, Table};

handle_call({unregister, Key}, _From, Table) ->
    case ets:lookup(Table, Key) of
        [] ->
            %% not registered, skip
            {reply, ok, Table};
        [{Key, _Data}] ->
            ets:delete(Table, Key),
            {reply, ok, Table}
    end;

handle_call(_Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_info({'EXIT', _From, Reason}, Table) ->
    {stop, Reason, Table};
handle_info({'DOWN', _MRef, process, Pid, _Reason}, Table) ->
    ets:match_delete(Table, {'_', Pid, '_'}),
    {noreply, Table};
handle_info(_InfoMsg, State) ->
    {noreply, State}.

terminate(_Reason, Table) ->
    ets:delete(Table),
    ok.

%% unused callbacks
handle_cast(_Cast, State) ->
    {noreply, State}.

code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.
