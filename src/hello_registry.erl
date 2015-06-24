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
    start/0, start_link/0, all/1, register_link/2, register_link/3, unregister_link/1,
    register/2, register/3, unregister/1, lookup/1, match/2
]).

%% gen_server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("hello.hrl").
-include("hello_log.hrl").

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
    register_link(Key, undefined, Value).

register_link(Key, Pid, Value) ->
    link(whereis(?SERVER)),
    register(Key, Pid, Value).

register(Key, Value) ->
    gen_server:call(?SERVER, {register, Key, undefined, Value}).

register(Key, Pid, Value) ->
    gen_server:call(?SERVER, {register, Key, Pid, Value}).

unregister_link(Name) ->
    unlink(whereis(?SERVER)),
    unregister(Name).

unregister(Name) ->
    gen_server:call(?SERVER, {unregister, Name}).

lookup(Key) ->
    case ets:lookup(?TABLE, Key) of
        [] ->
            {error, not_found};
        [{Key, Pid, Value, _}] ->
            {ok, Pid, Value}
    end.

all(Type) ->
    Table = ?TABLE,
    [{Name, Pid, Args, Ref} || [Name, Pid, Args, Ref] <- ets:match(Table, {{Type, '$1'}, '$2', '$3', '$4'})].

match(Type, Match) ->
    [{Name, Pid, Args, Ref} || {{_, Name}, Pid, Args, Ref} <- ets:match_object(?TABLE, {{Type, Match}, '_', '_', '_'})].

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
init({}) ->
    process_flag(trap_exit, true),
    Table = ets:new(?TABLE, [protected, ordered_set, named_table, {read_concurrency, true}]),
    {ok, Table}.

handle_call({register, Key, Pid, Data}, _From, Table) ->
    case register(Key, Pid, Data, Table) of
        true -> {reply, ok, Table};
        Error -> {reply, Error, Table}
    end;

handle_call({unregister, Key}, _From, Table) ->
    case ets:lookup(Table, Key) of
        [] ->
            %% not registered, skip
            {reply, ok, Table};
        [{Key, _, _, Ref}] ->
            dnssd_clean(Ref),
            ets:delete(Table, Key),
            {reply, ok, Table}
    end;

handle_call(_Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_info({'EXIT', From, Reason}, Table) ->
    ?LOG_WARNING("~p: exit with reason ~p", [From, Reason]),
    {noreply, Table};
handle_info({'DOWN', _MRef, process, Pid, Reason}, Table) ->
    Objects = ets:match(Table, {'$1', Pid, '_', '_'}),
    ?LOG_WARNING("~p: down ~p with reason ~p", [Pid, Objects, Reason]),
    spawn(fun() -> [down(Object)|| Object <- Objects] end),
    {noreply, Table};
handle_info({dnssd, _Ref, Msg}, State) ->
    ?LOG_DEBUG("dnssd Msg: ~p", [Msg]),
    {noreply, State};
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

%% --------------------------------------------------------------------------------
%% -- helpers
register(Key, Pid, Data, Table) ->
    case Pid =:= undefined orelse is_process_alive(Pid) of
        true ->
            is_pid(Pid) andalso monitor_(Table, Pid),
            ?LOG_DEBUG("Register ~p for pid ~p with data: ~p", [Key, Pid, Data]),
            bind(Key, Pid, Data, Table);
        false -> 
            ?LOG_ERROR("Tried to register ~p for pid ~p, but pid not alive", [Key, Pid]),
            {error, pid_not_alive}
    end.

monitor_(Table, Pid) ->
    ets:select_count(Table, [{{'_', '$1', '_', '_'},
                              [{'==', '$1', Pid}],
                              [true]}]) == 0
    andalso erlang:monitor(process, Pid).

bind({binding, {_Url, _RouterKey}} = Key, Pid, {Data, Port}, Table) ->
    % TODO: That is very dangarous, as single error on service, will simple crash the whole registry
    [App, Name] = string:tokens(binary_to_list(Data), "/"),
    Ref = dnss_register(App, Name, Port),
    ets:insert(Table, {Key, Pid, Data, Ref});
bind(Key, Pid, Data, Table) -> ets:insert(Table, {Key, Pid, Data, undefined}).

down([{listener, Key}]) -> hello_listener:stop(Key);
down([Key]) -> hello_registry:unregister(Key).

do_dnss_register(App, Name, Port) ->
    ?LOG_INFO("dnss register ~p/~p on port ~p", [App, Name, Port]),
    case dnssd:register(Name, <<"_", App/binary, "._tcp">>, Port) of
        {ok, Ref} -> Ref;
        _ -> ok
    end.

dnss_register(App, Name, Port)
  when is_list(App), is_list(Name), is_integer(Port) ->
    do_dnss_register(list_to_binary(App), list_to_binary(Name), Port);
dnss_register(_, _, _) -> ok.

dnssd_clean(Ref) when is_reference(Ref) -> dnssd:stop(Ref);
dnssd_clean(_) -> ok.
