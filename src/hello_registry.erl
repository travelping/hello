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
-export([start_link/0, register/3, multi_register/2, unregister/1, lookup/1, lookup_pid/1]).
-export([lookup_listener/1, lookup_listener/2, listener_key/2]).
-export([bindings/0, lookup_binding/4, binding_key/4]).
%% internal
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TABLE, hello_registry_tab).
-define(SERVER, hello_registry).

-include_lib("ex_uri/include/ex_uri.hrl").

%% --------------------------------------------------------------------------------
%% -- API
-type url() :: string().
-type protocol() :: atom().
-type name() :: term().
-type data() :: term().
-type address() :: inet:ip4_address() | inet:ip6_address() | {ipc, string()}.

-type listener_key() :: tuple().
-type binding_key() :: tuple().

%% @doc Start the registry server
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, hello_registry}, ?MODULE, {}, []).

%% @doc Atomically register a pid under the given name
%% @equiv multi_register([{Name, Data}], Pid)
-spec register(name(), data(), pid()) -> ok | {already_registered, pid(), data()}.
register(Name, Data, Pid) when is_pid(Pid) ->
    multi_register([{Name, Data}], Pid).

%% @doc Atomically register a pid under multiple names
%%   When one of the names is already registered, the function returns the pid and data
%%   of the existing process.
-spec multi_register(list({name(), data()}), pid()) -> ok | {already_registered, pid(), data()}.
multi_register(RegSpecs, Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {register, RegSpecs, Pid}).

%% @doc Lookup the pid and data for a given name
-spec lookup(name()) -> {ok, pid(), data()} | {error, not_found}.
lookup(Name) ->
    case ets:lookup(?TABLE, {name, Name}) of
        [] ->
            {error, not_found};
        [{{name, _}, Pid, Data}] ->
            {ok, Pid, Data}
    end.

-spec lookup_binding(atom(), binary(), inet:ip_port(), [binary()]) -> {ok, pid(), data()} | {error, not_found}.
lookup_binding(Protocol, Host, Port, Path) ->
    lookup(binding_key(Protocol, Host, Port, Path)).

-spec bindings() -> [{protocol(), url(), pid(), module()}].
bindings() ->
    [{Protocol, encode_binding_uri(Protocol, Host, Port, Path), Pid, Module} ||
        {Protocol, Host, Port, Path, Pid, Module} <- lookup_bindings_ms(?TABLE)].

-spec lookup_listener(listener_key()) -> {ok, pid(), module()} | {error, not_found}.
lookup_listener({listener, IP, Port}) ->
    lookup_listener(IP, Port).

-spec lookup_listener(address(), inet:ip_port() | undefined) -> {ok, pid(), module()} | {error, not_found}.
lookup_listener({ipc, Path}, undefined) ->
    lookup(listener_key({ipc, Path}, undefined));

lookup_listener(IP, Port) ->
    case lookup(listener_key({0,0,0,0}, Port)) of
        {ok, Pid, Module} ->
            {ok, Pid, Module};
        {error, not_found} ->
            lookup(listener_key(IP, Port))
    end.

%% @doc Lookup all names for the given pid
-spec lookup_pid(pid()) -> {ok, list(name())} | {error, not_found}.
lookup_pid(Pid) ->
    case lookup_pid_ms(?TABLE, Pid) of
        [] ->
            {error, not_found};
        Results ->
            {ok, Results}
    end.

-spec unregister(name()) -> ok.
unregister(Name) ->
    gen_server:call(?SERVER, {unregister, Name}).

-spec listener_key({ipc, filename:name()} | inet:ip4_address() | inet:ip6_address(), inet:ip_port() | undefined) -> listener_key().
listener_key({ipc, Path}, undefined) ->
    {listener, {ipc, Path}, undefined};
listener_key(IP, Port) when is_tuple(IP) andalso is_integer(Port) andalso (Port >= 0) ->
    {listener, IP, Port}.

-spec binding_key(protocol(), binary(), inet:ip_port(), [binary()]) -> binding_key().
binding_key(Protocol, Host, Port, Path) when is_atom(Protocol), is_binary(Host), is_list(Path) ->
    {binding, Protocol, Host, Port, Path}.

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
init({}) ->
    Table = ets:new(?TABLE, [protected, ordered_set, named_table, {read_concurrency, true}]),
    {ok, Table}.

handle_call({register, RegSpecs, Pid}, _From, Table) ->
    case is_process_alive(Pid) of
        true ->
            case any_registered_ms(Table, RegSpecs) of
                {OtherPid, OtherData} ->
                    {reply, {already_registered, OtherPid, OtherData}, Table};
                undefined ->
                    erlang:monitor(process, Pid),
                    NameTuples = [{{name, Name}, Pid, Data}  || {Name, Data} <- RegSpecs],
                    PidTuples  = [{{pid, Pid, Name}} || {Name, _} <- RegSpecs],
                    ets:insert(Table, NameTuples ++ PidTuples),
                    {reply, ok, Table}
            end;
        false ->
            {reply, {error, noproc}, Table}
    end;
handle_call({unregister, Name}, _From, Table) ->
    ets:delete(Table, {name, Name}),
    {reply, ok, Table};

handle_call(_Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_info({'DOWN', _MRef, process, Pid, _Reason}, Table) ->
    DelCount = delete_pid(Table, Pid),
    case DelCount of
        0 -> error_logger:error_report(io_lib:format("Got down message from unregistered pid: ~p~n", [Pid]));
        _ -> ok
    end,
    {noreply, Table};
handle_info(_InfoMsg, State) ->
    {noreply, State}.

%% unused callbacks
handle_cast(_Cast, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

%% --------------------------------------------------------------------------------
%% -- helpers
delete_pid(Table, Pid) ->
    NameMS = [{{{name, Name}, '_', '_'}, [], [true]} || Name <- lookup_pid_ms(Table, Pid)],
    PidMS  = {{{pid, Pid, '_'}}, [], [true]},
    ets:select_delete(Table, [PidMS | NameMS]).

lookup_pid_ms(Table, Pid) ->
    PidMS = [{{{pid, Pid, '$1'}}, [], ['$1']}],
    ets:select(Table, PidMS).

lookup_bindings_ms(Table) ->
    [{Protocol, Host, Port, Path, Pid, Data} ||
        {{name, {binding, Protocol, Host, Port, Path}}, Pid, Data} <- ets:match_object(Table, {{name, {binding, '_', '_', '_', '_'}}, '_', '_'})].

encode_binding_uri(Protocol, Host, Port, PathList) ->
    URI = #ex_uri{scheme    = atom_to_list(Protocol),
                  path      = encode_path(PathList),
                  authority = #ex_uri_authority{host = Host, port = Port}},
    binary_to_list(iolist_to_binary(ex_uri:encode(URI))).

encode_path([])        -> "";
encode_path(PathList)  -> "/" ++ string:join(lists:map(fun binary_to_list/1, PathList), "/").

any_registered_ms(Table, Names) ->
    NamesMS = [{{{name, Name}, '$1', '$2'}, [], [{{'$1', '$2'}}]} || {Name, _} <- Names],
    case ets:select(Table, NamesMS) of
        []               -> undefined;
        [FirstMatch | _] -> FirstMatch
    end.
