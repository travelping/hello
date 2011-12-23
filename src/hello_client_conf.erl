-module(hello_client_conf).
-export([start_link/0]).
-export([reconfigure/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% --------------------------------------------------------------------------------
%% -- API

%% @doc create and link a connection context
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

reconfigure() ->
	gen_server:call(?MODULE, reconfigure).

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
init([]) ->
	Clients = get_config(),
	lists:foreach(fun({Name, URI, Method, Opts}) -> hello_client:start_connection(Name, URI, Method, Opts) end, Clients),
	{ok, Clients}.

handle_call(reconfigure, _From, OldClients) ->
	case get_config() of
		NewClients when is_list(NewClients) ->
			ToStart = lists:subtract(NewClients, OldClients),
			ToStop  = lists:subtract(OldClients, NewClients),
			lists:foreach(fun({Name, _, _, _}) -> hello_client:stop_connection(Name) end, ToStop),
			lists:foreach(fun({Name, URI, Method, Opts}) -> hello_client:start_connection(Name, URI, Method, Opts) end, ToStart),
			{reply, ok, NewClients};
		_ ->
			{reply, ok, OldClients}
	end.

%% unused callbacks
handle_info(_Info, State) ->
    {noreply, State}.
handle_cast(_Cast, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
	ok.
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

get_config() ->
	case application:get_env(hello, clients) of
		{ok, Clients} ->
			validate_clients(Clients, Clients);
		_ ->
			[]
	end.

validate_clients([], Ret) ->
	Ret;
validate_clients([ClntSpec = {Name, URI, Method, Opts}|Clients], Ret) when is_atom(Name) ->
	case hello_client:validate_ctx(URI, Method, Opts) of
		ok ->
			validate_clients(Clients, Ret);
		Error ->
			error_logger:warning_report([{client, invalid}, {reason, Error}, ClntSpec]),
			error
	end;
validate_clients([ClntSpec|_], _) ->
	error_logger:warning_report([{client, invalid}, ClntSpec]),
	error.
