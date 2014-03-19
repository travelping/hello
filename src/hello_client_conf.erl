% Copyright 2011-2012, Travelping GmbH <info@travelping.com>

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

%% @private
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
	lists:foreach(fun({Name, URI, Opts}) -> hello_client:start_supervised(Name, URI, Opts) end, Clients),
	{ok, Clients}.

handle_call(reconfigure, _From, OldClients) ->
	case get_config() of
		NewClients when is_list(NewClients) ->
			ToStart = lists:subtract(NewClients, OldClients),
			ToStop  = lists:subtract(OldClients, NewClients),
			lists:foreach(fun({Name, _, _, _}) -> hello_client:stop_supervised(Name) end, ToStop),
			lists:foreach(fun({Name, URI, Opts}) -> hello_client:start_supervised(Name, URI, Opts) end, ToStart),
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
validate_clients([ClntSpec = {Name, URI, Opts}|Clients], Ret) when is_atom(Name) ->
	case hello_client:validate_options(URI, Opts) of
		ok ->
			validate_clients(Clients, Ret);
		Error ->
			error_logger:warning_report([{client, invalid}, {reason, Error}, ClntSpec]),
			error
	end;
validate_clients([ClntSpec|_], _) ->
	error_logger:warning_report([{client, invalid}, ClntSpec]),
	error.
