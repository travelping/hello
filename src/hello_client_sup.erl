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
-module(hello_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([connections/0, start_connection/3, start_connection/4, stop_connection/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc start a connection
start_connection(URI, Method, Opts) ->
	supervisor:start_child(?MODULE, [URI, Method, Opts]).

%% @doc start a connection and register a name for it
start_connection(Name, URI, Method, Opts) ->
	supervisor:start_child(?MODULE, [Name, URI, Method, Opts]).

%% @doc stop a connection
stop_connection(Name) when is_atom(Name) ->
	case whereis(Name) of
		Pid when is_pid(Pid) -> supervisor:terminate_child(?MODULE, Pid);
		_ -> {error, undefined}
	end.

%% @doc return a list of running connections
connections() ->
	lists:map(fun({_, Child, _, _}) -> Child end, supervisor:which_children(?MODULE)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, {{simple_one_for_one, 0, 1},
          [{hello_client, {hello_client, start_link, []},
            temporary, brutal_kill, worker, [hello_client]}]}}.
