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
%% API
-export([start_link/0]).
-export([clients/0, start_client/2, start_named_client/3, stop_client/1]).

-behaviour(supervisor).
-export([init/1]).

%% ----------------------------------------------------------------------------------------------------
%% -- Public API
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc start a client process
start_client(URI, Opts) ->
    supervisor:start_child(?MODULE, [URI, Opts]).

%% @doc start a client process and register a name for it
start_named_client(Name, URI, Opts) ->
    supervisor:start_child(?MODULE, [Name, URI, Opts]).

%% @doc stop a client process
stop_client(Name) when is_atom(Name) ->
    case whereis(Name) of
        Pid when is_pid(Pid) -> supervisor:terminate_child(?MODULE, Pid);
        _                    -> ok
    end;
stop_client(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

%% @doc return a list of running connections
clients() ->
    [Child || {_, Child, _, _} <- supervisor:which_children(?MODULE)].

%% ----------------------------------------------------------------------------------------------------
%% -- supervisor callbacks
init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{hello_client, {hello_client, start_link, []},
            temporary, 500, worker, [hello_client]}]}}.
