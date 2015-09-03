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
-module(hello_supervisor).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-include("hello.hrl").
-include("hello_log.hrl").

-define(SERVER, hello_supervisor).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

init({}) ->
    Children = lists:flatmap(fun role_children/1, get_roles()),
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.

get_roles() ->
    case application:get_env(role) of
        {ok, client} ->
            [client];
        {ok, server} ->
            [server];
        {ok, Role} when is_list(Role) ->
            Exess = lists:subtract(Role, [client, server]),
            if
                length(Exess) > 0 ->
                    ?LOG_INFO("Hello supervisor got invalid list '~p' of roles.", [Role],
                            [{hello_error_reason, invalid_roles}], ?LOGID59);
                true              -> ok
            end,
            lists:subtract(Role, Exess);
        {ok, Role} ->
            ?LOG_INFO("Hello supervisor got invalid role '~p'.", [Role],
                            [{hello_error_reason, invalid_roles}], ?LOGID60),
            [client, server];
        undefined ->
            [client, server]
    end.

role_children(client) ->
    [{client_sup,   {hello_client_sup, start_link, []}, permanent, infinity, supervisor, [hello_client_sup]}];
role_children(server) ->
    [{registry,     {hello_registry, start_link, []}, permanent, 1000, worker, [hello_registry]},
     {listener_sup, {hello_listener_supervisor, start_link, []}, permanent, infinity, supervisor, [hello_listener_supervisor]}];
role_children(_) ->
    [].
