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
-module(hello_handler_supervisor).

-behaviour(supervisor).
-export([start_handler/1, stop_handler/1]).
-export([start_link/0, init/1]).

-include("internal.hrl").
-define(SERVER, hello_handler_supervisor).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

start_handler(Binding = #binding{handler_type = HandlerMod}) ->
	ChildSpec = { 	Binding,
					{gen_server, start_link, [HandlerMod, Binding, []]},
					transient,
					infinity,
					worker,
					dynamic
				},
	supervisor:start_child(?SERVER, ChildSpec).

stop_handler(Binding) ->
    case hello_registry:lookup_handler(Binding) of
        {ok, _Handler} ->
			ok = supervisor:terminate_child(?SERVER, Binding),
			ok = supervisor:delete_child(?SERVER, Binding);
        _ ->
            ok
    end.

init(_Arg) ->
    RestartStrategy = {one_for_one, 5, 10},
	{ok, {RestartStrategy, []}}.
