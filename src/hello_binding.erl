% Copyright (c) 2011 by Travelping GmbH <info@travelping.com>

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
-module(hello_binding).
-export([register_link/2, unregister_link/2, lookup/2, all/0, binds_for_uri/1]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("hello.hrl").

register_link(ExUriURL, HandlerMod) ->
    RouterKey = hello_lib:to_binary(HandlerMod:router_key()),
    Name = hello_lib:to_binary(HandlerMod:name()),
    Port = hello_listener:port(ExUriURL),
    hello_registry:register_link({binding, {ExUriURL, RouterKey}}, self(), {Name, Port}).

lookup(ExUriURL, RouterKey) ->
    hello_registry:lookup({binding, {ExUriURL, RouterKey}}).

unregister_link(ExUriURL, HandlerMod) ->
    RouterKey = hello_lib:to_binary(HandlerMod:router_key()),
    hello_registry:unregister_link({binding, {ExUriURL, RouterKey}}).

all() -> hello_registry:all(binding).

binds_for_uri(ExUriURL) ->
    hello_registry:match(binding, {ExUriURL, '_'}).
