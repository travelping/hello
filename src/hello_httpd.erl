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
-module(hello_httpd).
-export([start/1, stop/1]).

-define(HANDLER, hello_http_handler).

start(ServerName) ->
    User = case application:get_env(httpd_config) of
               {ok, File} ->
                   case file:consult(File) of
                       {ok, Terms} -> Terms;
                       {error, Reason} ->
                           error_logger:error_msg("httpd config file (~p) parse error: ~p~n", [File, Reason]),
                           exit({error, config_file_syntax})
                   end;
               undefined ->
                   error_logger:info_msg("No httpd config file specified, starting with defaults.~n"),
                   []
           end,
    Config   = merge_config(default_config(), User),
    Match    = unslash(proplists:get_value(prefix, Config)) ++ ['...'],
    Dispatch = [{'_', [{Match, ?HANDLER, []}]}],
    cowboy:start_listener(ServerName, proplists:get_value(acceptors, Config),
        cowboy_tcp_transport, [{port, proplists:get_value(port, Config)}],
        cowboy_http_protocol, [{dispatch, Dispatch}]).

stop(ServerName) ->
    cowboy:stop_listener(ServerName).

default_config() ->
    [{port,      getenv(httpd_port, 5671)},
     {prefix,    getenv(httpd_rpc_prefix, "/rpc")},
     {acceptors, getenv(httpd_acceptors, 10)},
     {bind_addr, getenv(httpd_bind_address, any)}].

merge_config(Config1, Config2) ->
    lists:keymerge(1, lists:keysort(1, Config1), lists:keysort(1, Config2)).

getenv(Key, Default) ->
  case application:get_env(Key) of
    {ok, Val} -> Val;
    undefined -> Default
  end.

unslash(Path) ->
    case re:split(Path, "/", [{return, binary}]) of
        []            -> [];
        [<<>> | Rest] -> Rest;
        List          -> List
    end.
