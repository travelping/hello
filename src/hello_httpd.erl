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
-export([start_link/0]).

-define(OurModule, hello_inets).

start_link() ->
    Defaults = default_config(),
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
    InetsConfig = merge_config(Defaults, User),
    inets:start(httpd, InetsConfig, stand_alone).

default_config() ->
    DefaultRoot  = filename:join(code:priv_dir(hello), "server_root"),
    Port         = getenv(httpd_port, 5671),
    Prefix       = getenv(httpd_rpc_prefix, "/rpc"),
    BindAddr     = getenv(httpd_bind_address, any),
    Name         = getenv(httpd_server_name, "localhost"),

    ErrorLog    = filename:absname("inets_error.log"),
    TransferLog = filename:absname("inets_transfer.log"),
    SecurityLog = filename:absname("inets_security.log"),

    [{modules, [?OurModule]},
     {port, Port},
     {server_name, Name},
     {bind_address, BindAddr},
     {json_rpc_prefix, Prefix},
     {error_log, ErrorLog},
     {transfer_log, TransferLog},
     {security_log, SecurityLog},
     {server_root,   DefaultRoot},
     {document_root, DefaultRoot}].

merge_config(Defaults, UserIn) ->
    User = case proplists:get_value(server_root, UserIn) of
               undefined -> lists:map(fun absnamed/1, UserIn);
               _Else     -> UserIn
           end,
    Snd = fun (_, _, Y) -> Y end,
    Dic = dict:merge(Snd, dict:from_list(Defaults), dict:from_list(User)),
    Modules = dict:fetch(modules, Dic),
    Merged  = case lists:member(?OurModule, Modules) of
                  true  -> Dic;
                  false -> dict:store(modules, [?OurModule | Modules], Dic)
              end,
    dict:to_list(Merged).

absnamed({Key, Value}) ->
    PathParam = [document_root, error_log, security_log, transfer_log,
                 transfer_disk_log, error_disk_log, security_disk_log,
                 ssl_ca_certificate_file, ssl_certificate_file, auth_user_file,
                 auth_group_file, security_group_file],
    case lists:member(Key, PathParam) of
        true  -> {Key, filename:absname(Value)};
        false ->
           case Key of
              mime_types ->
                  if is_tuple(Value) -> {Key, Value};
                     true            -> {Key, filename:absname(Value)}
                  end;
              directory ->
                  {Path, Props} = Value,
                  {Key, {Path, lists:map(fun absnamed/1, Props)}};
              security_directory ->
                  {Path, Props} = Value,
                  {Key, {Path, lists:map(fun absnamed/1, Props)}};
              _ ->
                  {Key, Value}
          end
    end.

getenv(Key, Default) ->
  case application:get_env(Key) of
    {ok, Val} -> Val;
    undefined -> Default
  end.
