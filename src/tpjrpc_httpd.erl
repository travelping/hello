%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tpjrpc_httpd).
-export([start_link/0]).

start_link() ->
    Defaults = default_config(),
    User = case application:get_env(httpd_config) of
               {ok, File} ->
                   case file:consult(File) of
                       {ok, Terms} -> Terms;
                       {error, Reason} ->
                           error_logger:error_msg("httpd config file (~p) parse error:~n~p", [File, Reason]),
                           exit({error, config_file_syntax})
                   end;
               undefined ->
                   error_logger:info_msg("No httpd config file specified, starting with defaults.~n"),
                   []
           end,
    InetsConfig = merge_config(Defaults, User),
    inets:start(httpd, InetsConfig, stand_alone).

default_config() ->
    Port          = getenv(httpd_port, 5671),
    ServerRoot    = filename:absname(getenv(httpd_server_root, "server_root")),
    DocumentRoot  = filename:join(ServerRoot, "htdocs"),
    LogDir        = filename:absname(getenv(httpd_log_dir, filename:join(ServerRoot, "logs"))),
    TransferLog   = filename:join(LogDir, "access.log"),
    ErrorLog      = filename:join(LogDir, "error.log"),
    SecurityLog   = filename:join(LogDir, "security.log"),

    [{modules, [mod_auth, tpjrpc_inets, mod_log]},
     {port, Port},
     {server_name, "localhost"},
     {server_root, ServerRoot},
     {document_root, DocumentRoot},
     {error_log,    ErrorLog},
     {security_log, SecurityLog},
     {transfer_log, TransferLog},
     {mime_types,[{"html","text/html"},
                  {"css", "text/css"},
                  {"js",  "application/x-javascript"}]}].

merge_config(C1, C2) ->
    C1.

getenv(Key, Default) ->
  case application:get_env(Key) of
    {ok, Val} -> Val;
    undefined -> Default
  end.
