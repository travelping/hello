%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tpjrpc_app).

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    tp_json_rpc_service:init(),

    RequestLog = case application:get_env(request_log_enabled) of
                     {ok, true} ->
                        {ok, RequestLogFile} = application:get_env(request_log_file),
                        {ok, Log} = tpjrpc_logger:open(RequestLogFile),
                        Log;
                     {ok, false} ->
                        undefined
                 end,
    {ok, Super} = tpjrpc_sup:start_link(),
    {ok, Super, RequestLog}.

stop(undefined) ->
    ok;
stop(Log) ->
    tpjrpc_logger:close(Log),
    ok.
