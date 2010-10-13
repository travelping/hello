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
    {ok, Super} = tpjrpc_sup:start_link(),
    {ok, Super, no_state}.

stop(_ST) -> ok.
