%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tp_json_rpc).

-export([request/2]).

request(Service, JSON) when is_list(JSON) ->
    Resp = case tpjrpc_proto:request_json(JSON) of
               {ok, Request}  ->
                   tp_json_rpc_service:handle_request(Service, Request);
               {batch, Valid, Invalid} ->
                   Resps = tp_json_rpc_service:handle_request(Service, Valid),
                   Invalid ++ Resps;
               {error, Error} ->
                   Error
           end,
    tpjrpc_proto:response_json(Resp).
