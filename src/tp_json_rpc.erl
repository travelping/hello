%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tp_json_rpc).

-export([request/2, call/3]).

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

call(Host, Method, ArgList) when is_list(ArgList) ->
    Objectencode = tpjrpc_json:encode({obj, [{"method", Method}, {"params", ArgList}, {"id", 1}]}),
    Objectrequest = {Host, [], "application/json", Objectencode},
    {ok, {_Line, _Header, Body}} = httpc:request(post, Objectrequest, [], []),
    {ok, {obj, Props}, _Rest} = tpjrpc_json:decode(Body),
    case proplists:get_value("error", Props) of
        null ->
            Result = proplists:get_value("result", Props),
            {ok, Result};
        {obj, ErrorObject} ->
            case proplists:get_value("code", ErrorObject) of
                -32600 -> {error, invalid_request};
                -32601 -> {error, method_not_found};
                -32602 -> {error, invalid_params};
                -32603 -> {error, internal_error};
                Code when (Code >= -32099) and (Code =< -32000) ->
                    {error, server_error};
                Code -> {error, Code}
                 
            end
    end.

