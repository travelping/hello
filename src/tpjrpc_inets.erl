%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tpjrpc_inets).
-export([do/1, load/2, store/2, remove/1]).

-include("jrpc_internal.hrl").
-record(mod,{init_data,
	         data = [],
             socket_type = ip_comm,
             socket,
             config_db,
             method,
             absolute_uri = [],
	         request_uri,
             http_version,
             request_line,
             parsed_header = [],
             entity_body,
             connection}).

do(ModData = #mod{request_uri = Path, entity_body = Body, config_db = Config}) ->
    Prefix = unslash(httpd_util:lookup(Config, json_rpc_prefix)),
    Resp = case path_service(Prefix, Path) of
               no_handle   -> ModData#mod.data;
               empty       -> json_error(404, service_missing);
               ServiceName ->
                   case tp_json_rpc_service:lookup(ServiceName) of
                       {ok, _Module} ->
                           case lists:member(ModData#mod.method, ["PUT", "POST"]) of
                               true ->
                                   JSON_Resp = tp_json_rpc:handle_request(ServiceName, Body),
                                   json_response(200, JSON_Resp);
                               false ->
                                   json_error(400, bad_http_method)
                           end;
                        {error, _} -> json_error(404, service_not_found)
                   end
           end,
    {proceed, Resp}.

json_response(Code, Body) ->
    Len = integer_to_list(byte_size(Body)),
    [{response, {response, [{code, Code}, {content_type, "application/json"}, {content_length, Len}], [Body]}}].

json_error(Code, Resp = #response{}) -> json_response(Code, tpjrpc_proto:response_json(Resp));
json_error(Code, Msg)                -> json_error(Code,    tpjrpc_proto:std_error(Msg)).

path_service(Prefix, PathIn) ->
    {Decoded, _Query} = httpd_util:split_path(PathIn),
    Path = unslash(Decoded),
    case lists:prefix(Prefix, Path) of
        true  ->
            case lists:nthtail(length(Prefix), Path) of
                [ServiceName | _Rest] -> ServiceName;
                _Else                 -> empty
            end;
        false -> no_handle
    end.

unslash(Path) ->
    case re:split(Path, "/", [{return, list}]) of
        []          -> [];
        [[] | Rest] -> Rest;
        List        -> List
    end.

%% misc inets callbacks
store({json_rpc_prefix, Path}, _Config) when is_list(Path) -> {ok, [{json_rpc_prefix, unslash(Path)}]}.
load("JsonRpcPrefix " ++ Prefix, []) -> {ok, [], {json_rpc_prefix, Prefix}}.
remove(_DB) -> ok.
