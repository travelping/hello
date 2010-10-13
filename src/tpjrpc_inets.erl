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
    Prefix = httpd_util:lookup(Config, json_rpc_prefix, ["rpc"]),
    Resp = case path_service(Prefix, Path) of
               no_handle   -> ModData#mod.data;
               empty       -> json_error(404, service_missing);
               ServiceName ->
                   case tp_json_rpc_service:lookup(ServiceName) of
                       {ok, _Module} ->
                           case ModData#mod.method of
                               "POST" ->
                                   case tpjrpc_proto:request_json(Body) of
                                       {ok, Request}  -> do_request(ServiceName, Request);
                                       {error, Error} -> json_error(400, Error)
                                   end;
                                _M    -> json_error(400, bad_http_method)
                           end;
                        {error, _} -> json_error(404, service_not_found)
                   end
           end,
    {proceed, Resp}.

do_request(ServiceName, Request = #request{method = Method, params = Params}) ->
    try
        Result = tp_json_rpc_service:handle_request(ServiceName, Request),
        JSON   = tpjrpc_proto:response_json(Result),
        json_response(200, JSON)
    catch
        What:How ->
            error_logger:error_msg("JSON-RPC call of method ~s/~s has crashed.~n"
                                   "Params: ~p~n"
                                   "Error: ~p:~p~n"
                                   "Trace:~n~p~n",
                                   [ServiceName, Method, Params, What, How, erlang:get_stacktrace()]),
            json_error(500, {What, How})
    end.

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
