%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tpjrpc_proto).
-export([request/1, request_json/1, response/2, error_response/3, error_response/4,
         response_json/1, std_error/1, std_error/2]).
-include("jrpc_internal.hrl").

%% ----------------------------------------------------------------------
%% -- Responses

%% @equiv error_response(Req, Code, Msg, undefined)
error_response(Req, Code, Msg) ->
    error_response(Req, Code, Msg, undefined).

%% @spec (Request::request(), Code::integer(), Msg::string(), Data::any()) -> response()
%% @doc  Creates a response object that represents a JSON-RPC error response.
error_response(Req, Code, Msg, Data) ->
    DataT = case Data of
                undefined -> [];
                _         -> [{data, Data}]
            end,
    MsgBin = if is_binary(Msg) -> Msg;
                is_list(Msg)   -> list_to_binary(Msg);
                true           -> list_to_binary(io_lib:format("~w", [Msg]))
             end,
    #response{version = Req#request.version,
              id      = Req#request.id,
              error   = {obj, [{code, Code}, {message, MsgBin} | DataT]}}.

%% @type standard_error() = parse_error | invalid_request | method_not_found
%%                        | invalid_params | {invalid_params, string()}
%%                        | internal_error | server_error | {server_error, string()}
%%                        | string()

%% @spec (Error::standard_error()) -> response()
%% @doc Create a response object representing a JSON-RPC standard error.
%%      Called if nothing is known about the request.
std_error(Error) -> std_error(#request{}, Error).

%% @spec (Request::request(), Error::standard_error()) -> response()
%% @doc Create a response object representing a JSON-RPC standard error.
std_error(Req, Error) ->
    {Code, Msg} = case Error of
                      parse_error         -> {-32700, "Parse error"};
                      invalid_request     -> {-32600, "Invalid Request"};
                      method_not_found    -> {-32601, "Method not found"};
                      invalid_params      -> {-32602, "Invalid params"};
                      {invalid_params, M} -> {-32602, "Invalid params: " ++ M};
                      internal_error      -> {-32603, "Internal Error"};
                      server_error        -> {-32099, "Server Error"};
                      {E, _}              -> {-32099, io_lib:format("Server Error: ~p", [E])};
                      E                   -> {-32099, io_lib:format("Server Error: ~p", [E])}
                  end,
    Data = case Error of
              {server_error, Term} -> list_to_binary(io_lib:format("~p", [Term]));
              _                    -> undefined
           end,
    error_response(Req, Code, Msg, Data).

%% @spec (Request::request(), Result::tpjrpc_json:json()) -> response()
%% @doc  Creates a response object matching the given request.
response(#request{id = undefined}, _Resp) ->
    empty_response;
response(Req, Result) ->
    #response{version = Req#request.version,
              id      = Req#request.id,
              result  = Result}.

%% @spec (Response::response()) -> binary()
%% @doc Convert a response object to JSON.
response_json(empty_response) ->
    <<>>;
response_json(R = #response{version = RespVersion, error = RespError}) ->
    Version    = case RespVersion of
                     1 -> [];
                     _ -> [{jsonrpc, <<"2.0">>}]
                 end,
    Result     = case RespError of
                     undefined ->
                         [{error, null}, {result, maybe_null(R#response.result)}];
                     Error ->
                         [{result, null}, {error, maybe_null(Error)}]
                 end,
    ResOrError = case RespVersion of
                     1 -> Result;     % keep both result and error for v1.0 responses
                     _ -> tl(Result)  % omit result or error for v2.0
                 end,
    RespObj = {obj, Version ++ [{id, maybe_null(R#response.id)} | ResOrError]},
    list_to_binary(tpjrpc_json:encode(RespObj)).

%% ----------------------------------------------------------------------
%% -- Requests

%% @spec (JSON::string()) -> {ok, request()} | {error, response()}
%% @doc Create a request object from unparsed JSON.
%%      In case of a parse error or if the request does not obey the JSON-RPC standard,
%%      the appropriate JSON-RPC error response object is returned.
request_json(JSON) ->
    case tpjrpc_json:decode(JSON) of
        {error, _Error}      -> {error, std_error(parse_error)};
        {ok, Request, _Rest} -> request(Request)
    end.

%% @spec (JSON::tpjrpc_json:json_value()) -> {ok, request()} | {error, response()}
%% @doc Create a request object from parsed request structure
request(Obj) ->
    try if is_list(Obj) ->
                case Obj of
                    [] -> throw(invalid);
                    _  -> {ok, lists:map(fun single_request/1, Obj)}
                end;
           true ->
               {ok, single_request(Obj)}
        end
    catch
        throw:invalid            -> {error, std_error(invalid_request)};
        throw:{invalid, ID, Vsn} -> {error, std_error(#request{id = ID, version = Vsn}, invalid_request)}
    end.

single_request({obj, Props}) ->
    Version = req_version(Props),
    ID      = case Version of
                 2 -> proplists:get_value("id", Props);
                 1 -> case proplists:get_value("id", Props) of
                         undefined -> throw({invalid, null, Version});
                         null      -> undefined;
                         Value     -> Value
                      end
              end,
    Method = case property(Props, "method") of
                 Name when is_list(Name) or is_binary(Name) -> Name;
                 undefined -> throw({invalid, ID, Version});
                 _         -> throw({invalid, ID, Version})
             end,
    Params = case property(Props, "params", []) of
                 List when is_list(List)         -> List;
                 Obj = {obj, _} when Version > 1 -> Obj;
                 _                               -> throw({invalid, ID, Version})
             end,
    #request{version = Version, method = Method, params = Params, id = ID};
single_request(_Other) ->
    throw(invalid).

req_version(Props) ->
    case property(Props, "jsonrpc") of
        <<"2.0">> -> 2;
        <<"1.2">> -> 2;
        <<"1.0">> -> 1;
        undefined -> 1;
        _Other    -> throw(invalid)
    end.

property(Plist, Key) ->
    property(Plist, Key, undefined).
property(Plist, Key, Default) ->
    case proplists:get_value(Key, Plist, undefined) of
        undefined -> Default;
        null      -> Default;
        Value     -> Value
    end.

maybe_null(undefined) -> null;
maybe_null(null)      -> null;
maybe_null(Term)      -> Term.
