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

-module(tpjrpc_proto).
-export([request/1, request_json/1, response/2, error_response/3, error_response/4,
         response_json/1, std_error/1, std_error/2]).
-include("jrpc_internal.hrl").

%% ----------------------------------------------------------------------
%% -- Responses

%% @type response()

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

%% @spec (Response) -> binary()
%%    Response = request() | [request()]
%% @doc Convert a response object to JSON.
response_json(empty_response) ->
    <<>>;
response_json(Resps) when is_list(Resps) ->
    Conc =
        lists:foldl(fun (Resp, Bin) ->
                        case Resp of
                            empty_response -> Bin;
                            _              -> <<Bin/binary, ",", (response_json(Resp))/binary>>
                        end
                    end, <<>>, Resps),
    case Conc of
        <<>>               -> <<>>;
        <<",",Res/binary>> -> <<"[", Res/binary, "]">>
    end;
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
    tpjrpc_json:encode(RespObj).

%% ----------------------------------------------------------------------
%% -- Requests

%% @type request()

%% @spec (JSON::string()) -> {ok, request()} | {error, response()} | {batch, Valid, Invalid}
%%     Valid   = [request()]
%%     Invalid = [response()]
%% @doc Create a request object from unparsed JSON.
%%      In case of a parse error or if the request does not obey the JSON-RPC standard,
%%      the appropriate JSON-RPC error response object is returned.
%% @see request/1
request_json(JSON) ->
    case tpjrpc_json:decode(JSON) of
        {error, _Error}      -> {error, std_error(parse_error)};
        {ok, Request, _Rest} -> request(Request)
    end.

%% @spec (JSON::tpjrpc_json:json_value()) -> {ok, request()} | {error, response()} | {batch, Valid, Invalid}
%%     Valid   = [request()]
%%     Invalid = [response()]
%% @doc Create a request object from parsed request structure
%%
%%      `{ok, Request}' is returned if the given request is valid.<br/>
%%      `{error, Response}' is returned if the given request is not valid.<br/>
%%      `{batch, Valid, Invalid}' is returned for batch requests. `Valid' contains
%%      all valid requests in the batch, `Invalid' is a list of error responses for the ones that were invalid.
request(Obj) ->
    case Obj of
        [] ->
            {error, std_error(invalid_request)};
        Lis when is_list(Lis) ->
            {Valid, Invalid} = lists:foldl(
                    fun (ReqObj, {Va, Iva}) ->
                            case single_request(ReqObj) of
                                {ok, Req}     -> {[Req | Va], Iva};
                                {error, Resp} -> {Va, [Resp | Iva]}
                            end
                    end, {[], []}, Lis),
            {batch, Valid, Invalid};
        _ ->
            single_request(Obj)
    end.

single_request({obj, Props}) ->
    try
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
        {ok, #request{version = Version, method = Method, params = Params, id = ID}}
    catch
        throw:invalid            -> {error, std_error(invalid_request)};
        throw:{invalid, RID, Vsn} -> {error, std_error(#request{id = RID, version = Vsn}, invalid_request)}
    end;
single_request(_Other) ->
    {error, std_error(invalid_request)}.

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
