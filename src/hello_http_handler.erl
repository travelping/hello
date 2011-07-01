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
-module(hello_http_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-include("internal.hrl").

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {PathInfo, _} = cowboy_http_req:path_info(Req),
    case PathInfo of
        [] ->
            {ok, Req2} = json_error(Req, 404, service_missing);
        [SBin | _] ->
            ServiceName = binary_to_list(SBin),
            case hello_service:lookup(ServiceName) of
                {ok, _Module} ->
                    {Method, _} = cowboy_http_req:method(Req),
                    case lists:member(Method, ['PUT', 'POST']) of
                        true ->
                            {ok, Body, _} = cowboy_http_req:body(Req),
                            JSON_Resp = hello:handle_request(ServiceName, Body),
                            {ok, Req2} = json_response(Req, 200, JSON_Resp);
                        false ->
                            {ok, Req2} = json_error(Req, 400, bad_http_method)
                    end;
                {error, _} ->
                    {ok, Req2} = json_error(Req, 404, service_not_found)
            end
    end,
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

json_response(Req, Code, Body) ->
    {ok, Vsn} = application:get_key(hello, vsn),
    Headers   = [{'Content-Type', <<"application/json">>},
                 {'Server', erlang:list_to_binary("hello/" ++ Vsn)}],
    cowboy_http_req:reply(Code, Headers, Body, Req).

json_error(Req, Code, Resp = #response{}) -> json_response(Req, Code, hello_proto:response_json(Resp));
json_error(Req, Code, Msg)                -> json_error(Req, Code, hello_proto:std_error(Msg)).
