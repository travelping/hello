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
-module(hello_stateless_http_server).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-include("internal.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").

init({tcp, http}, Req, [IP]) ->
    {ok, Req, IP}.

handle(Req, State = IP) ->
    case hello_stateless_httpd:lookup_service(IP, Req) of
        {undefined, Req1} ->
            ResponseJSON = json_error(service_not_found),
            Req2 = log_request(hello, Req1, ResponseJSON),
            {ok, ReturnReq} = json_response(Req2, 404, ResponseJSON);

        {Module, Req1} ->
            {Method, Req2} = cowboy_http_req:method(Req1),
            case lists:member(Method, ['PUT', 'POST']) of
                true ->
                    {Body, Req3} = get_body(Req2),
                    ResponseJSON = hello:run_stateless_binary_request(Module, Body),
                    Req4 = log_request(Module, Req3, ResponseJSON),
                    {ok, ReturnReq} = json_response(Req4, 200, ResponseJSON);
                false ->
                    ResponseJSON = json_error(bad_http_method),
                    Req2 = log_request(Module, Req2, ResponseJSON),
                    {ok, ReturnReq} = json_response(Req2, 400, ResponseJSON)
            end
    end,
    {ok, ReturnReq, State}.

terminate(_Req, _State) ->
    ok.

json_response(Req, Code, Body) ->
    {ok, Vsn} = application:get_key(hello, vsn),
    Headers   = [{'Content-Type', <<"application/json">>},
                 {'Server', erlang:list_to_binary("hello/" ++ Vsn)}],
    cowboy_http_req:reply(Code, Headers, Body, Req).

json_error(Resp = #response{}) -> hello_proto:response_json(Resp);
json_error(Msg)                -> json_error(hello_proto:std_error(Msg)).

log_request(CallbackModule, Request, ResponseJSON) ->
    {ok, _Log} = hello_request_log:open(CallbackModule, self()),
    try
        {Host, Req1} = cowboy_http_req:raw_host(Request),
        {Port, Req2} = cowboy_http_req:port(Req1),
        {Path, Req3} = cowboy_http_req:raw_path(Req2),
        {Body, Req4} = get_body(Req3),
        URI          = #ex_uri{authority = #ex_uri_authority{host = Host, port = Port},
                               path = Path, scheme = "http"},
        URIBinary    = list_to_binary(ex_uri:encode(URI)),
        hello_request_log:request(CallbackModule, URIBinary, Body, ResponseJSON),
        Req4
    after
        hello_request_log:close(CallbackModule)
    end.

get_body(Req) ->
    case cowboy_http_req:body(Req) of
        {ok, Body, Req2} -> {Body, Req2};
        {error, badarg}  -> {<<>>, Req}
    end.
