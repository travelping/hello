% Copyright 2012, Travelping GmbH <info@travelping.com>

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
-module(hello_http_client).
-behaviour(hello_client).
-export([validate_options/1, init/2, send_request/3, handle_info/3, terminate/3]).

%% internal
-export([http_send/4]).

-include("internal.hrl").

-record(http_options, {
    ib_opts :: list({atom(), term()}),
    method = post :: 'put' | 'post'
}).

-record(http_state, {
    url :: string(),
    options :: #http_options{}
}).

validate_options(Options) ->
    validate_options(Options, #http_options{ib_opts = [{socket_options, [{reuseaddr, true }]}]}).

validate_options([{method, put} | R], Opts) ->
    validate_options(R, Opts#http_options{method = put});
validate_options([{method, post} | R], Opts) ->
    validate_options(R, Opts#http_options{method = post});
validate_options([{method, _}|_], _) ->
    {error, "invalid HTTP method"};
validate_options([{Option, Value} | R], Opts) when is_atom(Option) ->
    validate_options(R, Opts#http_options{ib_opts = [{Option, Value} | Opts#http_options.ib_opts]});
validate_options([_ | R], Opts) ->
    validate_options(R, Opts);
validate_options([], Opts) ->
    {ok, Opts}.

init(URL, Options) ->
    {ok, #http_state{url = ex_uri:encode(URL), options = Options}}.

send_request(ClientCtx, Request, State = #http_state{url = URL, options = Options}) ->
    spawn(?MODULE, http_send, [ClientCtx, Request, URL, Options]),
    {noreply, State}.

handle_info(_ClientCtx, _Info, State) ->
    {noreply, State}.

terminate(_ClientCtx, _Reason, _State) ->
    ok.

%% ----------------------------------------------------------------------------------------------------
%% -- Helpers
http_send(ClientCtx, Request, URL, #http_options{method = Method, ib_opts = Opts}) ->
    EncRequest = hello_proto:encode(Request),
    {ok, Vsn} = application:get_key(hello, vsn),
    MimeType = hello_proto:mime_type(Request),
    Headers = [{"Content-Type", binary_to_list(MimeType)},
               {"Accept", binary_to_list(MimeType)},
               {"User-Agent", "hello/" ++ Vsn}],
    ResponseBody =
        case ibrowse:send_req(URL, Headers, Method, EncRequest, Opts) of
            {ok, "200", _, Body} ->
                Body;
            {ok, "201", _, Body} ->
                Body;
            {ok, "202", _, Body} ->
                Body;

            %% gotcha, those clauses don't return
            {ok, HttpCode, _, _} ->
                hello_client:client_ctx_reply(ClientCtx, {error, {transport, list_to_integer(HttpCode)}}),
                exit(normal);
            {error, Reason} ->
                hello_client:client_ctx_reply(ClientCtx, {error, {transport, Reason}}),
                exit(normal)
        end,
    ClientReply =
        case Request of
            #request{reqid = undefined} ->
                ok;
            #request{} ->
                case hello_proto:decode(Request, ResponseBody) of
                    #response{result = Result} ->
                        {ok, Result};
                    Error = #error{} ->
                        {error, hello_proto:error_resp_to_error_reply(Error)};
                    _ ->
                        {error, bad_response}
                end;
            #batch_request{} ->
                case hello_proto:decode(Request, ResponseBody) of
                    #batch_response{responses = BatchResps} ->
                        lists:map(fun (#response{result = Result}) -> {ok, Result};
                                      (Error = #error{}) -> {error, hello_proto:error_resp_to_error_reply(Error)}
                                  end, BatchResps);
                    _ ->
                        {error, {http, bad_response}}
                end
        end,
    hello_client:client_ctx_reply(ClientCtx, ClientReply).
