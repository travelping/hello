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
-export([validate_options/1, init/2, send_request/3, handle_info/2, terminate/2]).

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

send_request(Request, From, State = #http_state{url = URL, options = Options}) ->
    spawn(?MODULE, http_send, [Request, From, URL, Options]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% ----------------------------------------------------------------------------------------------------
%% -- Helpers
http_send(Request, From, URL, #http_options{method = Method, ib_opts = Opts}) ->
    EncRequest = hello_proto:encode(Request),
    {ok, Vsn} = application:get_key(hello, vsn),
    MimeType = hello_proto:mime_type(Request),
    Headers = [{"Content-Type", MimeType},
               {"Accept", MimeType},
               {"User-Agent", "hello/" ++ Vsn}],
    Resp = case ibrowse:send_req(URL, Headers, Method, EncRequest, Opts) of
               {error, Reason} ->
                   {error, {http, Reason}};
               {ok, "200", _, []} ->
                   {error, {http, empty}};
               {ok, "200", _, ResponseBody} ->
                   {ok, ResponseBody};
               {ok, HttpCode, _, _} ->
                   {error, {http, list_to_integer(HttpCode)}}
           end,
    case Request of
        #request{reqid = undefined} ->
            case Resp of
                {ok, _}                -> gen_server:reply(From, ok);
                {error, {http, empty}} -> gen_server:reply(From, ok);
                {error, Ibrowse}       -> gen_server:reply(From, {error, Ibrowse})
            end;
        #request{} ->
            case Resp of
                {ok, Body} ->
                    case hello_proto:decode(Request, Body) of
                        #response{result = Result} ->
                            gen_server:reply(From, {ok, Result});
                        Error = #error{} ->
                            gen_server:reply(From, {error, hello_proto:error_resp_to_error_reply(Error)});
                        _ ->
                            gen_server:reply(From, {error, bad_response})
                    end;
                {error, Ibrowse} ->
                    gen_server:reply(From, {error, Ibrowse})
            end;
        #batch_request{} ->
            case Resp of
                {ok, Body} ->
                    case hello_proto:decode(Request, Body) of
                        #batch_response{responses = BatchResps} ->
                            Reply = lists:map(fun (#response{result = Result}) -> {ok, Result};
                                                  (Error = #error{}) -> {error, hello_proto:error_resp_to_error_reply(Error)}
                                              end, BatchResps),
                            gen_server:reply(From, Reply);
                        _ ->
                            gen_server:reply(From, {error, bad_response})
                    end;
                {error, Ibrowse} ->
                    gen_server:reply(From, {error, Ibrowse})
            end
    end.
