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
-export([init_transport/2, send_request/3, terminate_transport/2]).
-export([http_send/4]).

-include("hello.hrl").
-record(http_options, {
    ib_opts :: list({atom(), term()}),
    method = post :: 'put' | 'post'
}).
-record(http_state, {
    url :: string(),
    options :: #http_options{}
}).

%% hello_client callbacks
init_transport(URL, Options) ->
    case validate_options(Options) of
        {ok, ValOpts} ->
            {ok, #http_state{url = ex_uri:encode(URL), options = ValOpts}};
        {error, Reason} ->
            {error, Reason}
    end.

send_request(Request, MimeType, State) when is_binary(Request), is_binary(MimeType) ->
    spawn(?MODULE, http_send, [self(), Request, MimeType, State]),
    {ok, State};
send_request(_, _, State) ->
    {error, no_valid_request, State}.

terminate_transport(_Reason, _State) ->
    ok.

%% http client helpers
http_send(Client, Request, MimeType, State = #http_state{url = URL, options = Options}) ->
    #http_options{method = Method, ib_opts = Opts} = Options,
    {ok, Vsn} = application:get_key(hello, vsn),
    Headers = [{"Content-Type", binary_to_list(MimeType)},
               {"Accept", MimeType},
               {"User-Agent", "hello/" ++ Vsn}],
    case ibrowse:send_req(URL, Headers, Method, Request, Opts) of
        {ok, _HttpCode, _, []} -> %% empty responses are ignored
            ok;
        {ok, "200", _, Body} ->
            outgoing_message(Client, MimeType, Body, State);
        {ok, "201", _, Body} ->
            outgoing_message(Client, MimeType, Body, State);
        {ok, "202", _, Body} ->
            outgoing_message(Client, MimeType, Body, State);
        {ok, HttpCode, _, _Body} ->
            Client ! {?INCOMING_MSG, {error, list_to_integer(HttpCode), State}},
            exit(normal);
        {error, Reason} ->
            Client ! {?INCOMING_MSG, {error, Reason, State}},
            exit(normal)
    end.

outgoing_message(Client, MimeType, Body, State) ->
    Body1 = list_to_binary(Body),
    case MimeType of
        <<"application/json">> ->
            Body2 = binary:replace(Body1, <<"}{">>, <<"}$$${">>, [global]),
            Body3 = binary:replace(Body2, <<"]{">>, <<"]$$${">>, [global]),
            Body4 = binary:replace(Body3, <<"}[">>, <<"}$$$[">>, [global]),
            Bodies = binary:split(Body4, <<"$$$">>, [global]),
            [ Client ! {?INCOMING_MSG, {ok, SingleBody, State}} || SingleBody <- Bodies ];
        _NoJson ->
            Client ! {?INCOMING_MSG, {ok, Body1, State}}
    end.

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

