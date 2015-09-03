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
-export([init_transport/2, send_request/3, terminate_transport/2, handle_info/2]).
-export([http_send/4]).
-export([gen_meta_fields/1]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("hello.hrl").
-include("hello_log.hrl").
-record(http_options, {
    ib_opts :: list({atom(), term()}),
    method = post :: 'put' | 'post'
}).
-record(http_state, {
    url :: string(),
    path :: string(),
    scheme :: atom(),
    options :: #http_options{}
}).

%% hello_client callbacks
init_transport(URL, Options) ->
    case validate_options(Options) of
        {ok, ValOpts} ->
            http_connect_url(URL),
            {ok, #http_state{url = ex_uri:encode(URL), scheme = URL#ex_uri.scheme, path = URL#ex_uri.path, options = ValOpts}};
        {error, Reason} ->
            ?LOG_INFO("Hello http client invoked with invalid options. Terminated with reason '~p'.", [Reason],
                        [{hello_error_reason, {error, Reason, Options}}], ?LOGID39),
            {error, Reason}
    end.

send_request(Request, Signarute, State) when is_binary(Request), is_binary(Signarute) ->
    spawn(?MODULE, http_send, [self(), Request, Signarute, State]),
    {ok, State};
send_request(_, _, State) ->
    {error, no_valid_request, State}.

terminate_transport(_Reason, _State) ->
    ok.

handle_info({dnssd, _Ref, {resolve,{Host, Port, _Txt}}}, State = #http_state{scheme = Scheme, path = Path}) ->
    ?LOG_INFO("Hello http client: DNS discovery service resolved path '~p' to host '~p:~w'.", [Path, Host, Port],
                gen_meta_fields(State), ?LOGID40),
    {noreply, State#http_state{url = build_url(Scheme, Host, Path, Port)}};
handle_info({dnssd, _Ref, Msg}, State) ->
    ?LOG_INFO("Hello https client received message '~p' from DNS discovery service.", [Msg],
                gen_meta_fields(State), ?LOGID41),
    {noreply, State}.

build_url(Scheme, Host, Path, Port) ->
    ex_uri:encode(#ex_uri{scheme = Scheme,
                          authority = #ex_uri_authority{host = clean_host(Host), port = Port},
                          path = Path}).

clean_host(Host) ->
    HostSize = erlang:byte_size(Host),
    CleanedHost = case binary:match(Host, <<".local.">>) of
        {M, L} when HostSize == (M + L) ->
            <<HostCuted:M/binary, _/binary>> = Host,
            HostCuted;
        _ ->
            Host
    end,
    binary_to_list(CleanedHost).

content_type(Signarute) ->
    Json = hello_json:signature(),
    MsgPack = hello_msgpack:signature(),
    case Signarute of
        Json -> <<"application/json">>;
        MsgPack -> <<"application/x-msgpack">>;
        _ -> <<"application/octet-stream">>
    end.

%% http client helpers
http_send(Client, Request, Signarute, State = #http_state{url = URL, options = Options}) ->
    #http_options{method = Method, ib_opts = Opts} = Options,
    {ok, Vsn} = application:get_key(hello, vsn),
    Headers = [{<<"content-type">>, content_type(Signarute)},
               {<<"accept">>, content_type(Signarute)},
               {<<"user-agent">>, <<"hello/", (list_to_binary(Vsn))/binary>>}],
    case hackney:Method(URL, Headers, Request, Opts) of
        {ok, Success, RespHeaders, ClientRef} when Success =:= 200; Success =:= 201; Success =:= 202 ->
            {ok, Body} = hackney:body(ClientRef),
            Signature1 = hackney_headers:get_value(<<"content-type">>, hackney_headers:new(RespHeaders), <<"undefined">>),
            outgoing_message(Client, Signature1, Body, State);
        {ok, HttpCode, _, _} ->
            Client ! {?INCOMING_MSG, {error, HttpCode, State}},
            exit(normal);
        {error, Reason} ->
            ?LOG_INFO("Hello http client received an error after executing a request to '~p' with reason '~p'.", [URL, Reason],
                        lists:append(gen_meta_fields(State), [{hello_error_reason, {{request, Request}, {error, Reason}}}]), ?LOGID42),
            Client ! {?INCOMING_MSG, {error, Reason, State}},
            exit(normal)
    end.

outgoing_message(Client, Signarute, Body, State) ->
    Json = hello_json:signature(),
    Signarute1 = hello_http_listener:signature(Signarute),
    case Signarute1 of
        Json ->
            Body1 = binary:replace(Body, <<"}{">>, <<"}$$${">>, [global]),
            Body2 = binary:replace(Body1, <<"]{">>, <<"]$$${">>, [global]),
            Body3 = binary:replace(Body2, <<"}[">>, <<"}$$$[">>, [global]),
            Bodies = binary:split(Body3, <<"$$$">>, [global]),
            [ Client ! {?INCOMING_MSG, {ok, Signarute1, SingleBody, State}} || SingleBody <- Bodies ];
        _NoJson ->
            Client ! {?INCOMING_MSG, {ok, Signarute1, Body, State}}
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

http_connect_url(#ex_uri{authority = #ex_uri_authority{host = Host}, path = [$/|Path]}) ->
    dnssd:resolve(list_to_binary(Path), <<"_", (list_to_binary(Host))/binary, "._tcp.">>, <<"local.">>),
    ok;
http_connect_url(URI) ->
    URI.

gen_meta_fields(#http_state{url = URL, path = Path}) ->
    [{hello_transport, http}, {hello_transport_url, URL}, {hello_transport_path, Path}].
