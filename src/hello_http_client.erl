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
http_send(Request = #request{reqid = ReqId}, From, URL, #http_options{method = Method, ib_opts = Opts}) ->
    RequestJSON = hello_proto:encode(Request),
    {ok, Vsn} = application:get_key(hello, vsn),
    Headers = [{"Content-Type", "application/json"},
               {"Accept", "application/json"},
               {"User-Agent", "hello/" ++ Vsn}],
    Resp = case ibrowse:send_req(URL, Headers, Method, RequestJSON, Opts) of
               {error, Reason} ->
                   {error, {http, Reason}};
               {ok, "200", _, []} ->
                   {error, {http, empty}};
               {ok, "200", _, ResponseBody} ->
                   {ok, ResponseBody};
               {ok, HttpCode, _, _} ->
                   {error, {http, list_to_integer(HttpCode)}}
           end,
    case ReqId of
        undefined ->
            case Resp of
                {ok, _}                -> gen_server:reply(From, ok);
                {error, {http, empty}} -> gen_server:reply(From, ok);
                {error, Ibrowse}       -> gen_server:reply(From, {error, Ibrowse})
            end;
        _ ->
            case Resp of
                {ok, Body} ->
                    case hello_proto:decode(Request, Body) of
                        #response{result = Result} ->
                            gen_server:reply(From, {ok, Result});
                        ResponseRec = #error{} -> gen_server:reply(From, {error, hello_proto:error_resp_to_error_reply(ResponseRec)});
                        {error, Error} ->
                            gen_server:reply(From, {error, Error})
                    end;
                {error, Ibrowse} ->
                    gen_server:reply(From, {error, Ibrowse})
            end
    end.
