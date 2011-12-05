% Copyright 2010-2011, Travelping GmbH <info@travelping.com>

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

%% @doc This module contains a simple JSON-RPC client.

-module(hello_client).
-export([start/3, start_link/3, start/4, start_link/4, stop/1]).
-export([start_connection/3, start_connection/4, stop_connection/1]).
-export([call/3, notification/3, call_np/3]).
%% helper
-export([validate_ctx/3]).
-export_type([url/0, method/0, rpc_error/0]).

-include("internal.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-define(TIMEOUT, {timeout, 15000}).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type context() :: pid() | atom().
-type url() :: string().
-type method() :: 'put' | 'post' | 'req' | 'dealer' | 'router'.
-type connection_protocol() :: 'http' | 'zmq'.
-type rpc_error() :: {error, {http, term()}} | {error, syntax_error}
                     | {error, invalid_request} | {error, method_not_found} | {error, invalid_params}
                     | {error, internal_error} | {error, internal_error} | {error, integer()}.

%% --------------------------------------------------------------------------------
%% -- Client API

%% @doc create and link a connection context
start_link(URI, Method, Opts) ->
    gen_server:start_link(?MODULE, {URI, Method, Opts}, []).

%% @doc create a connection context
start(URI, Method, Opts) ->
    gen_server:start(?MODULE, {URI, Method, Opts}, []).

%% @doc create and link a named connection context
start_link(Name, URI, Method, Opts) ->
    gen_server:start_link(Name, ?MODULE, {URI, Method, Opts}, []).

%% @doc create a named connection context
start(Name, URI, Method, Opts) ->
    gen_server:start(Name, ?MODULE, {URI, Method, Opts}, []).

stop(Name) when is_atom(Name); is_pid(Name) ->
	gen_server:call(Name, terminate).

%% @doc create a supervised connection context
%%
%% This method is used to create context as part of a supervisor tree.
%% Terminated contexts will not be restarted
%%
start_connection(URI, Method, Opts) ->
	hello_client_sup:start_connection(URI, Method, Opts).

%% @doc create a supervised named connection context
%%
%% This method is used to create context as part of a supervisor tree.
%% Terminated contexts will not be restarted
%%
start_connection(Name, URI, Method, Opts) ->
	hello_client_sup:start_connection(Name, URI, Method, Opts).

%% @doc destroy a supervised named connection context
stop_connection(Name) ->
	hello_client_sup:start_connection(Name).

%% @doc Perform a JSON-RPC method call.
-spec call(context(), string(), [hello_json:value()]) -> {ok, hello_json:value()} | {error, rpc_error()}.
call(Ctx, Method, ArgList) when is_list(ArgList) or is_tuple(ArgList) ->
	gen_server:call(Ctx, {call, Method, ArgList}).

%% @doc Performs a JSON-RPC method call with named parameters (property list).
-spec call_np(context(), string(), [{string(), hello_json:value()}]) -> {ok, hello_json:value()} | {error, rpc_error()}.
call_np(Ctx, Method, ArgProps) when is_list(ArgProps) ->
    gen_server:call(Ctx, {call , Method, {ArgProps}}).

%% @doc Special form of a JSON-RPC method call that returns no result.
-spec notification(context(), string(), [hello_json:value()]) -> ok | {error, rpc_error()}.
notification(Ctx, Method, ArgList) ->
	gen_server:call(Ctx, {notification, Method, ArgList}).
 
%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(hello_client_ctx, {
		  uri         :: string(),
		  method      :: method(),
		  opts = []   :: list(term()),

		  protocol    :: connection_protocol(),
		  next_req_id :: integer(),
		  pending     :: gb_trees:gb_trees(),
		  context                           %% local storage for protocol dependent data
		 }).

-record(zmq_ctx, {
		  uri :: string(),
		  context,
		  socket,

		  pending = [] :: list()
}).

init({URI, Method, Opts}) ->
	Ctx0 = #hello_client_ctx{uri = URI,
							 method = Method,
							 opts = Opts,
							 next_req_id = 1,
							 pending = gb_trees:empty()},
	Ctx1 = init_connection_protocol(Ctx0),
	Ctx2 = init_context(Ctx1),
	{ok, Ctx2}.

handle_call({call, Method, ArgList}, From, Ctx) ->
    Request = #request{id = Ctx#hello_client_ctx.next_req_id, method = Method, params = ArgList},
	NewCtx = inc_req_id(Ctx),
	rpc_request(Request, From, NewCtx);
handle_call({notification, Method, ArgList}, From, Ctx) ->
	Request = #request{id = undefined, method = Method, params = ArgList},
    rpc_request(Request, From, Ctx);
handle_call(terminate, _From, Ctx) ->
	{stop, normal, ok, Ctx}.

terminate(_Reason, Ctx) ->
	terminate_context(Ctx).

handle_info({zmq, _Socket, Msg, [rcvmore]}, Ctx = #hello_client_ctx{context = ZmqCtx}) ->
	NewCtx = Ctx#hello_client_ctx{context = ZmqCtx#zmq_ctx{pending = [Msg|ZmqCtx#zmq_ctx.pending]}},
    {noreply, NewCtx};

handle_info({zmq, _Socket, Msg, []}, Ctx0 = #hello_client_ctx{context = ZmqCtx}) ->
	Ctx1 = handle_zmq_reply(lists:reverse([Msg|ZmqCtx#zmq_ctx.pending]), Ctx0),
	Ctx2 = Ctx1#hello_client_ctx{context = ZmqCtx#zmq_ctx{pending = []}},
    {noreply, Ctx2};

handle_info({http, ReqId, Resp}, Ctx) ->
	NewCtx = case Resp of
				 {error, _} ->
					 reply_pending_req(ReqId, Resp, Ctx);
				 {ok, Body} ->
					 http_reply(Body, Ctx)
			 end,
	{noreply, NewCtx}.

%% unused callbacks
handle_cast(_Cast, State) ->
    {noreply, State}.
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

%% --------------------------------------------------------------------------------
%% -- Helper functions
validate_ctx(URI, Method, Opts) ->
	Ctx0 = #hello_client_ctx{uri = URI,
							 method = Method,
							 opts = Opts},
	case catch init_connection_protocol(Ctx0) of
		#hello_client_ctx{protocol = Protocol, method = Method1} ->
			validate_method(Protocol, Method1, Opts);
		{'EXIT', {Repl, _}} ->
			Repl;
		Error -> io:format("Error: ~w~n", [Error]),
				 Error
	end.

validate_method(http, Method, Opts)
  when Method == put; Method == post ->
	validate_opts(http, Method, Opts);
validate_method(zmq, Method, Opts)
  when Method == req; Method == dealer; Method == router ->
	validate_opts(zmq, Method, Opts);
validate_method(_, _, _) ->
	invalid_method.
	
validate_opts(http, Method, Opts) ->
	Permited = [max_sessions, ssl_options, proxy_host, proxy_port, proxy_user, proxy_password,
				use_absolute_uri, basic_auth, cookie, http_vsn, inactivity_timeout, connect_timeout],
	permited_keys(Permited, http, Method, Opts, fun validate_http/3);
validate_opts(zmq, Method, Opts) ->
	Permited = [],
	permited_keys(Permited, zmq, Method, Opts, fun validate_zmq/3).

permited_keys(Permited, Protocol, Method, Opts, NextFun) ->
	Keys = proplists:get_keys(Opts),
	case lists:subtract(Keys, Permited) of
		[] -> NextFun(Protocol, Method, proplists:unfold(Opts));
		Excess -> {invalid_opts, Excess}
	end.

%% TODO: additional validation
validate_http(_Protocol, _Method, []) ->
	ok;
validate_http(Protocol, Method, [_|Rest]) ->
	validate_http(Protocol, Method, Rest).

validate_zmq(_Protocol, _Method, []) ->
	ok;
validate_zmq(Protocol, Method, [_|Rest]) ->
	validate_zmq(Protocol, Method, Rest).

init_connection_protocol(Ctx = #hello_client_ctx{uri = URI}) ->
    case (catch ex_uri:decode(URI)) of
        {ok, Rec = #ex_uri{}, _} ->
            init_request_scheme(Rec, Ctx);
        _Other ->
            error(badurl)
    end.

inc_req_id(Ctx = #hello_client_ctx{next_req_id = Id}) ->
	Ctx#hello_client_ctx{next_req_id = Id + 1}.

init_request_scheme(#ex_uri{scheme = "http"}, Ctx = #hello_client_ctx{}) ->
	Ctx#hello_client_ctx{protocol = http};
init_request_scheme(#ex_uri{scheme = "https"}, Ctx = #hello_client_ctx{opts = Opts}) ->
	Ctx#hello_client_ctx{protocol = http, opts = [{is_ssl, true}|Opts]};
init_request_scheme(URI = #ex_uri{scheme = "zmq-tcp"}, Ctx = #hello_client_ctx{}) ->
	Uri = ex_uri:encode(URI#ex_uri{scheme = "tcp"}),
	Ctx#hello_client_ctx{protocol = zmq, context = #zmq_ctx{uri = Uri}};
init_request_scheme(URI = #ex_uri{scheme = "zmq-ipc"}, Ctx = #hello_client_ctx{}) ->
	Uri = ex_uri:encode(URI#ex_uri{scheme = "ipc"}),
	Ctx#hello_client_ctx{protocol = zmq, context = #zmq_ctx{uri = Uri}};
init_request_scheme(_, _) ->
	error(badscheme).

init_context(Ctx = #hello_client_ctx{protocol = http}) ->
	Ctx#hello_client_ctx{opts = [{socket_options, [{reuseaddr, true}]}|Ctx#hello_client_ctx.opts]};
init_context(Ctx = #hello_client_ctx{protocol = zmq, method = Method, context = ZmqCtx}) ->
    {ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, [Method, {active, true}]),
	ok = erlzmq:connect(Socket, ZmqCtx#zmq_ctx.uri),
	Ctx#hello_client_ctx{context = ZmqCtx#zmq_ctx{context = Context, socket = Socket}}.

terminate_context(#hello_client_ctx{protocol = http}) ->
	ok;
terminate_context(#hello_client_ctx{protocol = zmq, context = ZmqCtx}) ->
    erlzmq:close(ZmqCtx#zmq_ctx.socket),
    erlzmq:term(ZmqCtx#zmq_ctx.context).

rpc_request(Request = #request{id = ReqId}, From, Ctx = #hello_client_ctx{protocol = zmq, method = Method, context = #zmq_ctx{socket = Socket}}) ->
    RequestJSON = encode_request(Request),
	zmq_send(Socket, Method, RequestJSON),
    case ReqId of
        undefined -> {reply, ok, Ctx};
        _         -> NewCtx = add_pending_req(ReqId, From, Ctx),
					 {noreply, NewCtx}
    end;

rpc_request(Request = #request{id = ReqId}, From, Ctx = #hello_client_ctx{protocol = http, uri = URI, method = Method, opts = Opts}) ->
    RequestJSON = encode_request(Request),
    {ok, Vsn}   = application:get_key(hello, vsn),
    Headers     = [{"Content-Type", "application/json"},
                   {"Accept", "application/json"},
                   {"User-Agent", "hello/" ++ Vsn}],
	Master = self(),
	spawn(fun() ->
				  Resp = case ibrowse:send_req(URI, Headers, Method, RequestJSON, Opts) of
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
							  {error, _}             -> gen_server:reply(From, Resp)
						  end;
					  _         -> 
						  Master ! {http, ReqId, Resp}
				  end
		  end),
	case ReqId of
		undefined -> {noreply, Ctx};
		_         -> NewCtx = add_pending_req(ReqId, From, Ctx),
					 {noreply, NewCtx}
	end.

zmq_send(Socket, req, RequestJSON) ->
    erlzmq:send(Socket, RequestJSON);
zmq_send(Socket, dealer, RequestJSON) ->
    erlzmq:send(Socket, <<>>, [sndmore]),
    erlzmq:send(Socket, RequestJSON).

%% TODO: filter zMQ message...
handle_zmq_reply(Msg, Ctx = #hello_client_ctx{method = req}) ->
	io:format("zmq req reply: ~p~n", [Msg]),
	zmq_reply(Msg, Ctx);
handle_zmq_reply(Msg, Ctx = #hello_client_ctx{method = dealer}) ->
	io:format("zmq dealer reply: ~p~n", [Msg]),
	zmq_reply(Msg, Ctx);
handle_zmq_reply(Msg, Ctx = #hello_client_ctx{method = router}) ->
	io:format("zmq router reply: ~p~n", [Msg]),
	zmq_reply(Msg, Ctx).

zmq_reply(Body, Ctx) ->
	case decode_json(Body) of
		{ok, ReqId, Resp} ->
			reply_pending_req(ReqId, {ok, Resp}, Ctx);
		{error, ReqId, Reason} ->
			reply_pending_req(ReqId, {error, Reason}, Ctx);
		_ ->
			Ctx
	end.

http_reply(Body, Ctx) ->
	case decode_json(Body) of
		{ok, ReqId, Resp} ->
			reply_pending_req(ReqId, {ok, Resp}, Ctx);
		{error, ReqId, Reason} ->
			reply_pending_req(ReqId, {error, Reason}, Ctx);
		_ -> Ctx
	end.
	
add_pending_req(ReqId, From, Ctx = #hello_client_ctx{pending = Pending}) ->
	Ctx#hello_client_ctx{pending = gb_trees:enter(ReqId, From, Pending)}.

reply_pending_req(ReqId0, Resp, Ctx = #hello_client_ctx{pending = Pending}) ->
	ReqId = binstr_to_integer(ReqId0),
	case gb_trees:lookup(ReqId, Pending) of
		none ->
			Ctx;
		{value, From} ->
			gen_server:reply(From, Resp),
			Ctx#hello_client_ctx{pending = gb_trees:delete(ReqId, Pending)}
	end.
			
decode_json(Body) ->
	case hello_json:decode(Body) of
		{error, Reason} -> {error, Reason};
		{ok, {Props}, _Rest} ->
			ReqId = proplists:get_value(<<"id">>, Props, null),
			case proplists:get_value(<<"error">>, Props, null) of
				null ->
					Result = proplists:get_value(<<"result">>, Props),
					{ok, ReqId, Result};
				{ErrorObject} ->
					ErrorInfo = case proplists:get_value(<<"code">>, ErrorObject) of
									-32600 -> invalid_request;
									-32601 -> method_not_found;
									-32602 -> invalid_params;
									-32603 -> internal_error;
									Code when (Code >= -32099) and (Code =< -32000) -> server_error;
									Code -> {Code, proplists:get_value(<<"message">>, ErrorObject)}
								end,
					{error, ReqId, ErrorInfo}
			end
	end.

encode_request(#request{id = Id, method = Method, params = ArgList}) ->
    Methodto    = into_bin(Method),
    IDField     = case Id of
                      undefined -> [];
                      _         -> [{"id", Id}]
                  end,
    hello_json:encode({IDField ++ [{"jsonrpc", <<"2.0">>}, {"method", Methodto}, {"params", ArgList}]}).

into_bin(Bin) when is_list(Bin) -> list_to_binary(Bin);
into_bin(Bin)                   -> Bin.

binstr_to_integer(Bin) when is_integer(Bin) ->
	Bin;
binstr_to_integer(Bin) when is_binary(Bin) ->
	list_to_integer(binary_to_list(Bin)).
