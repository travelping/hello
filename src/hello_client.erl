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
-export([start/2, start_link/2, start/3, start_link/3, stop/1]).
-export([start_connection/2, start_connection/3, stop_connection/1]).
-export([call/3, notification/3, call_np/3]).

-export([behaviour_info/1]).

-export_type([url/0, rpc_error/0]).

-include("internal.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-define(TIMEOUT, {timeout, 15000}).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type context() :: pid() | atom().
-type url() :: string().
-type rpc_error() :: {error, {http, term()}} | {error, syntax_error}
                     | {error, invalid_request} | {error, method_not_found} | {error, invalid_params}
                     | {error, internal_error} | {error, internal_error} | {error, integer()}.

behaviour_info(callbacks) ->
    [{validate_options,1}, {init,2}, {send_request,3}, {handle_info,2}, {terminate,2}];
behaviour_info(_) ->
    undefined.

%% --------------------------------------------------------------------------------
%% -- Client API
%% @doc create and link a connection context
start_link(URI, Opts) ->
    gen_server:start_link(?MODULE, {URI, Opts}, []).

%% @doc create a connection context
start(URI, Opts) ->
    gen_server:start(?MODULE, {URI, Opts}, []).

%% @doc create and link a named connection context
start_link(Name, URI, Opts) ->
    gen_server:start_link(Name, ?MODULE, {URI, Opts}, []).

%% @doc create a named connection context
start(Name, URI, Opts) ->
    gen_server:start(Name, ?MODULE, {URI, Opts}, []).

stop(Name) when is_atom(Name); is_pid(Name) ->
    gen_server:call(Name, terminate).

%% @doc create a supervised connection context
%%
%% This method is used to create context as part of a supervisor tree.
%% Terminated contexts will not be restarted
%%
start_connection(URI, Opts) ->
    hello_client_sup:start_connection(URI, Opts).

%% @doc create a supervised named connection context
%%
%% This method is used to create context as part of a supervisor tree.
%% Terminated contexts will not be restarted
%%
start_connection(Name, URI, Opts) ->
    hello_client_sup:start_connection(Name, URI, Opts).

%% @doc destroy a supervised named connection context
stop_connection(Name) ->
    hello_client_sup:stop_connection(Name).

%% @doc Perform a JSON-RPC method call.
-spec call(context(), string(), [hello_json:value()]) -> {ok, hello_json:value()} | {error, rpc_error()}.
call(Ctx, Method, ArgList) when is_list(ArgList) or is_tuple(ArgList) ->
    gen_server:call(Ctx, {call, Method, ArgList}).

%% @doc Performs a JSON-RPC method call with named parameters (property list).
-spec call_np(context(), string(), [{string(), hello_json:value()}]) -> {ok, hello_json:value()} | {error, rpc_error()}.
call_np(Ctx, Method, ArgProps) when is_list(ArgProps) ->
    gen_server:call(Ctx, {call, Method, {ArgProps}}).

%% @doc Special form of a JSON-RPC method call that returns no result.
-spec notification(context(), string(), [hello_json:value()]) -> ok | {error, rpc_error()}.
notification(Ctx, Method, ArgList) ->
    gen_server:call(Ctx, {notification, Method, ArgList}).

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(client_state, {
    mod :: module(),
    mod_state :: term(),
    next_reqid = 0 :: pos_integer()
}).

init({URI, Opts}) ->
    case (catch ex_uri:decode(URI)) of
        {ok, URIRec = #ex_uri{}, _} ->
            case uri_client_module(URIRec) of
                {NewURIRec, Module} ->
                    case Module:validate_options(Opts) of
                        {ok, OptionData} ->
                            {ok, InitialModState} = Module:init(NewURIRec, OptionData),
                            State = #client_state{mod = Module, mod_state = InitialModState},
                            {ok, State};
                        {error, Error} ->
                            {stop, {invalid_options, Error}}
                    end;
                badscheme ->
                    {stop, badscheme}
            end;
        _Other ->
            {stop, badurl}
    end.

handle_call({call, Method, ArgList}, From, State = #client_state{next_reqid = ReqId, mod = Module, mod_state = ModState}) ->
    Request = hello_proto:new_request(hello_proto_jsonrpc, ReqId, Method, ArgList),
    handle_reply_result(ReqId + 1, State, Module:send_request(Request, From, ModState));
handle_call({notification, Method, ArgList}, From, State = #client_state{next_reqid = ReqId, mod = Module, mod_state = ModState}) ->
    Request = hello_proto:new_request(hello_proto_jsonrpc, undefined, Method, ArgList),
    handle_reply_result(ReqId, State, Module:send_request(Request, From, ModState));
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_info(Info, State = #client_state{mod = Module, mod_state = ModState}) ->
    case Module:handle_info(Info, ModState) of
        {noreply, NewModState} ->
            {noreply, State#client_state{mod_state = NewModState}};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#client_state{mod_state = NewModState}}
    end.

terminate(Reason, #client_state{mod = Module, mod_state = ModState}) ->
    Module:terminate(Reason, ModState).

%% unused callbacks
handle_cast(_Cast, State) ->
    {noreply, State}.
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

%% --------------------------------------------------------------------------------
%% -- Helper functions
uri_client_module(URI = #ex_uri{scheme = "http"})    -> {URI, hello_http_client};
uri_client_module(URI = #ex_uri{scheme = "https"})   -> {URI, hello_http_client};
uri_client_module(URI = #ex_uri{scheme = "zmq-tcp"}) -> {URI#ex_uri{scheme = "tcp"}, hello_zmq_client};
uri_client_module(URI = #ex_uri{scheme = "zmq-ipc"}) -> {URI#ex_uri{scheme = "ipc"}, hello_zmq_client};
uri_client_module(_) ->
    badscheme.

handle_reply_result(NewReqId, State, {reply, Reply, NewModState}) ->
    {reply, Reply, State#client_state{next_reqid = NewReqId, mod_state = NewModState}};
handle_reply_result(NewReqId, State, {noreply, NewModState}) ->
    {noreply, State#client_state{next_reqid = NewReqId, mod_state = NewModState}};
handle_reply_result(NewReqId, State, {stop, Reason, Reply, NewModState}) ->
    {stop, Reason, Reply, State#client_state{next_reqid = NewReqId, mod_state = NewModState}};
handle_reply_result(NewReqId, State, {stop, Reason, NewModState}) ->
    {stop, Reason, State#client_state{next_reqid = NewReqId, mod_state = NewModState}}.
