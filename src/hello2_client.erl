% Copyright 2010-2012, Travelping GmbH <info@travelping.com>

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

%% @doc This module contains an RPC client.
-module(hello2_client).
%% public API
-export([start/2, start_link/2, start/3, start_link/3, stop/1, validate_options/2,
         start_supervised/2, start_supervised/3, stop_supervised/1,
         call/3, call/4, call_np/3, call_np/4, batch_call/2, batch_call/3,
         notify/3, notify/4, notify_np/3, notify_np/4]).
%% deprecated
-export([notification/3]).

%% client transport implementation API
-export([behaviour_info/1]).
-export([client_ctx_reply/2, client_ctx_notify/2]).
%% internal
-export([run_notification_sink_function/4]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export_type([client/0, method/0, options/0, error_code/0, rpc_error/0]).
-export_type([server_name/0]).

-include("internal.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-define(DEFAULT_TIMEOUT, 10000).

-type client() :: pid() | atom().

-type method() :: atom() | string() | binary().
-type notification_sink() :: undefined | pid() | atom()
                           | fun((pid(), Method::binary(), hello2_json:json_array() | hello2_json:json_object()) -> any()).
-type generic_options() :: {protocol, module()} | {notification_sink, notification_sink()}.
-type options() :: [generic_options() | {atom(), any()}].
-type start_error() :: {invalid_options, any()} | badscheme | badurl.
-type standard_error() :: parse_error | invalid_request | method_not_found | invalid_params | internal_error | server_error.
-type error_code() :: standard_error() | integer().
-type rpc_error() :: timeout | {transport, term()} | {error_code(), binary()}.

%% @private
behaviour_info(callbacks) ->
    [{validate_options,1}, {init,2}, {send_request,3}, {handle_info,3}, {terminate,3}];
behaviour_info(_) ->
    undefined.

%% --------------------------------------------------------------------------------
%% -- Client API
%% @doc create and link a client process.
%%   The ``Options'' argument, a property list, configures the client. The list of
%%   valid options depends on the transport that is used. The following options are supported
%%   for all listeners:
%%   <ul style="list-style-type: none;">
%%     <li>
%%       <b>{protocol, module()}</b><br/>
%%       Selects the RPC protocol implementation. The default is <tt>hello2_proto_jsonrpc</tt>.<br/><br/>
%%     </li>
%%     <li>
%%       <b>{notification_sink, function() | pid() | atom()}</b><br/>
%%       This option configures the handling of RPC notifications sent by the server.<br/>
%%       <ul>
%%         <li>
%%           If a function is given, it will be called with 3 arguments for each notification.
%%           The first argument is the pid of the client process, the second argument is the notification's
%%           RPC method name (as a binary) and the third is the argument list. For protocols that
%%           support named parameters, the third parameter can also be a value of type hello2_json:json_object().
%%         </li>
%%         <li>
%%           If the notification sink is the atom <tt>undefined</tt>, notifications are dropped.<br/>
%%           This is the default behaviour.
%%         </li>
%%         <li>
%%           If the notification sink is a pid or any atom except <tt>undefined</tt>, notifications will be sent
%%           as an Erlang message to the process with the given pid or to a named process with the given name.
%%           Notification messages obey the following format:
%%           <pre>{rpc_notification, Client::pid(), Method::binary(), Parameters}</pre>
%%           As with notification sink functions, the Parameters are either sent as a list
%%           or as a <tt>hello2_json:json_object()</tt>.
%%         </li>
%%       </ul>
%%     </li>
%%   </ul>
-spec start_link(hello2:url(), options()) -> {ok, pid()} | {error, start_error()}.
start_link(URI, Options) ->
    gen_server:start_link(?MODULE, {URI, Options}, []).

%% @doc create a client process
%%   For a description of the parameters, see {@link start_link/2}.
-spec start(hello2:url(), options()) -> {ok, pid()} | {error, start_error()}.
start(URI, Options) ->
    gen_server:start(?MODULE, {URI, Options}, []).

%% A registered server name as in gen_server.
-type server_name() :: {local, atom()} | {global, atom()}.

%% @doc create and link a named client client
%%   For a description of the parameters, see {@link start_link/2}.
-spec start_link(server_name(), hello2:url(), options()) -> {ok, pid()} | {error, start_error()}.
start_link(Name, URI, Options) ->
    gen_server:start_link(Name, ?MODULE, {URI, Options}, []).

%% @doc create a named client
%%   For a description of the parameters, see {@link start_link/2}.
-spec start(server_name(), hello2:url(), options()) -> {ok, pid()} | {error, start_error()}.
start(Name, URI, Options) ->
    gen_server:start(Name, ?MODULE, {URI, Options}, []).

% @doc stop a running client
-spec stop(client()) -> ok.
stop(Client) ->
    gen_server:call(Client, terminate).

%% @doc create client that is supervised by hello
%%   For a description of the parameters, see {@link start_link/2}.
-spec start_supervised(hello2:url(), options()) -> {ok, pid()} | {error, start_error()}.
start_supervised(URI, Options) ->
    hello2_client_sup:start_client(URI, Options).

%% @doc create a named client that is supervised by hello
%%   This function is used to create a client as part of the hello supervisor tree.
%%   It is intended to be used by applications that do not have supervisor trees
%%   on their own, and for scripts.
%%   For a description of the parameters, see {@link start_link/2}.
-spec start_supervised(atom(), hello2:url(), options()) -> {ok, pid()} | {error, start_error()}.
start_supervised(Name, URI, Options) ->
    hello2_client_sup:start_named_client(Name, URI, Options).

%% @doc terminate a client process that is supervised by hello
-spec stop_supervised(client()) -> ok.
stop_supervised(Client) ->
    hello2_client_sup:stop_client(Client).

%% @doc Perform an RPC method call with positional parameters
-spec call(client(), method(), [hello2_json:value()]) -> {ok, hello2_json:value()} | {error, rpc_error()}.
call(Client, Method, ArgList) when is_list(ArgList) ->
    timeout_call(Client, {call, Method, ArgList}, ?DEFAULT_TIMEOUT).

%% @doc Like {@link call/3}, but with a configurable timeout
-spec call(client(), method(), [hello2_json:value()], timeout()) -> {ok, hello2_json:value()} | {error, rpc_error()}.
call(Client, Method, ArgList, Timeout) when is_list(ArgList) ->
    timeout_call(Client, {call, Method, ArgList}, Timeout).

%% @doc Perform an RPC method call with named parameters
-spec call_np(client(), method(), [{string(), hello2_json:value()}]) -> {ok, hello2_json:value()} | {error, rpc_error()}.
call_np(Client, Method, ArgProps) when is_list(ArgProps) ->
    timeout_call(Client, {call, Method, {ArgProps}}, ?DEFAULT_TIMEOUT).

%% @doc Like {@link call_np/3}, but with a configurable timeout
-spec call_np(client(), method(), [{string(), hello2_json:value()}], timeout()) -> {ok, hello2_json:value()} | {error, rpc_error()}.
call_np(Client, Method, ArgProps, Timeout) when is_list(ArgProps) ->
    timeout_call(Client, {call, Method, {ArgProps}}, Timeout).

%% @deprecated
%% @doc Send an RPC notification
%%   This function is deprecated and will be removed in hello 0.3. Use {@link notify/3} instead.
-spec notification(client(), method(), [hello2_json:value()]) -> ok | {error, rpc_error()}.
notification(Client, Method, ArgList) ->
    notify(Client, Method, ArgList).

%% @doc Send an RPC notification with positional parameters
-spec notify(client(), method(), [hello2_json:value()]) -> ok | {error, rpc_error()}.
notify(Client, Method, Parameters) ->
    timeout_call(Client, {notification, Method, Parameters}, ?DEFAULT_TIMEOUT).

%% @doc Like {@link notify/3}, but with a configurable timeout
-spec notify(client(), method(), [hello2_json:value()], timeout()) -> ok | {error, rpc_error()}.
notify(Client, Method, Parameters, Timeout) ->
    timeout_call(Client, {notification, Method, Parameters}, Timeout).

%% @doc Send an RPC notification with named parameters
-spec notify_np(client(), method(), [{string(), hello2_json:value()}]) -> ok | {error, rpc_error()}.
notify_np(Client, Method, Parameters) ->
    timeout_call(Client, {notification, Method, {Parameters}}, ?DEFAULT_TIMEOUT).

%% @doc Like {@link notify_np/3}, but with a configurable timeout
-spec notify_np(client(), method(), [{string(), hello2_json:value()}], timeout()) -> ok | {error, rpc_error()}.
notify_np(Client, Method, Parameters, Timeout) ->
    timeout_call(Client, {notification, Method, {Parameters}}, Timeout).

%% @doc Send multiple RPC calls in one request.
%%   The order of the results in the returned list matches the order of the
%%   calls in the given Batch.
-spec batch_call(client(), [Call]) -> timeout | [{ok, hello2_json:value()} | {error, rpc_error()}] when
    Call :: {method(), hello2_json:json_object() | hello2_json:json_array()}.
batch_call(Client, Batch) ->
    timeout_call(Client, {batch_call, Batch}, ?DEFAULT_TIMEOUT).

%% @doc Like {@link batch_call/2}, but with a configurable timeout
-spec batch_call(client(), [Call], timeout()) -> {error, timeout} | [{ok, hello2_json:value()} | {error, rpc_error()}] when
    Call :: {method(), hello2_json:json_object() | hello2_json:json_array()}.
batch_call(Client, Batch, Timeout) ->
    timeout_call(Client, {batch_call, Batch}, Timeout).

timeout_call(Client, Call, infinity) ->
    gen_server:call(Client, Call, infinity);
timeout_call(Client, Call, Timeout) ->
    try
        gen_server:call(Client, Call, Timeout)
    catch
        exit:{timeout, {gen_server, call, _}} ->
            {error, timeout}
    end.

%% @doc validate the options for a given client URL
-spec validate_options(hello2:url(), list()) -> ok | {error, string()}.
validate_options(URL, Options) ->
    case (catch ex_uri:decode(URL)) of
        {ok, URIRec = #ex_uri{}, _} ->
            case uri_client_module(URIRec) of
                {_NewURIRec, Module} ->
                    do_validate_options(Module, Options);
                badscheme ->
                    {error, "unknown URL scheme"}
            end;
        _Other ->
            {error, "malformed URL"}
    end.

%% ----------------------------------------------------------------------------------------------------
%% -- client transport API
-record(client_reply_ctx, {
    client :: pid(),
    from   :: {reference(), pid()},
    notification_sink :: function() | pid() | atom()
}).

-record(client_notify_ctx, {
    client :: pid(),
    notification_sink :: function() | pid() | atom()
}).

%% @private
%% @doc transport implementations use this function to deliver a reply back to the caller
client_ctx_reply(#client_reply_ctx{from = From}, Reply) ->
    gen_server:reply(From, Reply).

%% @private
%% @doc transport implementations use this function to deliver incoming notifications
client_ctx_notify(#client_reply_ctx{client = Client, notification_sink = Sink}, Notification) ->
    ctx_notify1(Client, Sink, Notification);
client_ctx_notify(#client_notify_ctx{client = Client, notification_sink = Sink}, Notification) ->
    ctx_notify1(Client, Sink, Notification).

ctx_notify1(_Client, undefined, _Notification) ->
    ok;
ctx_notify1(Client, Pid, #request{method = Method, params = Params}) when is_pid(Pid) orelse is_atom(Pid) ->
    catch (Pid ! {rpc_notification, Client, Method, Params});
ctx_notify1(Client, Function, #request{method = Method, params = Params}) when is_function(Function) ->
    spawn(?MODULE, run_notification_sink_function, [Function, Client, Method, Params]).

%% @hidden
run_notification_sink_function(Function, Client, Method, Params) ->
    Function(Client, Method, Params).

%% ----------------------------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(client_options, {
    protocol = hello2_proto_jsonrpc :: module(),
    notification_sink :: function() | pid() | atom()
}).

-record(client_state, {
    mod :: module(),
    mod_state :: term(),
    options :: #client_options{},
    next_reqid = 0 :: non_neg_integer()
}).

%% @hidden
init({URI, Options}) ->
    case (catch ex_uri:decode(URI)) of
        {ok, URIRec = #ex_uri{}, _} ->
            case uri_client_module(URIRec) of
                {NewURIRec, Module} ->
                    case do_validate_options(Module, Options) of
                        {ok, ClientOptions, ModOptionData} ->
                            {ok, InitialModState} = Module:init(NewURIRec, ModOptionData),
                            State = #client_state{mod = Module,
                                                  options = ClientOptions,
                                                  mod_state = InitialModState},
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

%% @hidden
handle_call({call, Method, ArgList}, From, State = #client_state{options = Opts, next_reqid = ReqId}) ->
    Request   = hello2_proto:new_request(Opts#client_options.protocol, ReqId, Method, ArgList),
    ClientCtx = make_reply_ctx(State, From),
    SendReply = (State#client_state.mod):send_request(ClientCtx, Request, State#client_state.mod_state),
    handle_reply_result(ReqId + 1, State, SendReply);
handle_call({notification, Method, ArgList}, From, State = #client_state{options = Opts, next_reqid = ReqId}) ->
    Request = hello2_proto:new_notification(Opts#client_options.protocol, Method, ArgList),
    ClientCtx = make_reply_ctx(State, From),
    SendReply = (State#client_state.mod):send_request(ClientCtx, Request, State#client_state.mod_state),
    handle_reply_result(ReqId, State, SendReply);
handle_call({batch_call, Batch}, From, State = #client_state{options = Opts, next_reqid = ReqId}) ->
    Protocol = Opts#client_options.protocol,
    {Reqs, NewReqId} = lists:mapfoldl(fun ({Method, Args}, ReqIdAcc) ->
                                              Req = hello2_proto:new_request(Protocol, ReqIdAcc, Method, Args),
                                              {Req, ReqIdAcc + 1}
                                      end, ReqId, Batch),
    Request = hello2_proto:new_batch_request(Protocol, Reqs),
    ClientCtx = make_reply_ctx(State, From),
    SendReply = (State#client_state.mod):send_request(ClientCtx, Request, State#client_state.mod_state),
    handle_reply_result(NewReqId, State, SendReply);

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

%% @hidden
handle_info(Info, State = #client_state{mod = Module, mod_state = ModState}) ->
    case Module:handle_info(make_notify_ctx(State), Info, ModState) of
        {noreply, NewModState} ->
            {noreply, State#client_state{mod_state = NewModState}};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#client_state{mod_state = NewModState}}
    end.

%% @hidden
terminate(Reason, State = #client_state{mod = Module, mod_state = ModState}) ->
    Module:terminate(make_notify_ctx(State), Reason, ModState).

%% @hidden
handle_cast(_Cast, State) ->
    {noreply, State}.
%% @hidden
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

%% --------------------------------------------------------------------------------
%% -- Helper functions
uri_client_module(URI = #ex_uri{scheme = "http"})    -> {URI, hello2_http_client};
uri_client_module(URI = #ex_uri{scheme = "https"})   -> {URI, hello2_http_client};
uri_client_module(URI = #ex_uri{scheme = "zmq-tcp"}) -> {URI#ex_uri{scheme = "tcp"}, hello2_zmq_client};
uri_client_module(URI = #ex_uri{scheme = "zmq-ipc"}) -> {URI#ex_uri{scheme = "ipc"}, hello2_zmq_client};
uri_client_module(_) ->
    badscheme.

make_reply_ctx(#client_state{options = #client_options{notification_sink = Sink}}, From) ->
    #client_reply_ctx{client = self(), from = From, notification_sink = Sink}.

make_notify_ctx(#client_state{options = #client_options{notification_sink = Sink}}) ->
    #client_notify_ctx{client = self(), notification_sink = Sink}.

do_validate_options(Module, OptionList) ->
    case validate_generic_options(OptionList, [], #client_options{}) of
        {ok, GenericOptions, StrippedRest} ->
            case Module:validate_options(StrippedRest) of
                {ok, ValidModOptions} ->
                    {ok, GenericOptions, ValidModOptions};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

validate_generic_options([{protocol, Protocol} | R], Acc, Opts) when is_atom(Protocol) ->
    case code:ensure_loaded(Protocol) of
        {module, ProtocolMod} ->
            validate_generic_options(R, Acc, Opts#client_options{protocol = ProtocolMod});
        {error, _Error} ->
            {error, {protocol, load_module}}
    end;
validate_generic_options([{protocol, _} | _R], _Acc, _Opts) ->
    {error, {protocol, not_atom}};
validate_generic_options([{notification_sink, Fun} | R], Acc, Opts) when is_function(Fun) ->
    validate_generic_options(R, Acc, Opts#client_options{notification_sink = Fun});
validate_generic_options([{notification_sink, Pid} | R], Acc, Opts) when is_pid(Pid) ->
    validate_generic_options(R, Acc, Opts#client_options{notification_sink = Pid});
validate_generic_options([{notification_sink, ProcessName} | R], Acc, Opts) when is_atom(ProcessName) ->
    validate_generic_options(R, Acc, Opts#client_options{notification_sink = ProcessName});
validate_generic_options([{notification_sink, _} | _R], _Acc, _Opts) ->
    {error, {notification_sink, not_function_or_process}};
validate_generic_options([Option = {Key, _Value} | R], Acc, Opts) when is_atom(Key) ->
    validate_generic_options(R, [Option | Acc], Opts);
validate_generic_options([Term | _R], _Acc, _Opts) ->
    {error, {badoption, Term}};
validate_generic_options([], Acc, Opts) ->
    {ok, Opts, Acc}.

handle_reply_result(NewReqId, State, {reply, Reply, NewModState}) ->
    {reply, Reply, State#client_state{next_reqid = NewReqId, mod_state = NewModState}};
handle_reply_result(NewReqId, State, {noreply, NewModState}) ->
    {noreply, State#client_state{next_reqid = NewReqId, mod_state = NewModState}};
handle_reply_result(NewReqId, State, {stop, Reason, Reply, NewModState}) ->
    {stop, Reason, Reply, State#client_state{next_reqid = NewReqId, mod_state = NewModState}};
handle_reply_result(NewReqId, State, {stop, Reason, NewModState}) ->
    {stop, Reason, State#client_state{next_reqid = NewReqId, mod_state = NewModState}}.
