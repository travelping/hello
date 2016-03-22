% Copyright 2010-2014, Travelping GmbH <info@travelping.com>

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

% @doc This module is the main interface to the hello application.
-module(hello).

-behaviour(application).

-export([start/2, stop/1, start/0]).
-export([start_service/2, stop_service/1,
         start_listener/1, start_listener/2, start_listener/5, start_listener/6,
         stop_listener/1, call_service/2, call_service/3]).
-export([bind_handler/3, bind/2, bind/3, bind/7, unbind/2]).

-include("hello.hrl").
-include("hello_log.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").

-type decoded_url() :: #ex_uri{}.

%% --------------------------------------------------------------------------------
%% -- type definitions
-type url()         :: decoded_url() | string() | binary().
-type bind_errors() ::  {error, callback_not_loaded} |
                        {error, callback_already_defined} |
                        {error, typespec_not_loaded} |
                        {error, {badurl, term()}}.

%% --------------------------------------------------------------------------------
%% -- debugging stuff
% @doc Starts the application and all dependencies. For debugging and testing.
start() ->
    application:ensure_all_started(hello).

% @doc Callback for application behaviour.
start(_Type, _StartArgs) ->
    ok = start_dnssd(),
    {ok, Supervisor} = hello_supervisor:start_link(),
    {ok, Supervisor, undefined}.

% @doc Callback for application behaviour.
stop(_StopArgs) ->
    ok.

%% --------------------------------------------------------------------------------
%% -- hello binding API
% @doc Starts a RPC server on the given URL.
%   The transport implementation that is chosen depends on
%   the protocol field of the URL. The available transports are documented
%   below. Hello is capable of binding multiple services to a given URL. Arriving
%   messages are routed according to their method prefix. Therefore it is crucial
%   to spend a unique prefix on every callback module. "CallbackArg" is the initial
%   argument to the callback module and is executed by init/1.
%
%    <table border="1">
%      <thead><tr><td width="140"><b>URL scheme</b></td><td><b>Transport</b></td><td><b>Notes</b></td></tr></thead>
%      <tbody>
%        <tr><td>http://...</td><td>HTTP</td><td>Multiple services can be bound to the same host/port
%                                                combination, as long as the path is different.</td></tr>
%        <tr><td>zmq-tcp[6]://...</td><td>ZeroMQ over TCP</td><td>The port <em>must</em> be specified.</td></tr>
%      </tbody>
%    </table>
%   <br/>
%   The function returns:
%   <ul>
%     <li>``ok'' when the server has been bound successfully</li>
%     <li>``{error, callback_not_loaded}'' when the callback module is not loaded</li>
%     <li>``{error, callback_already_defined}'' when the same module is already bound to the given URL</li>
%     <li>``{error, typespec_not_loaded}'' when the yang typespec cannot be loaded</li>
%   </ul>
%
%   The function will <b>exit</b>:
%   <ul>
%     <li>with reason ``badurl'' when the URL is malformed</li>
%     <li>with reason ``badprotocol'' when the URL uses an unknown scheme</li>
%   </ul>
-spec bind_handler(url(), callback(), term()) -> ok | bind_errors().
bind_handler(URL, CallbackMod, CallbackArg) ->
    bind(URL, [], CallbackMod, hello_handler, [{callback_args, CallbackArg}], hello_proto_jsonrpc, []).

% @doc Generic interface for binding callback modules.
%   This function is independent from protocol or transport specific arguments. It should only be used
%   for developing purposes. A more convenient shortcut is bind_handler/3 which should be capable of everything
%   you want.
% @deprecated
-spec bind(url(), trans_opts(), callback(), handler(), handler_opts(), protocol(), protocol_opts()) -> ok | bind_errors().
bind(URL, TransportOpts, CallbackMod, _HandlerMod, HandlerOpts, Protocol, ProtocolOpts) ->
    start_listener(URL, TransportOpts, Protocol, ProtocolOpts, hello_router),
    bind(URL, CallbackMod, HandlerOpts).

%% API

% @deprecated
start_service(_HandlerMod, _HandlerArgs) ->
    %hello_service:register_link(HandlerMod, HandlerArgs).
    ok.

% @deprecated
stop_service(_HandlerMod) ->
    %hello_service:unregister_link(HandlerMod).
    ok.

-spec start_listener(URL :: url()) -> {ok, listener_ref()} | {error, Reason :: term()}.
start_listener(URL) ->
    start_listener(URL, []).

-spec start_listener(URL :: url(), TransportOpts :: list()) -> {ok, listener_ref()} | {error, Reason :: term()}.
start_listener(URL, TransportOpts) ->
    start_listener(URL, TransportOpts, hello_proto_jsonrpc, [{decoder, hello_msgpack}], hello_router).

-spec start_listener(URL :: url(), TransportOpts :: list(), Protocol :: module(),
                     ProtocolOpts :: list(), RouterMod :: module()) ->
    {ok, listener_ref()} | {error, Reason :: term()}.
start_listener(URL, TransportOpts, Protocol, ProtocolOpts, RouterMod) ->
    start_listener(undefined, URL, TransportOpts, Protocol, ProtocolOpts, RouterMod).
    
-spec start_listener(Name :: term(), URL :: url(), TransportOpts :: list(), Protocol :: module(),
                     ProtocolOpts :: list(), RouterMod :: module()) ->
    {ok, listener_ref()} | {error, Reason :: term()}.
start_listener(Name, URL, TransportOpts, Protocol, ProtocolOpts, RouterMod) ->
    on_ex_uri(URL, fun(ExUriURL) ->
                           hello_listener:start(Name, ExUriURL, TransportOpts, Protocol, ProtocolOpts, RouterMod)
                   end).

-spec stop_listener(URL :: url()) -> ok | {error, Reason :: term()}.
stop_listener(URL) ->
    on_ex_uri(URL, fun(ExUriURL) -> hello_listener:stop(ExUriURL) end).

-spec bind(URL :: url(), HandlerMod :: module()) -> ok | {error, Reason :: term()}.
bind(URL, HandlerMod) ->
    bind(URL, HandlerMod, []).

-spec bind(URL :: url(), HandlerMod :: module(), HandlerArgs :: list()) -> ok | {error, Reason :: term()}.
bind(URL, HandlerMod, HandlerArgs) ->
    on_ex_uri(URL, fun(ExUriURL) ->
                           hello_service:lookup(HandlerMod) == {error, not_found} andalso
                               hello_service:register_link(HandlerMod, HandlerArgs),
                           hello_binding:register_link(ExUriURL, HandlerMod)
                   end).

-spec unbind(URL :: url(), HandlerMod :: module()) -> ok.
unbind(URL, HandlerMod) ->
    on_ex_uri(URL, fun(ExUriURL) ->
                           hello_binding:unregister_link(ExUriURL, HandlerMod),
                           hello_binding:lookup(ExUriURL, HandlerMod:router_key()) == {error, not_found} andalso
                               hello_service:unregister_link(HandlerMod)
                   end).

-spec call_service(Name :: binary(), request() | {Method :: binary(), Args :: list()}) -> term().
call_service(Name, Request) -> call_service(Name, undefined, Request).

-spec call_service(Name :: binary(), Identifier :: term(), request() | {Method :: binary(), Args :: list()}) ->
    term().
call_service(Name, UniqId, Request) -> hello_service:call(Name, UniqId, Request).

%% --------------------------------------------------------------------------------
%% -- Helpers
on_ex_uri(URL, Fun) when is_binary(URL) -> on_ex_uri(binary_to_list(URL), Fun);
on_ex_uri(#ex_uri{} = URL, Fun) -> Fun(URL);
on_ex_uri(URL, Fun) ->
    case (catch ex_uri:decode(URL)) of
        {ok, ExUriURL, _} ->
            Fun(ExUriURL);
        Other ->
            error(badarg, [URL, Other])
    end.

start_dnssd() ->
    Reason = case application:get_env(hello, dnssd) of
        {ok, true} ->
            case application:ensure_all_started(dnssd) of
                {ok, _} -> ok;
                Error -> Error
            end;
        _ -> ok
    end,
    Reason /= ok andalso ?LOG_ERROR("Application dnssd is not started.", [], 
                                    [{hello_dnssd_starting_reason, Reason}], ?LOGID64),
    Reason.
