-module(hello_listener).
-export([start/5, stop/1, all/0, async_incoming_message/3, await_answer/0, handle_incoming_message/3]).
-export([behaviour_info/1]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("internal.hrl").
%% --------------------------------------------------------------------------------
%% -- start and stop a listener
-record(listener, {
    exuri,
    ref,
    protocol,
    protocol_opts,
    router
}).

behaviour_info(callbacks) ->
    [{listener_specification, 2},
     {send_response, 2},
     {close, 1},
     {listener_termination, 1}
     ];
behaviour_info(_) ->
    undefined.

start(ExUriURL, TransportOpts, Protocol, ProtocolOpts, RouterMod) ->
    case hello_registry:lookup({listener, ExUriURL}) of
        {error, not_found} ->
            case start1(ExUriURL, TransportOpts) of
                {ok, ListenerRef} ->
                    ListenerInfo = #listener{ref = ListenerRef,
                                             exuri = ExUriURL,
                                             protocol = Protocol,
                                             protocol_opts = ProtocolOpts,
                                             router = RouterMod},
                    hello_registry:register({listener, ExUriURL}, ListenerInfo),
                    {ok, ListenerRef};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, ListenerRef} ->
            {ok, ListenerRef}
    end.

stop(ExUriURL = #ex_uri{scheme = Scheme}) ->
    TransportMod = transport_module(Scheme),
    case TransportMod:listener_termination(ExUriURL) of
        child ->
            hello_listener_supervisor:stop_child(TransportMod, ExUriURL);
        _ ->
            ok
    end.

all() ->
    hello_registry:all(listener).

async_incoming_message(Context, ExUriURL, Binary) ->
    spawn(?MODULE, handle_incoming_message, [Context, ExUriURL, Binary]).

await_answer() ->
    receive
        {?INCOMING_MSG, Response} ->
            Response
    after
        5000 ->
            {error, timeout}
    end.

handle_incoming_message(Context, ExUriURL, Binary) ->
    {ok, _, #listener{protocol = ProtocolMod, protocol_opts = ProtocolOpts, router = Router}} = hello_registry:lookup({listener, ExUriURL}),
    {ok, BinResp} = hello_proto:handle_incoming_message(Context, ProtocolMod, ProtocolOpts, Router, ExUriURL, Binary),
    send(BinResp, Context).

send(BinResp, Context = #context{transport = TransportMod}) ->
    TransportMod:send_response(Context, BinResp).
%% -------------------------------------------------------------------------------
%% -- helpers TODO e.g. for keep alive mechanism
start1(ExUriURL = #ex_uri{scheme = Scheme}, TransportOpts) ->
    TransportMod = transport_module(Scheme),
    case TransportMod:listener_specification(ExUriURL, TransportOpts) of
        {make_child, ListenerChildSpec} ->
            {ok, _} = hello_listener_supervisor:start_child(ListenerChildSpec),
            {ok, make_ref()};
        {other_supervisor, _Result} ->
            {ok, make_ref()};
        {error, Reason} ->
            erlang:error(Reason)
    end.

transport_module("zmq-tcp")  -> hello_zmq_listener;
transport_module("zmq-tcp6") -> hello_zmq_listener;
transport_module("zmq-ipc")  -> hello_zmq_listener;
transport_module("http")     -> hello_http_listener;
transport_module(_Scheme)    -> error(notsup).
