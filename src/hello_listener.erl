-module(hello_listener).
-export([start/5, stop/1, lookup/1, port/1, all/0, async_incoming_message/4, handle_incoming_message/4]).
-export([behaviour_info/1]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("hello.hrl").
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
     {send_response, 3},
     {close, 1},
     {port, 2},
     {listener_termination, 2}
     ];
behaviour_info(_) ->
    undefined.

start(ExUriURL, TransportOpts, Protocol, ProtocolOpts, RouterMod) ->
    case lookup(ExUriURL) of
        {error, not_found} ->
            case start1(ExUriURL, TransportOpts) of
                {ok, ListenerRef} ->
                    ListenerInfo = #listener{ref = ListenerRef,
                                             exuri = ExUriURL,
                                             protocol = Protocol,
                                             protocol_opts = ProtocolOpts,
                                             router = RouterMod},
                    hello_registry:register_link({listener, ExUriURL}, self(), ListenerInfo),
                    {ok, ListenerRef};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, _, ListenerRef} ->
            {ok, ListenerRef}
    end.

stop(ExUriURL = #ex_uri{scheme = Scheme}) ->
    TransportMod = transport_module(Scheme),
    case lookup(ExUriURL) of
        {ok, _, #listener{ref = ListenerRef}} ->
            hello_registry:unregister_link({listener, ExUriURL}),
            case TransportMod:listener_termination(ExUriURL, ListenerRef) of
                child -> hello_listener_supervisor:stop_child(TransportMod, ExUriURL);
                _     -> ok
            end;
        {error, not_found} ->
            {error, not_found}
    end.

lookup(ExUriURL) ->
    hello_registry:lookup({listener, ExUriURL}).

port(#ex_uri{authority = #ex_uri_authority{port = Port}}) when is_integer(Port) andalso Port > 0 -> Port;
port(ExUriURL = #ex_uri{scheme = Scheme}) ->
    TransportMod = transport_module(Scheme),
    case lookup(ExUriURL) of
        {ok, _, ListenerRef} -> TransportMod:port(ExUriURL, ListenerRef#listener.ref);
        {error, not_found}   -> error(badarg, [ExUriURL])
    end.

all() ->
    hello_registry:all(listener).

async_incoming_message(Context, ExUriURL, Signarute, Binary) ->
    spawn(?MODULE, handle_incoming_message, [Context, ExUriURL, Signarute, Binary]).

handle_incoming_message(Context, ExUriURL, Signarute, Binary) ->
    {ok, _, #listener{protocol = ProtocolMod, protocol_opts = ProtocolOpts, router = Router}} = lookup(ExUriURL),
    case hello_proto:handle_incoming_message(Context, ProtocolMod, ProtocolOpts, Router, ExUriURL, Signarute, Binary) of
        {ok, BinResp} ->
            % for backward compatibility we should to send Signature that we received on listener 
            send(Signarute, BinResp, Context);
            % when we will convinced that there aren't clients with old code we will start to send Signature from protocol
            % send(hello_proto:signature(ProtocolMod, ProtocolOpts), BinResp, Context),
        ignore -> ignore
    end,
    close(Context).

send(Signarute, BinResp, Context = #context{transport = TransportMod}) ->
    TransportMod:send_response(Context, Signarute, BinResp).

close(Context = #context{transport = TransportMod}) ->
    TransportMod:close(Context).
%% -------------------------------------------------------------------------------
%% -- helpers TODO e.g. for keep alive mechanism
start1(ExUriURL = #ex_uri{scheme = Scheme}, TransportOpts) ->
    TransportMod = transport_module(Scheme),
    case TransportMod:listener_specification(ExUriURL, TransportOpts) of
        {make_child, ListenerChildSpec} ->
            {ok, Pid} = hello_listener_supervisor:start_child(ListenerChildSpec),
            {ok, Pid};
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
