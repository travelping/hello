-module(hello_listener).
-export([start/5, start/6, stop/1, lookup/1, port/1, all/0, async_incoming_message/4, handle_incoming_message/4]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("hello.hrl").
-include("hello_metrics.hrl").


%% Behaviour callbacks
-callback listener_specification(#ex_uri{}, trans_opts()) ->
    {make_child, Specs :: supervisor:child_spec()} | {other_supervisor, Result :: term()}.

-callback send_response(context(), signature(), Response :: binary()) -> ok.

-callback close(Context :: context()) -> ok.

-callback port(URI :: #ex_uri{}, ListenerRef :: listener_ref()) -> integer().

-callback listener_termination(URI :: #ex_uri{}, ListenerRef :: listener_ref()) ->
    child | ok | {error, not_found}.


%% --------------------------------------------------------------------------------
%% -- start and stop a listener
-record(listener, {
    id :: term(),
    exuri :: #ex_uri{},
    ref :: listener_ref(),
    protocol :: protocol(),
    protocol_opts :: protocol_opts(),
    metrics_info :: listener_metrics_info(),
    router :: module()
}).

start(ExUriURL, TransportOpts, Protocol, ProtocolOpts, RouterMod) ->
    start(undefined, ExUriURL, TransportOpts, Protocol, ProtocolOpts, RouterMod).

start(Id, ExUriURL, TransportOpts, Protocol, ProtocolOpts, RouterMod) ->
    case lookup(ExUriURL) of
        {error, not_found} ->
            case start1(ExUriURL, TransportOpts) of
                {ok, ListenerRef} ->
                    {AtomIP, AtomPort} = hello_metrics:atomize_ex_uri(ExUriURL),
                    MetricsInfo = {hello_metrics:to_atom(Id), AtomIP, AtomPort},
                    ListenerInfo = #listener{id = Id,
                                             ref = ListenerRef,
                                             exuri = ExUriURL,
                                             protocol = Protocol,
                                             protocol_opts = ProtocolOpts,
                                             metrics_info = MetricsInfo,
                                             router = RouterMod},
                    hello_metrics:create_listener(MetricsInfo),
                    hello_metrics:update_listener_time(last_reset, MetricsInfo),
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
        {ok, _, #listener{ref = ListenerRef, metrics_info = MetricsInfo}} ->
            hello_registry:unregister_link({listener, ExUriURL}),
            hello_metrics:delete_listener(MetricsInfo),
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
    proc_lib:spawn(?MODULE, handle_incoming_message, [Context, ExUriURL, Signarute, Binary]).

handle_incoming_message(Context, ExUriURL, Signature, [Binary]) ->
    handle_incoming_message(Context, ExUriURL, Signature, Binary);
handle_incoming_message(Context, ExUriURL, Signature, Binary) ->
    {ok, _, #listener{id = Id, protocol = ProtocolMod, protocol_opts = ProtocolOpts, router = Router, metrics_info = MetricsInfo}} = lookup(ExUriURL),
    hello_metrics:update_listener_packet(in, MetricsInfo, size(Binary)),
    case hello_proto:handle_incoming_message(Context#context{listener_id = Id}, ProtocolMod, ProtocolOpts, Router, ExUriURL, Signature, Binary) of
        {ok, BinResp} ->
            hello_metrics:update_listener_packet(out, MetricsInfo, size(BinResp)),
            % for backward compatibility we should to send Signature that we received on listener
            send(Signature, BinResp, Context);
            % when we will convinced that there aren't clients with old code we will start to send Signature from protocol
            % send(hello_proto:signature(ProtocolMod, ProtocolOpts), BinResp, Context),
        ignore -> ignore
    end,
    close(Context).

send(Signature, BinResp, Context = #context{transport = TransportMod}) ->
    TransportMod:send_response(Context, Signature, BinResp).

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
        {error, Reason} -> {error, Reason}
    end.

transport_module("zmq-tcp")  -> hello_zmq_listener;
transport_module("zmq-tcp6") -> hello_zmq_listener;
transport_module("zmq-ipc")  -> hello_zmq_listener;
transport_module("http")     -> hello_http_listener;
transport_module(_Scheme)    -> error(notsup).
