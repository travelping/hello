-module(hello_service).
-export([register_link/2, unregister_link/1, lookup/1, call/3, call/4, await/0, await/1, outgoing_message/2, all/0]).
-include("hello.hrl").
-include("hello_log.hrl").

-define(REG_NAME(Name), Name}).

register_link(HandlerMod, HandlerArgs) ->
    Name = hello_lib:to_binary(HandlerMod:name()),
    hello_registry:register_link({service, Name}, self(), {HandlerMod, HandlerArgs}).

unregister_link(HandlerMod) ->
    Name = hello_lib:to_binary(HandlerMod:name()),
    hello_registry:unregister_link({service, Name}).

lookup(HandlerMod) ->
    Name = hello_lib:to_binary(HandlerMod:name()),
    hello_registry:lookup({service, Name}).

call(Name, Identifier, Request) ->
    call(Name, Identifier, Request, local).

call(Name, Identifier, {Method, Args}, ExUriURL) ->
    Context = #context{connection_pid = self(), peer = make_ref()},
    case call(Name, Identifier, #request{context = Context, method = Method, args = Args}, ExUriURL) of
        {error, method_not_found} = NotFound -> NotFound;
        _ -> await()
    end;
call(Name, Identifier, Request = #request{context = Context}, ExUriURL) ->
    case hello_registry:lookup({service, Name}) of
        {ok, _, {HandlerMod, HandlerArgs}} ->
            MetricsInfo = make_metrics_info(Name, Context#context.listener_id, ExUriURL),
            Handler = hello_handler:get_handler(Name, Identifier, HandlerMod, HandlerArgs, MetricsInfo),
            hello_handler:process(Handler, Request);
        {error, not_found} ->
            ?LOG_WARNING("Service ~s not found.", [Name],
                      [{hello_error_response, {error, method_not_found}}], ?LOGID58),
            {error, method_not_found}
    end.

await() ->
    ServerTimeout = application:get_env(hello, server_timeout, 10000),
    await(ServerTimeout).

await(Timeout) ->
    receive
        {?INCOMING_MSG, Response} ->
            Response
    after
        Timeout ->
            {error, timeout}
    end.

outgoing_message(_Context = #context{connection_pid = ConnectionPid}, Response) ->
    ConnectionPid ! {?INCOMING_MSG, Response}.

all() -> hello_registry:all(service).

make_metrics_info(ServiceName, _ListenerId, local) ->
    AtomServiceName = hello_metrics:to_atom(ServiceName),
    {AtomServiceName, local, undefined, undefined};
make_metrics_info(ServiceName, ListenerId, ExUriURL) ->
    AtomListenerId = hello_metrics:to_atom(ListenerId),
    {AtomListenerIP, AtomListenerPort} = hello_metrics:atomize_ex_uri(ExUriURL),
    AtomServiceName = hello_metrics:to_atom(ServiceName),
    {AtomServiceName, AtomListenerId, AtomListenerIP, AtomListenerPort}.
