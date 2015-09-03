-module(hello_service).
-export([register_link/2, unregister_link/1, lookup/1, call/3, await/0, await/1, outgoing_message/2, all/0]).
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

call(Name, Identifier, {Method, Args}) ->
    Context = #context{connection_pid = self(), peer = make_ref()},
    call(Name, Identifier, #request{context = Context, method = Method, args = Args}),
    await();
call(Name, Identifier, Request) ->
    case hello_registry:lookup({service, Name}) of
        {ok, _, {HandlerMod, HandlerArgs}} ->
            Handler = hello_handler:get_handler(Name, Identifier, HandlerMod, HandlerArgs),
            hello_handler:process(Handler, Request);
        {error, not_found} ->
            ?LOG_INFO("Hello service ~s not found.", [Name],
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
