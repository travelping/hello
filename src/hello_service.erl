-module(hello_service).
-export([register_link/2, unregister_link/1, call/3, await/1, outgoing_message/2, all/0]).
-include("hello.hrl").
-include("hello_log.hrl").

-define(REG_NAME(Name), Name}).

register_link(HandlerMod, HandlerArgs) ->
    Name = hello_lib:to_binary(HandlerMod:name()),
    hello_registry:register_link({service, Name}, self(), {HandlerMod, HandlerArgs}).

unregister_link(HandlerMod) ->
    Name = hello_lib:to_binary(HandlerMod:name()),
    hello_registry:unregister_link({service, Name}).

call(Name, Identifier, {Method, Args}) ->
    call(Name, Identifier, #request{method = Method, args = Args});
call(Name, Identifier, Request) ->
    case hello_registry:lookup({service, Name}) of
        {ok, _, {HandlerMod, HandlerArgs}} ->
            Handler = hello_handler:get_handler(Name, Identifier, HandlerMod, HandlerArgs),
            hello_handler:process(Handler, Request);
        {error, not_found} ->
            ?LOG_WARNING("Service ~s not found", [Name]),
            {error, method_not_found}
    end.

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
