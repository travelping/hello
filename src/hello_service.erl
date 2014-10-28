-module(hello_service).
-export([register/2, unregister/1, call/3, await/1, outgoing_message/2, all/0]).
-include("internal.hrl").

-define(REG_NAME(Name), Name}).

register(HandlerMod, HandlerArgs) ->
    Name = to_bin(HandlerMod:name()),
    hello_registry:register({service, Name}, {HandlerMod, HandlerArgs}).

unregister(HandlerMod) ->
    Name = to_bin(HandlerMod:name()),
    hello_registry:unregister({service, Name}).

to_bin(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
to_bin(List) when is_list(List) -> list_to_binary(List);
to_bin(Bin) when is_binary(Bin) -> Bin.

call(Name, Identifier, {Method, Args}) ->
    call(Name, Identifier, #request{method = Method, args = Args});
call(Name, Identifier, Request) ->
    case hello_registry:lookup({service, Name}) of
        {ok, _, {HandlerMod, HandlerArgs}} ->
            Handler = hello_handler:get_handler(Name, Identifier, HandlerMod, HandlerArgs),
            hello_handler:process(Handler, Request);
        {error, not_found} ->
            {error, service_not_found}
    end.

await(Timeout) ->
    receive
        {?INCOMING_MSG, Response} ->
            Response
    after
        Timeout ->
            {error, timeout}
    end.

outgoing_message(Context = #context{connection_pid = ConnectionPid}, Response) ->
    ConnectionPid ! {?INCOMING_MSG, Response}.

all() -> hello_registry:all(service).
