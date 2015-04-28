-module(hello_router).
-export([route/3]).
-include("internal.hrl").

route(_Context = #context{session_id = Id}, _Request = #request{method = Method}, ExURI) ->
    Namespace = get_namespace(Method),
    case hello_binding:lookup(ExURI, Namespace) of
        {error, not_found} ->
            lager:warning("Route couldn't find a service for a key ~s not found", [Namespace]),
            {error, method_not_found};
        {ok, _, Name} ->
            {ok, Name, Id}
    end.

get_namespace(Method) ->
    Method1 = hello_lib:to_binary(Method),
    SplittedMethod = binary:split(Method1, <<".">>, [global]),
    SplittedNs = lists:sublist(SplittedMethod, 1, length(SplittedMethod)-1),
    case SplittedNs of
        [] ->
            <<>>;
        [NsHead] ->
            NsHead;
        [NsHead | NsTail] ->
            lists:foldl( fun(NewNs, Ns) -> <<Ns/binary, ".", NewNs/binary>> end, NsHead, NsTail )
    end.
