-module(hello_router).
-export([route/3]).
-include("internal.hrl").

route(_Context = #context{session_id = Id}, _, _) ->
    [{Name, _, _} | _] = hello_service:all(),
    {ok, Name, Id}.

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
