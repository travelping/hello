-module(hello_router).
-export([route/3]).
-include("hello.hrl").
-include("hello_log.hrl").

route(_Context = #context{session_id = Id}, Request = #request{method = Method}, ExURI) ->
    Namespace = get_namespace(Method),
    case hello_binding:lookup(ExURI, Namespace) of
        {error, not_found} ->
            ?LOG_INFO("Hello router couldn't find service for a key ~s. Request is dismissed.", [Namespace],
                            [   {hello_request, hello_log:format(Request)},
                                {hello_error_response, {error, not_found, Namespace}}],
                         ?LOGID37),
            {error, method_not_found};
        {ok, _, Name} ->
            ?LOG_DEBUG("Hello router resolved namespace '~p' to hello handler '~p'.", [Namespace, Name],
                        [{hello_request, hello_log:format(Request)}], ?LOGID38),
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
