-module(hello_lib).
-export([get_in/2, to_binary/1, wrap/1, get/2, get/3]).

get_in(Map, Path) when is_list(Path) == false -> get_in(Map, Path);
get_in(Value, []) -> Value;
get_in(Map, [Path | Pathes]) when is_map(Map) ->
    case maps:get(Path, Map, undefined) of
        undefined ->
            undefined;
        NextValue ->
            get_in(NextValue, Pathes)
    end;

get_in(_Value, _Pathes) -> undefined.

get(Map, Key) -> get(Map, Key, undefined).
get(Map, Key, Default) -> maps:get(Key, Map, Default).

to_binary(Str) when is_atom(Str)   -> atom_to_binary(Str, utf8);
to_binary(Str) when is_list(Str)   -> unicode:characters_to_binary(Str);
to_binary(Str) when is_binary(Str) -> Str;
to_binary(Term) -> unicode:characters_to_binary(io_lib:format("~p", [Term])).

wrap(Requests) when is_list(Requests) -> Requests;
wrap(Request) -> [Request].
