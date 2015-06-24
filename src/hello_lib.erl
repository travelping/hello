-module(hello_lib).
-export([get_in/2, to_binary/1, wrap/1, get/2, get/3, dnssd_service_type/2, dnssd_service_type/3]).

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

-spec dnssd_service_type(Scheme :: string(), App :: binary()) -> ServiceName :: binary().
dnssd_service_type(Scheme, App) ->
    SchemeBin = hello_lib:to_binary(Scheme), 
    App1 = binary:replace(App, <<"/">>, <<"_">>, [global]), 
    <<"_", SchemeBin/binary, "_", App1/binary, "._tcp.">>.

-spec dnssd_service_type(Scheme :: string(), Host :: string(), Path :: string()) -> ServiceName :: binary().
dnssd_service_type(Scheme, Host, Path) ->
    SchemeBin = hello_lib:to_binary(Scheme), 
    HostBin = hello_lib:to_binary(Host), 
    PathBin = binary:replace(hello_lib:to_binary(Path), <<"/">>, <<"_">>, [global]), 
    <<"_", SchemeBin/binary, "_", HostBin/binary, "_", PathBin/binary, "._tcp.">>.
