-module(hello_msgpack).
-behaviour(hello_decoder).
-export([decode/1, encode/1, signature/0]).

decode(Binary) ->
    {ok, Unpacked} = msgpack:unpack(Binary, [{format, map}]),
    Unpacked.

encode(Json) ->
    msgpack:pack(Json, [{format, jsx}, {allow_atom, pack}]).

signature() -> <<16#AA, 16#FD>>.
