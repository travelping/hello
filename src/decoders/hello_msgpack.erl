-module(hello_msgpack).
-export([decode/1, encode/1]).

decode(Binary) ->
    {ok, Unpacked} = msgpack:unpack(Binary, [{format, map}]),
    Unpacked.

encode(Json) ->
    msgpack:pack(Json, [{format, jsx}]).
