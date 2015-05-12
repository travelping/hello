-module(hello_msgpack).
-behaviour(hello_decoder).
-export([decode/1, encode/1, mime_type/0]).

decode(Binary) ->
    {ok, Unpacked} = msgpack:unpack(Binary, [{format, map}]),
    Unpacked.

encode(Json) ->
    msgpack:pack(Json, [{format, jsx}]).

mime_type() -> <<"application/x-msgpack">>.
