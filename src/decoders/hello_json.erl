-module(hello_json).
-behaviour(hello_decoder).
-export([decode/1, encode/1, mime_type/0]).

decode(Binary) ->
    jsx:decode(Binary, [return_maps]).

encode(Json) ->
    jsx:encode(Json).

mime_type() -> <<"application/json">>.
