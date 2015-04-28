-module(hello_json).
-export([decode/1, encode/1]).

decode(Binary) ->
    jsx:decode(Binary, [return_maps]).

encode(Json) ->
    jsx:encode(Json).
