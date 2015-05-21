-module(hello_json).
-behaviour(hello_decoder).
-export([decode/1, encode/1, signature/0]).

decode(Binary) ->
    jsx:decode(Binary, [return_maps]).

encode(Json) ->
    jsx:encode(Json).

signature() -> <<16#AA, 16#FF>>.
