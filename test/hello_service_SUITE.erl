-module(hello_service_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("hello_test.hrl").
-include("../include/internal.hrl").

all() ->
    [register].
