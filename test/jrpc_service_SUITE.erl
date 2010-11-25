%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(jrpc_service_SUITE).
-compile(export_all).

-include("ct.hrl").
-include_lib("tp_json_rpc/include/tp_json_rpc.hrl").

% ---------------------------------------------------------------------
% -- test cases
-record(example_rec, {a = "default value of a from record definition", b, c}).

-define(checkExit(Exn, Form), (try Form, not_ok catch exit:Exn -> ok end)).

record_to_json_object(_Config) ->
    Rec1 = #example_rec{a = <<"a">>, b = 2, c = [<<"c">>, 3]},
    {obj, Props1} = ?record_to_json_obj(example_rec, Rec1),

    <<"a">>      = proplists:get_value("a", Props1),
    2            = proplists:get_value("b", Props1),
    [<<"c">>, 3] = proplists:get_value("c", Props1),

    % undefined -> null
    {obj, Props2} = ?record_to_json_obj(example_rec, #example_rec{a = undefined}),
    null = proplists:get_value("a", Props2),

    % atom -> binary
    {obj, Props3} = ?record_to_json_obj(example_rec, #example_rec{a = an_atom}),
    <<"an_atom">> = proplists:get_value("a", Props3),

    % complex values
    Rec4 = #example_rec{b = {1, 2, 3}}, % tuples cannot be represented in plain json
    ok = ?checkExit(json_incompatible, ?record_to_json_obj(example_rec, Rec4)),

    Rec5 = #example_rec{b = {obj, [{"foo", [<<"first">>, {1, 2}]}]}}, % incompatible value in nested object
    ok = ?checkExit(json_incompatible, ?record_to_json_obj(example_rec, Rec5)),

    % bad record
    ok = ?checkExit(badarg, ?record_to_json_obj(example_rec, {foobar, 1, 2, 3})),
    ok = ?checkExit(badarg, ?record_to_json_obj(example_rec, {test_rec, 1})),
    ok = ?checkExit(badarg, ?record_to_json_obj(example_rec, <<"not a record at all">>)),

    ok.

json_object_to_record(_Config) ->
    Obj1 = {obj, [{"a", 1}, {"b", 2}, {"c", 3}]},
    {ok, #example_rec{a = 1, b = 2, c = 3}} = ?json_obj_to_record(example_rec, Obj1),

    % null -> undefined
    Obj2 = {obj, [{"a", 1}, {"b", null}, {"c", 3}]},
    {ok, #example_rec{a = 1, b = undefined, c = 3}} = ?json_obj_to_record(example_rec, Obj2),

    % default value from record definition for keys that are not present
    Obj3 = {obj, [{"b", 2}]},
    {ok, #example_rec{a = "default value of a from record definition", b = 2, c = undefined}} =
        ?json_obj_to_record(example_rec, Obj3),

    % custom defaults record
    Obj4 = {obj, [{"a", 1}]},
    {ok, #example_rec{a = 1, b = <<"default b">>, c = <<"default c">>}} =
        ?json_obj_into_record(example_rec, #example_rec{b = <<"default b">>, c = <<"default c">>}, Obj4),

    % error for non-objects
    {error, not_object} = ?json_obj_to_record(example_rec, <<"not an object">>),
    {error, not_object} = ?json_obj_to_record(example_rec, 1),
    {error, not_object} = ?json_obj_to_record(example_rec, ["a", "b", "c"]),

    % error if custom defaults record doesn't match definition
    {error, bad_defaults} = ?json_obj_into_record(example_rec, {foobar, 1, 2, 3}, {obj, []}),
    {error, bad_defaults} = ?json_obj_into_record(example_rec, {example_rec, 1}, {obj, []}),
    {error, bad_defaults} = ?json_obj_into_record(example_rec, <<"not a record at all">>, {obj, []}),

    ok.

% ---------------------------------------------------------------------
% -- common_test callbacks
all() -> [record_to_json_object, json_object_to_record].
