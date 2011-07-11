-module(hello_json_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("../include/hello.hrl").

% ---------------------------------------------------------------------
% -- test cases
-record(example_rec, {a = "default value of a from record definition", b, c}).

-define(checkExit(Exn, Form), (try Form, not_ok catch error:Exn -> ok end)).

record_to_json_object(_Config) ->
    Rec1 = #example_rec{a = <<"a">>, b = 2, c = [<<"c">>, 3]},
    {Props1} = ?record_to_json_obj(example_rec, Rec1),

    <<"a">>      = proplists:get_value("a", Props1),
    2            = proplists:get_value("b", Props1),
    [<<"c">>, 3] = proplists:get_value("c", Props1),

    % obj as value
    Rec2 = #example_rec{a = {[{"key", 3}]}},
    {Props2}  = ?record_to_json_obj(example_rec, Rec2),
    {Props2a} = proplists:get_value("a", Props2),
    3         = proplists:get_value("key", Props2a),

    % undefined -> null
    {Props3} = ?record_to_json_obj(example_rec, #example_rec{a = undefined}),
    null = proplists:get_value("a", Props3),

    % atom -> binary
    {Props4} = ?record_to_json_obj(example_rec, #example_rec{a = an_atom}),
    <<"an_atom">> = proplists:get_value("a", Props4),

    % complex values
    Rec5 = #example_rec{b = {1, 2, 3}}, % tuples cannot be represented in plain json
    ok = ?checkExit(badjson, ?record_to_json_obj(example_rec, Rec5)),

    Rec6 = #example_rec{b = {[{"foo", [<<"first">>, {1, 2}]}]}}, % incompatible value in nested object
    ok = ?checkExit(badjson, ?record_to_json_obj(example_rec, Rec6)),

    % bad record
    ok = ?checkExit(badarg, ?record_to_json_obj(example_rec, {foobar, 1, 2, 3})),
    ok = ?checkExit(badarg, ?record_to_json_obj(example_rec, {test_rec, 1})),
    ok = ?checkExit(badarg, ?record_to_json_obj(example_rec, <<"not a record at all">>)),

    ok.

json_object_to_record(_Config) ->
    Obj1 = {[{"a", 1}, {"b", 2}, {"c", 3}]},
    #example_rec{a = 1, b = 2, c = 3} = ?json_obj_to_record(example_rec, Obj1),

    % null -> undefined
    Obj2 = {[{"a", 1}, {"b", null}, {"c", 3}]},
    #example_rec{a = 1, b = undefined, c = 3} = ?json_obj_to_record(example_rec, Obj2),

    % default value from record definition for keys that are not present
    Obj3 = {[{"b", 2}]},
    #example_rec{a = "default value of a from record definition", b = 2, c = undefined} =
        ?json_obj_to_record(example_rec, Obj3),

    % custom defaults record
    Obj4 = {[{"a", 1}]},
    #example_rec{a = 1, b = <<"default b">>, c = <<"default c">>} =
        ?json_obj_into_record(example_rec, #example_rec{b = <<"default b">>, c = <<"default c">>}, Obj4),

    % error for non-objects
    ?checkExit(badarg, ?json_obj_to_record(example_rec, <<"not an object">>)),
    ?checkExit(badarg, ?json_obj_to_record(example_rec, 1)),
    ?checkExit(badarg, ?json_obj_to_record(example_rec, ["a", "b", "c"])),

    % error if custom defaults record doesn't match definition
    ?checkExit(badarg, ?json_obj_into_record(example_rec, {foobar, 1, 2, 3}, {[]})),
    ?checkExit(badarg, ?json_obj_into_record(example_rec, {example_rec, 1}, {[]})),
    ?checkExit(badarg, ?json_obj_into_record(example_rec, <<"not a record at all">>, {[]})),

    ok.

% ---------------------------------------------------------------------
% -- common_test callbacks
all() -> [record_to_json_object, json_object_to_record].
