-module(log_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

% ---------------------------------------------------------------------
% -- test cases
map(_Config) ->
    ReqMap = #{<<"a">> => 2, <<"b">> => 1, <<"c">> => #{<<"d">> => "d", <<"e">> => "e"}},
    RepMap = #{a => 2, b => 1, c => #{d => "d", e => "e"}},

    init_log([]),
    true = (hello_json:encode(ReqMap) == hello_log:format_params(ReqMap, req)),
    true = (hello_json:encode(RepMap) == hello_log:format_params(RepMap, resp)),

    init_log([a]),
    true = (hello_json:encode(ReqMap#{<<"a">> => <<"HIDDEN">>}) 
            == hello_log:format_params(ReqMap, req)),
    true = (hello_json:encode(RepMap#{a => <<"HIDDEN">>}) 
            == hello_log:format_params(RepMap, resp)),

    init_log([[c, d]]),
    true = (hello_json:encode(ReqMap#{<<"c">> => #{<<"d">> => <<"HIDDEN">>, <<"e">> => "e"}}) 
            == hello_log:format_params(ReqMap, req)),
    true = (hello_json:encode(RepMap#{c => #{d => <<"HIDDEN">>, e => "e"}}) 
            == hello_log:format_params(RepMap, resp)),
    ok.

list(_Config) ->
    List = [{a, "a"}, {b, 1}, {c, [{d, "d"}]}],

    init_log([]),
    true = (hello_json:encode(List) == hello_log:format_params(List, resp)),

    init_log([a]),
    true = (hello_json:encode(lists:keyreplace(a, 1, List, {a, <<"HIDDEN">>})) 
            == hello_log:format_params(List, resp)),
    init_log([<<"a">>]),
    true = (hello_json:encode(lists:keyreplace(a, 1, List, {a, <<"HIDDEN">>})) 
            == hello_log:format_params(List, resp)),

    init_log([[c, d]]),
    true = (hello_json:encode(lists:keyreplace(c, 1, List, {c, [{d, <<"HIDDEN">>}]})) 
            == hello_log:format_params(List, resp)),
    ok.

mixed(_Config) ->
    Map = #{a => 2, b => [{r, 1}], c => #{d => "d"}, e => #{f => #{x => "x"}}},

    init_log([[c, d]]),
    true = (hello_json:encode(Map#{c => #{d => <<"HIDDEN">>}}) 
            == hello_log:format_params(Map, resp)),

    init_log([[b, r]]),
    true = (hello_json:encode(Map#{b => #{r => <<"HIDDEN">>}}) 
            == hello_log:format_params(Map, resp)),

    init_log([[e, f, x]]),
    true = (hello_json:encode(Map#{e => #{f => #{x => <<"HIDDEN">>}}}) 
            == hello_log:format_params(Map, resp)),

    init_log([a]),
    [<<"a">>] = application:get_env(hello, hidden_params, []),
    true = (hello_json:encode([<<"arg">>]) 
            == hello_log:format_params([<<"arg">>], req)),
    true = (hello_json:encode([<<"arg">>]) 
            == hello_log:format_params([<<"arg">>], resp)),
    true = (hello_json:encode(["arg"]) 
            == hello_log:format_params(["arg"], req)),
    true = (hello_json:encode(["arg"]) 
            == hello_log:format_params(["arg"], resp)),
    ok.

% ---------------------------------------------------------------------
% -- common_test callbacks
all() -> [map, 
          list,
          mixed
         ].

init_log(Params) ->
    application:set_env(hello, hidden_params, Params),
    hello_log:init().

init_per_suite(Config) ->
    application:set_env(hello, log_formatter, json),
    Config.

end_per_suite(_Config) ->
    application:set_env(hello, hidden_params, []),
    ok.
