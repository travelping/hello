% Copyright (c) 2010-2015 by Travelping GmbH <info@travelping.com>

% Permission is hereby granted, free of charge, to any person obtaining a
% copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
% DEALINGS IN THE SOFTWARE.

% @private
-module(hello_log).

-export([fmt_response/1, fmt_request/1, init/0]).
-export([format_params/2, to_binary/1]). % for test

-include("hello.hrl").


init() ->
    HiddenParams0 = application:get_env(hello, hidden_params, []),
    HiddenParams = [to_binary(P) || P <- HiddenParams0],
    application:set_env(hello, hidden_params, HiddenParams).


%% --------------------------------------------------------------------------------
%% -- Formaters
fmt_request(#request{method = Method, args = Params}) ->
    <<"method: ", (encode(Method))/binary, ", params: ", (format_params(Params, req))/binary>>.

fmt_response(ignore) -> ["ignored"];
fmt_response({ok, Result}) -> <<(format_params(Result, resp))/binary>>;
fmt_response(Result) -> io_lib:format("~4096p", [Result]).


%% --------------------------------------------------------------------------------
%% -- Helpers
format_params(Params0, Type) -> 
    HiddenParams = application:get_env(hello, hidden_params, []),
    Params = case Type of
        req -> Params0;
        resp -> params_to_binary(Params0)
    end,
    format_params_(Params, HiddenParams).

params_to_binary(Params) when is_list(Params) ->
    F = fun({Key, Value}) -> {hello_lib:to_binary(Key), params_to_binary(Value)};
           (Param) -> Param
        end,
    [F(Param) || Param <- Params];
params_to_binary(Params) when is_map(Params) ->
    Keys = maps:keys(Params),
    lists:foldl(fun(Key, Map) -> 
                    maps:put(hello_lib:to_binary(Key),
                             params_to_binary(maps:get(Key, Map)), 
                             maps:remove(Key, Map))
                end, Params, Keys);
params_to_binary(Params) -> Params.

to_binary([]) -> [];
to_binary([Head | Tail]) -> [hello_lib:to_binary(Head) | to_binary(Tail)];
to_binary(Key) -> hello_lib:to_binary(Key).

format_params_(Params, []) -> encode(Params);

format_params_(Params, HiddenParams) ->
    NewParams = lists:foldl(fun F([Key | [_|_] = Tail], Map) when is_map(Map) -> 
                                   F1 = fun() -> 
                                            maps:update(Key, F(Tail, maps:get(Key, Map)), Map)
                                        end,
                                   is_key(Key, Map, F1);
                                F([Key |[_|_] = Tail], List) when is_list(List) ->
                                   F1 = fun() -> 
                                            lists:keyreplace(Key, 1, List, {Key, F(Tail, proplists:get_value(Key, List))})
                                        end,
                                   is_key(Key, List, F1);
                                F([Key], MapOrList) -> hidden_key(Key, MapOrList);
                                F(Key, MapOrList) -> hidden_key(Key, MapOrList)
                            end, Params, HiddenParams),
    encode(NewParams).

is_key(Key, Map, Fun) when is_map(Map) ->
    case maps:is_key(Key, Map) of
        true -> Fun();
        false -> Map
    end;
is_key(Key, List, Fun) when is_list(List) ->
    case lists:keymember(Key, 1, List) of
        true -> Fun();
        false -> List
    end.

hidden_key(Key, Map) when is_map(Map) ->
    is_key(Key, Map, fun() -> maps:update(Key, <<"HIDDEN">>, Map) end);
hidden_key(Key, List) when is_list(List) ->
    is_key(Key, List, fun() -> lists:keyreplace(Key, 1, List, {Key, <<"HIDDEN">>}) end);
hidden_key(_, Other) -> Other.

encode(Data) -> 
    case application:get_env(hello, log_formatter, native) of
        json -> hello_json:encode(Data);
        _ -> list_to_binary(io_lib:format("~4096p", [Data]))
    end.
