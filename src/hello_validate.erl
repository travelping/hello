% Copyright (c) 2010-2011 by Travelping GmbH <info@travelping.com>

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
-module(hello_validate).
-export([find_method/2, request_params/3, type/2]).
-export_type([json_type/0, param_type/0]).

-include("hello.hrl").
-include("internal.hrl").

-type json_type()  :: 'boolean' | 'object' | 'integer' | 'float' | 'number' | 'string' | 'list' | 'array' | 'any'.
-type param_type() :: json_type() | {enum, [atom()]}.

%% --------------------------------------------------------------------------------
%% -- API functions
-spec find_method(list(#rpc_method{}), atom() | string() | binary()) -> #rpc_method{} | undefined.

find_method(MList, MethodName) when is_binary(MethodName) ->
    find_method(MList, binary_to_list(MethodName));
find_method(MList, MethodName) when is_list(MethodName) ->
    case catch list_to_existing_atom(MethodName) of
        {'EXIT', {badarg, _TR}} -> undefined;
        Atom                    -> find_method(MList, Atom)
    end;
find_method(MList, MethodName) when is_atom(MethodName) ->
    case lists:keyfind(MethodName, #rpc_method.name, MList) of
        false  -> undefined;
        Method -> Method
    end.

-spec request_params(#rpc_method{}, list(#rpc_param{}), #request{}) -> {ok, [hello_json:value()] | [{atom(), hello_json:value()}]}
                                                                     | {error, iodata()}.

request_params(#rpc_method{params_as = WantParamEncoding}, PInfo, #request{params = ParamsIn}) ->
    try
        Params = params_to_proplist(PInfo, ParamsIn),
        Validated = lists:map(fun (OneParamInfo) -> validate_field(OneParamInfo, Params) end, PInfo),
        case WantParamEncoding of
            proplist -> {ok, Validated};
            list     -> {ok, lists:map(fun ({_K, V}) -> V end, Validated)}
        end
    catch
        throw:{invalid, Msg} -> {error, Msg}
    end.

-spec type(json_type(), hello_json:value()) -> boolean().

type(boolean, Val) when (Val == true) or (Val == false) -> true;
type(object, {_}) -> true;
type(integer, Val) when is_integer(Val) -> true;
type(float, Val) when is_float(Val) -> true;
type(number, Val) when is_number(Val) -> true;
type(string, Val) when is_binary(Val) -> true;
type(list, Val) when is_list(Val) -> true;
type(array, Val) when is_list(Val) -> true;
type(iso_date, Val) when is_binary(Val) -> validate_date(Val);
type(any, _Val) -> true;
type(_T, _Val) -> false.

%% --------------------------------------------------------------------------------
%% -- internal functions
validate_field(Info = #rpc_param{name = PNameAtom}, Param) ->
    PName = atom_to_binary(PNameAtom, utf8),
    Value = case proplists:get_value(PName, Param) of
                Undef when (Undef =:= undefined) or (Undef =:= null) ->
                    if Info#rpc_param.optional -> Info#rpc_param.default;
                       true -> throw({invalid, ["required parameter '", PName, "' is missing"]})
                    end;
                GivenValue ->
                    validate_type(PName, Info, GivenValue)
            end,
    {PNameAtom, Value}.

validate_type(PName, #rpc_param{type = PType}, GivenValue) ->
    case PType of
        {enum, Elems} ->
            atom_from_enum(PName, Elems, GivenValue);
        _T ->
            case type(PType, GivenValue) of
                true -> GivenValue;
                false -> throw({invalid, ["invalid parameter type for param '", PName, "': expected ", atom_to_list(PType)]});
                {true, NewValue} -> NewValue
            end
    end.

atom_from_enum(Param, Enum, Input) ->
    try
        A = erlang:binary_to_existing_atom(Input, utf8),
        case lists:member(A, Enum) of
            true -> A;
            false -> erlang:error(badarg)
        end
    catch
        error:badarg ->
            Choices = string:join(lists:map(fun (P) -> ["\"", atom_to_list(P), "\""] end, Enum), ", "),
            throw({invalid, ["parameter '", Param, "' must be one of: ", Choices]})
    end.

params_to_proplist(_PInfo, {Props}) -> Props;
params_to_proplist(PInfo,  Params) when is_list(Params) ->
    Names = lists:map(fun (P) -> atom_to_binary(P#rpc_param.name, utf8) end, PInfo),
    {Proplist, TooMany} = zip(Names, Params, {[], false}),
    TooMany andalso throw({invalid, "superfluous parameters"}),
    lists:reverse(Proplist).

zip([], [], Result) ->
    Result;
zip([], _2, {Result, _TM}) ->
    zip([], [], {Result, true});
zip(_1, [], Result) ->
    zip([], [], Result);
zip([H1|R1], [H2|R2], {Result, TooMany}) ->
    zip(R1, R2, {[{H1, H2}|Result], TooMany}).

validate_date(Date) when is_binary(Date) ->
    validate_date(binary_to_list(Date));
validate_date(Date) ->
    case re:run(Date, "^([0-9]{4})(-?)([0-9]{2})(-?)([0-9]{2})([tT])(.*)$", [{capture, all_but_first, list}]) of
        {match, [Year, Cut1, Month, Cut2, Day, T, TimeString]} when (((T == "t") or (T == "T")) and (Cut1 == Cut2)) ->
            case re:run(TimeString, "^([0-9]{2})(:?)([0-9]{2})(:?)([0-9]{2})(.*)$", [{capture, all_but_first, list}]) of
                {match, [Hour, Cut3, Minute, Cut4, Second, TimeZoneString]} when (Cut3 == Cut4) ->
                    validate_datetime({s2i(Year), s2i(Month), s2i(Day)}, {s2i(Hour), s2i(Minute), s2i(Second)}, TimeZoneString);
                _ ->
                    false
            end;
        _ -> false
    end.


validate_datetime({Year, Month, Day} = DateTuple, {Hour, Minute, Second} = TimeTuple, TimeZoneString) ->
    case {calendar:valid_date(Year, Month, Day), valid_time(Hour, Minute, Second)} of
        {true, true} ->
            check_time_zone(DateTuple, TimeTuple, TimeZoneString);
        _ ->
            false
    end.

check_time_zone(DateTuple, TimeTuple, TimeZoneString) ->
    case re:run(TimeZoneString, "^([Z+-])([0-9]{2})?(:?)([0-9]{2})?$", [{capture, all_but_first, list}]) of
        {match, ["Z", "", ""]} ->
            {true, {DateTuple, TimeTuple}};
        {match, [T | Other]} when ((T == "+") or (T == "-")) ->
            add_time({DateTuple, TimeTuple}, T, Other);
        _ ->
            false
    end.

valid_time(H, M, S) when (H >= 0) and (H =< 23) and (M >= 0) and (M =< 59) and (S >= 0) and (S =< 59) ->
    true;
valid_time(_, _, _) ->
    false.

s2i("") -> 0;
s2i(Str) -> list_to_integer(Str).

add_time(DateTime, T, [H | Tail]) ->
    Seconds1 = calendar:datetime_to_gregorian_seconds(DateTime),
    Seconds2 = add(T, Seconds1, s2i(H) * 3600),
    Seconds3 =  case Tail of
                    [""] ->
                        Seconds2;
                    [_Cut, Minutes] ->
                        add(T, Seconds2, s2i(Minutes) * 60)
                end,
    {true, calendar:gregorian_seconds_to_datetime(Seconds3)}.

add("+", A, B) -> A - B;
add("-", A, B) -> A + B.
