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

%% @doc
%%    This module implements the generic part of
%%    a JSON-RPC service. It also manages the service registry
%%    and serves as the backbone of parameter validation.
%%
%%    == Object-to-Record Conversion ==
%%    One particularly useful facility is the automated conversion
%%    of JSON objects to and from Erlang records. Because record definitions
%%    exist only at compile time, the conversion routines are defined as
%%    macros in the `hello.hrl' include file. They are documented below.
%%
%%    <br/>
%%    <b>`?record_to_json_obj(RecordName::atom(), Record::tuple()) -> hello_json:jsonobj() | none()'</b>
%%
%%    This macro converts a record to a JSON object. Some things to be aware of:
%%    <ul>
%%      <li>`undefined' is converted to `null'</li>
%%      <li>The values contained in the record should adhere to the {@link hello_json:json()} type specification.
%%          Among other things, this means that all strings must be encoded as binaries and the only kind
%%          of tuple allowed is `{proplist()}'.<br/>
%%          If any value cannot be encoded, the conversion will exit with reason `json_incompatible'.
%%      </li>
%%      <li>The value passed in as `Record' must match the record definition of `RecordName'.
%%          If it doesn't, the conversion will exit with reason `badarg'.
%%      </li>
%%    </ul>
%%
%%    <br/>
%%    <b>`?json_obj_to_record(RecordName::atom(), Obj::hello_json:jsonobj()) -> {ok, tuple()} | {error, not_object}'</b>
%%
%%    This macro converts a JSON object into an Erlang record. The conversion will ignore any keys in the object that
%%    that do not have a corresponding field in the record. For missing keys the default value specified <i>in the record definition</i>
%%    will be used. Furthermore,
%%    <ul>
%%      <li>`null' is converted to `undefined'.</li>
%%      <li>`{error, not_object}' is returned if `Obj' is not a JSON object.</li>
%%    </ul>
%%
%%    <br/>
%%    <b>`?json_obj_into_record(RecordName::atom(), Defaults::tuple(), Obj::hello_json:jsonobj()) -> {ok, tuple()} | {error, not_object} | {error, bad_defaults}'</b>
%%
%%    This macro performs the same function as <b>`?json_obj_to_record/2'</b>, except that in case of missing keys the value
%%    used is taken <i>from the `Defaults' record</i>. You might find this macro useful if you want to merge an object
%%    <i>into</i> an existing instance of the record type in question.
%%
%%    `{error, bad_defaults}' is returned if `Defaults' does not match the definition of `RecordName'.
%% @end

-module(hello_service).
-export([behaviour_info/1]).
-export([init/0, register/1, register/2, unregister/1, lookup/1, handle_request/2]).
-export([object_to_record/5, record_to_object/4]).

-compile({no_auto_import, [register/1, register/2, unregister/1]}).

-include("hello.hrl").
-include("internal.hrl").
-define(SERVICE_TABLE, hello_services).
-record(service, {name, module}).

behaviour_info(callbacks) -> [{handle_request,3}, {method_info,0}, {param_info,1}];
behaviour_info(_Other)    -> undefined.

init() ->
    ets:new(?SERVICE_TABLE, [set, public, named_table, {keypos, #service.name}]).

lookup(Service) ->
    case ets:lookup(?SERVICE_TABLE, Service) of
        []                          -> {error, not_found};
        [#service{module = Module}] -> {ok, Module}
    end.

register(Mods) when is_list(Mods) ->
    lists:map(fun ({Name, Module}) -> ?MODULE:register(Name, Module) end, Mods).
register(Name, Module) when is_atom(Name) ->
    ?MODULE:register(atom_to_list(Name), Module);
register(Name, Module) when is_list(Name) and is_atom(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} -> ets:insert(?SERVICE_TABLE, #service{name = Name, module = Module});
        Error            -> Error
    end.

unregister([]) -> ok;
unregister(Name) when is_atom(Name) ->
    ?MODULE:unregister(atom_to_list(Name));
unregister(List) when is_list(List) and is_atom(hd(List)) ->
    lists:map(fun ?MODULE:unregister/1, List);
unregister(List) when is_list(List) and is_list(hd(List)) ->
    lists:map(fun ?MODULE:unregister/1, List);
unregister(Name) when is_list(Name) ->
    ets:delete(?SERVICE_TABLE, Name).

handle_request(CallbackModule, BatchReq) when is_list(BatchReq) ->
    pmap(fun (Req) -> handle_request_m(CallbackModule, Req) end, BatchReq);
handle_request(CallbackModule, Req) ->
    handle_request_m(CallbackModule, Req).

handle_request_m(Mod, Req) ->
    case Req#request.id of
        undefined ->
            spawn(fun () -> do_handle_request(Mod, Req) end),
            empty_response;
        _ID ->
            do_handle_request(Mod, Req)
    end.

do_handle_request(Mod, Req = #request{method = MethodName, params = Params}) ->
    case find_method(Mod, MethodName) of
        undefined -> hello_proto:std_error(Req, method_not_found);
        Method    ->
            case validate_params(Mod, Method, Params) of
                {ok, Validated} -> run_request(Req, Mod, Method, Validated);
                {error, Msg}    -> hello_proto:std_error(Req, {invalid_params, Msg})
            end
    end.

run_request(Req, Mod, Method, ValidatedParams) ->
    try Mod:handle_request(Req, Method#rpc_method.name, ValidatedParams) of
        {ok, Result} ->
            hello_proto:response(Req, Result);
        {error, Message} ->
            hello_proto:error_response(Req, 0, Message);
        {error, Code, Message} ->
            hello_proto:error_response(Req, Code, Message);
        _ ->
            hello_proto:std_error(Req, server_error)
    catch
         Type:Error ->
            error_logger:error_msg("Error (~p) thrown by a JSON-RPC handler function while executing the method ~s~n"
                                   "Parameters: ~p~nReason: ~p~nTrace:~n~p~n",
                                   [Type, Req#request.method, Req#request.params, Error, erlang:get_stacktrace()]),
            hello_proto:std_error(Req, server_error)
    end.

find_method(Mod, MethodName) when is_binary(MethodName) ->
    find_method(Mod, binary_to_list(MethodName));
find_method(Mod, MethodName) when is_list(MethodName) ->
    case catch list_to_existing_atom(MethodName) of
        {'EXIT', {badarg, _TR}} -> undefined;
        Atom                    -> find_method(Mod, Atom)
    end;
find_method(Mod, MethodName) when is_atom(MethodName) ->
    case lists:keyfind(MethodName, #rpc_method.name, Mod:method_info()) of
        false  -> undefined;
        Method -> Method
    end.

validate_params(Mod, #rpc_method{name = Method, params_as = WantParamEncoding}, ParamsIn) ->
    PInfo = Mod:param_info(Method),
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

validate_field(Info = #rpc_param{name = PNameAtom}, Param) ->
    PName = atom_to_list(PNameAtom),
    Value = case proplists:get_value(PName, Param) of
                Undef when (Undef =:= undefined) or (Undef =:= null) ->
                    if Info#rpc_param.optional -> Info#rpc_param.default;
                       true -> throw({invalid, "required parameter '" ++ PName ++ "' is missing"})
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
            case has_type(PType, GivenValue) of
                true -> GivenValue;
                false -> throw({invalid, "invalid parameter type for param '" ++ PName
                                          ++ "': expected " ++ atom_to_list(PType)})
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
            Choices = string:join(lists:map(fun (P) -> "\"" ++ atom_to_list(P) ++ "\"" end, Enum), ", "),
            throw({invalid, "parameter '" ++ Param ++ "' must be one of: " ++ Choices})
    end.

has_type(boolean, Val) when (Val == true) or (Val == false) -> true;
has_type(object, {_}) -> true;
has_type(integer, Val) when is_integer(Val) -> true;
has_type(float, Val) when is_float(Val) -> true;
has_type(number, Val) when is_number(Val) -> true;
has_type(string, Val) when is_binary(Val) -> true;
has_type(list, Val) when is_list(Val) -> true;
has_type(array, Val) when is_list(Val) -> true;
has_type(any, _Val) -> true;
has_type(_T, _Val) -> false.

params_to_proplist(_PInfo, {Props}) -> Props;
params_to_proplist(PInfo,  Params) when is_list(Params) ->
    Names = lists:map(fun (P) -> atom_to_list(P#rpc_param.name) end, PInfo),
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

pmap(F, List) ->
    [wait_result(Worker) || Worker <- [spawn_worker(self(),F,E) || E <- List]].
spawn_worker(Parent, F, E) ->
    erlang:spawn_monitor(fun() -> Parent ! {self(), F(E)} end).
wait_result({Pid,Ref}) ->
    receive
        {'DOWN', Ref, _, _, normal} -> receive {Pid,Result} -> Result end;
        {'DOWN', Ref, _, _, Reason} -> exit(Reason)
    end.

%% @hidden
object_to_record(RecName, RecAttrs, RecSize, Templ, {Props}) when is_list(Props) ->
    try
        (not is_tuple(Templ))          andalso throw(bad_defaults),
        (element(1, Templ) /= RecName) andalso throw(bad_defaults),
        (tuple_size(Templ) /= RecSize) andalso throw(bad_defaults),

        {_EndP, Rec} =
            lists:foldl(fun (Attr, {Posn, TheRec}) ->
                            case proplists:get_value(atom_to_list(Attr), Props) of
                                undefined -> {Posn + 1, TheRec};
                                null      -> {Posn + 1, setelement(Posn, TheRec, undefined)};
                                Value     -> {Posn + 1, setelement(Posn, TheRec, Value)}
                            end
                        end, {2, Templ}, RecAttrs),
        {ok, Rec}
    catch
        throw:Err -> {error, Err}
    end;
object_to_record(_RecName, _RecAttrs, _RecSize, _Defaults, _NonObject) ->
    {error, not_object}.

%% @hidden
record_to_object(RecName, RecAttrs, RecSize, Tuple) when is_tuple(Tuple) ->
    (element(1, Tuple) /= RecName) andalso exit(badarg),
    (tuple_size(Tuple) /= RecSize) andalso exit(badarg),
    {_EndP, ObjProps} =
    lists:foldl(fun (Attr, {Posn, Proplis}) ->
                    {Posn + 1, [{atom_to_list(Attr), ensure_json_compat(element(Posn, Tuple))} | Proplis]}
                end, {2, []}, RecAttrs),
    {ObjProps};
record_to_object(_RecNam, _RecAttr, _RecSiz, _Tuple) ->
    exit(badarg).

ensure_json_compat(undefined)           -> null;
ensure_json_compat(null)                -> null;
ensure_json_compat(true)                -> true;
ensure_json_compat(false)               -> false;
ensure_json_compat(A) when is_atom(A)   -> atom_to_binary(A, utf8);
ensure_json_compat(B) when is_binary(B) -> B;
ensure_json_compat(N) when is_number(N) -> N;
ensure_json_compat(L) when is_list(L) ->
    lists:map(fun ensure_json_compat/1, L);
ensure_json_compat({Props}) when is_list(Props) ->
    {lists:map(fun ({K, V}) -> {K, ensure_json_compat(V)} end, Props)};
ensure_json_compat(_Val) ->
    exit(json_incompatible).
