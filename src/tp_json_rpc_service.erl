%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tp_json_rpc_service).
-export([behaviour_info/1]).
-export([init/0, register/1, register/2, unregister/1, lookup/1, handle_request/2]).

-compile({no_auto_import, [register/1, register/2, unregister/1]}).
-compile(export_all).

-include("tp_json_rpc.hrl").
-include("jrpc_internal.hrl").
-define(SERVICE_TABLE, tp_json_rpc_services).
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

handle_request(ServiceName, BatchReq) when is_list(BatchReq) ->
    {ok, Mod} = lookup(ServiceName),
    pmap(fun (Req) -> handle_request_m(Mod, Req#request{service = ServiceName}) end, BatchReq);
handle_request(ServiceName, Req) ->
    {ok, Mod} = lookup(ServiceName),
    handle_request_m(Mod, Req#request{service = ServiceName}).

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
        undefined -> tpjrpc_proto:std_error(Req, method_not_found);
        Method    ->
            case validate_params(Mod, Method, Params) of
                {ok, Validated} -> run_request(Req, Mod, Method, Validated);
                {error, Msg}    -> tpjrpc_proto:std_error(Req, {invalid_params, Msg})
            end
    end.

run_request(Req, Mod, Method, ValidatedParams) ->
    try Mod:handle_request(Req, Method#rpc_method.name, ValidatedParams) of
        {ok, Result} ->
            tpjrpc_proto:response(Req, Result);
        {error, Message} ->
            tpjrpc_proto:error_response(Req, 0, Message);
        {error, Code, Message} ->
            tpjrpc_proto:error_response(Req, Code, Message);
        _ ->
            tpjrpc_proto:std_error(Req, server_error)
    catch
         Type:Error ->
            error_logger:error_msg("Error (~p) thrown by a JSON-RPC handler function while executing ~s/~s~n"
                                   "Parameters: ~p~nReason: ~p~nTrace:~n~p~n",
                                   [Type, Req#request.service, Req#request.method, Req#request.params, Error, erlang:get_stacktrace()]),
            tpjrpc_proto:std_error(Req, server_error)
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
has_type(object, {obj, _}) -> true;
has_type(integer, Val) when is_integer(Val) -> true;
has_type(float, Val) when is_float(Val) -> true;
has_type(number, Val) when is_number(Val) -> true;
has_type(string, Val) when is_binary(Val) -> true;
has_type(list, Val) when is_list(Val) -> true;
has_type(array, Val) when is_list(Val) -> true;
has_type(null, null) -> true;
has_type(_T, _Val) -> false.

params_to_proplist(_PInfo, {obj, Props}) -> Props;
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
