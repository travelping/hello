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
    ets:new(?SERVICE_TABLE, [set, public, named_table, {keypos, #service.name}, {read_concurrency, true}]).

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

handle_request(ServiceName, Req = #request{method = MethodName, params = Params}) ->
    {ok, Mod} = lookup(ServiceName),
    case find_method(Mod, MethodName) of
        undefined -> tpjrpc_proto:std_error(Req, method_not_found);
        Method    ->
            case validate_params(Mod, Method, Params) of
                {ok, Validated} -> run_request(Req, Mod, Method, Validated);
                {error, Msg}    -> tpjrpc_proto:std_error(Req, {param_error, Msg})
            end
    end.

run_request(Req, Mod, Method, ValidatedParams) ->
    case Mod:handle_request(Req, Method#rpc_method.name, ValidatedParams) of
        {ok, Result} ->
            tpjrpc_proto:response(Req, Result);
        {error, Message} ->
            tpjrpc_proto:error_response(Req, 0, Message);
        {error, Code, Message} ->
            tpjrpc_proto:error_response(Req, Code, Message);
        _ ->
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
    PInfo  = Mod:param_info(Method),
    Params = params_to_proplist(PInfo, ParamsIn),
    case validate_types(PInfo, Params) of
        {ok, ValidProplist} ->
            case WantParamEncoding of
                proplist -> {ok, ValidProplist};
                list     -> {ok, lists:map(fun ({_K, V}) -> V end, ValidProplist)}
            end;
        {error, Msg} ->
            {error, Msg}
    end.

validate_types(PInfo, Params) -> {ok, Params}.

params_to_proplist(_PInfo, {obj, Props}) -> Props;
params_to_proplist(PInfo,  Params) when is_list(Params) ->
    Names = lists:map(fun (P) -> atom_to_list(P#rpc_param.name) end, PInfo),
    zip(Names, Params, []).

zip([], [], Result) -> lists:reverse(Result);
zip([], _2, Result) -> zip([], [], Result);
zip(_1, [], Result) -> zip([], [], Result);
zip([H1|R1], [H2|R2], Result) ->
    zip(R1, R2, [{H1, H2}|Result]).
