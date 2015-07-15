% ---------------------------------------------------------------------------
% -- bootstrapping for binding callback modules and starting clients

%% URLs with transport parameters
-define(HTTP, {"http://127.0.0.1:6000", []}).
-define(ZMQ_TCP, {"zmq-tcp://127.0.0.1:6001", []}).
-define(ZMQ_REQ_TCP, {"zmq-tcp://127.0.0.1:6002", [{socket_type, req}]}).
-define(TRANSPORTS, [?HTTP, ?ZMQ_TCP, ?ZMQ_REQ_TCP]).

%% callbacks 
-define(STATEFUL_CALLBACKS, [handler1, handler2]).
%% handler
-define(HANDLER, [hello_handler]).
%% possible paramters for handlers
-define(STATEFUL_ARGS, [{callback_args, 0}]).
%% handler with parameters
-define(HANDLER_ARGS, [{handler1, 0}, {handler2, 1}]).
%% handler with callbacks
-define(CALLBACKS, [{hello_handler, ?STATEFUL_CALLBACKS}]).

%% protocols
-define(PROTOCOLS, [hello_proto_jsonrpc]).
%% possible protocol parameters
-define(PROTOCOL_ARGS, [{hello_proto_jsonrpc, undefined}]).

%% used callback modules for code loading
-define(CALLBACK_MODS, [handler1, handler2]).

-define(CLIENT_NAMES, [{?HTTP, http_client}, {?ZMQ_TCP, zmq_tcp_client}, {?ZMQ_REQ_TCP, zmq_req_tcp_client}]).

-define(NOTIFICATION_METHOD, <<"notification">>).
-define(NOTIFICATION_ARGS, [<<"notification_arg1">>, <<"notification_arg2">>]).
% ---------------------------------------------------------------------------
% -- bootstrapping for requests

% stateful1
-define(ARG11, <<"handler1_arg1">>).
-define(ARG12, <<"handler1_arg2">>).
-define(ARG13, <<"handler1_arg3">>).
-define(ARG14, <<"handler1_arg4">>).
-define(ARG15, <<"handler1_arg5">>).
-define(ARG16, <<"arg1">>).
-define(ARG17, <<"arg2">>).

% stateful2
-define(ARG21, <<"handler2_arg1">>).
-define(ARG22, <<"handler2_arg2">>).
-define(ARG23, <<"handler2_arg3">>).
-define(ARG24, <<"handler2_arg4">>).
-define(ARG25, <<"handler2_arg5">>).

-define(FUN11, <<"handler1.fun1">>).
-define(FUN12, <<"handler1.fun2">>).
-define(FUN13, <<"handler1.fun3">>).
-define(FUN14, <<"handler1.fun4">>).
-define(FUN15, <<"handler1.fun5">>).
-define(FUN16, <<"handler1.fun6">>).

-define(FUN21, <<"handler2.fun1">>).
-define(FUN22, <<"handler2.fun2">>).
-define(FUN23, <<"handler2.fun3">>).
-define(FUN24, <<"handler2.fun4">>).
-define(FUN25, <<"handler2.fun5">>).

-define(REQ11, {?FUN11, [?ARG11], []}).
-define(REQ12, {?FUN12, [?ARG12], []}).
-define(REQ13, {?FUN13, [?ARG13], []}).
-define(REQ14, {?FUN14, [?ARG14], []}).
-define(REQ15, {?FUN11, [?ARG11], [{notification, true}]}).
-define(REQ16, {?FUN12, [?ARG12], [{notification, true}]}).
-define(REQ17, {?FUN15, [?ARG15], []}).
-define(STATEFUL1_REQS, [?REQ11, ?REQ12, ?REQ13, ?REQ14, ?REQ15, ?REQ16, ?REQ17]).

-define(REQ21, {?FUN21, [?ARG21], []}).
-define(REQ22, {?FUN22, [?ARG22], []}).
-define(REQ23, {?FUN23, [?ARG23], []}).
-define(REQ24, {?FUN24, [?ARG24], []}).
-define(REQ25, {?FUN21, [?ARG21], [{notification, true}]}).
-define(REQ26, {?FUN22, [?ARG22], [{notification, true}]}).
-define(REQ27, {?FUN25, [?ARG25], []}).
-define(STATEFUL2_REQS, [?REQ21, ?REQ22, ?REQ23, ?REQ24, ?REQ25, ?REQ26, ?REQ27]).

% error requests
-define(DUMMY_REQ1, {<<"dummy.fun1">>, [<<"dummy_arg">>], []}).
-define(DUMMY_REQ2, {<<"handler1.dummy_fun">>, [<<"dummy_arg">>], []}).
-define(DUMMY_REQ3, {<<"handler1.fun1">>, [123], []}). %%binary is expected, not a number
-define(DUMMY_REQ4, {<<"handler1.fun1">>, [<<"args1">>, <<"arg2">>], []}). %%too many args
-define(DUMMY_BATCH1, [?DUMMY_REQ1, ?DUMMY_REQ1]).
-define(DUMMY_BATCH2, [?REQ11, ?DUMMY_REQ1]).

% ------------------------------------------------------------------------------------------
% -- bootstrapping of different request sets used for tests
-define(SIMPLE_ONE_SHOT, [?REQ11, ?REQ21]).

-define(SIMPLE_BATCH, [	[?REQ11, ?REQ12], 
						[?REQ21, ?REQ22] ]).

-define(SIMPLE_NOTIFICATION, [?REQ15, ?REQ25]).

-define(BATCH_NOTIFICATION, [[?REQ15, ?REQ16],
							 [?REQ25, ?REQ26] ]).

-define(SIMPLE_ASYNC_ONESHOT, [?REQ13, ?REQ23]).

-define(BATCH_ASYNC_ONESHOT, [[?REQ13, ?REQ14],
							  [?REQ23, ?REQ24] ]).

-define(ALL_MIXED_BATCH_ONE_CALLBACK,  [[?REQ11, ?REQ12, ?REQ13, ?REQ14, ?REQ15, ?REQ16],
										[?REQ21, ?REQ22, ?REQ23, ?REQ24, ?REQ25, ?REQ26]]).

-define(NORMAL_BATCH_ALL_CALLBACKS,  [?REQ11, ?REQ21]).

-define(ALL_MIXED_BATCH_ALL_CALLBACKS,  ?STATEFUL1_REQS ++ ?STATEFUL2_REQS ).

-define(NOTIFY_REQS, [?REQ17, ?REQ27]).

-define(NAMED_PARAMETER_REQ, {?FUN16, [{<<"arg2">>, ?ARG17}, {<<"arg1">>, ?ARG16}], []}). %% reversed to see if its working


