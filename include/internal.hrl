%% ----------------------------------------------------------------------------------------------------
%% -- these records are used for abstraction of request and responses independent of the used protocol
-record(context, {
    connection_pid          :: pid(),
    transport               :: module(),
    transport_pid           :: pid(),
    transport_params        :: term(),
    peer                    :: term()
    }).

-record(request,  {
    proto_request           :: term(),
    proto_mod               :: module(),
    context                 :: #context{}
}).

-record(response, {
    proto_response          :: term(),
    proto_mod               :: module(),
    context                 :: #context{}
}).

-record(binding, {
    namespace               :: binary(),
    callback                :: module(),
    handler_type            :: module(),
    handler_args            :: term(),
    protocol                :: module(),
    protocol_args           :: term(),
    url                     :: term()
}).

%% ----------------------------------------------------------------------------------------------------
%% -- internal records used by hello_handler
-record(request_context, {
    req_ref                 :: reference(),
    handler_pid             :: pid(),
    protocol_info           :: term(),
    context                 :: #context{}
    }).
-record(timer, {
    idle_timer              :: term(), %% the current timer
    idle_timeout_ref        :: reference(),
    idle_timeout = infinity :: timeout(),
    stopped_because_idle = false :: boolean()
    }).

%% ----------------------------------------------------------------------------------------------------
%% -- type definitions
-type request()             :: #request{}.
-type response()            :: #response{}.
-type binding()             :: #binding{}.
-type request_context()     :: #request_context{}.
-type timer()               :: #timer{}.
-type callback()            :: module().
-type handler()             :: hello_handler.
-type protocol()            :: hello_proto_jsonrpc.
-type trans_opts()          :: [{term(), term()}].
-type handler_opts()        :: [{term(), term()}].
-type protocol_opts()       :: [{term(), term()}].
-type url_string()          :: string().

%% ----------------------------------------------------------------------------------------------------
%% -- internal definitions
-define(IDLE_TIMEOUT_MSG, '$hello_idle_timeout').
-define(INCOMING_MSG, '$hello_incoming_message').

%% will b transmitted by transport to identify encoding
-define(INTERNAL, '$hello_internal').
-define(JSONRPC, '$hello_jsonrpc').

%% for keep alive initiated by a client
-define(PING, '$PING').
-define(PONG, '$PONG').
