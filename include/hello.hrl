-ifndef(HELLO).
-define(HELLO, 1).

%% ----------------------------------------------------------------------------------------------------
%% -- these records are used for abstraction of request and responses independent of the used protocol
-record(context, {
    listener_id             :: term(),
    connection_pid          :: pid(),
    transport               :: module(),
    transport_pid           :: pid(),
    transport_params        :: term(),
    protocol_mod            :: protocol(),
    peer                    :: term(),
    session_id              :: term(),
    req_ref                 :: reference(),
    handler_pid             :: pid()
}).

-record(request, {
    type = sync             :: sync | async,
    id                      :: term(),
    context                 :: #context{},
    proto_data              :: term(),
    method                  :: binary(),
    args                    :: term()
}).

-record(response, {
    id                      :: term(),
    context                 :: #context{},
    proto_data              :: term(),
    response                :: term()
}).

-record(error, {
    code                    :: atom() | integer(),
    message = <<"">>        :: iodata(),
    proto_data              :: term()
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
-type context()             :: #context{}.
-type timer()               :: #timer{}.
-type signature()           :: binary().
-type callback()            :: module().
-type handler()             :: hello_handler.
-type protocol()            :: hello_proto_jsonrpc.
-type trans_opts()          :: [{term(), term()}].
-type handler_opts()        :: [{term(), term()}].
-type protocol_opts()       :: [{term(), term()}].
-type client_opts()         :: [{term(), term()}].
-type url_string()          :: string().
-type listener_ref()        :: pid() | reference().

%% ----------------------------------------------------------------------------------------------------
%% -- internal definitions
-define(IDLE_TIMEOUT_MSG, '$hello_idle_timeout').
-define(INCOMING_MSG, '$hello_incoming_message').

%% for keep alive initiated by a client
-define(PING, <<"$PING">>).
-define(PONG, <<"$PONG">>).
-define(INTERNAL_SIGNATURE, <<16#AA, 16#AA>>).

-endif.
