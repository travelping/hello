-ifndef(HELLO_LOG).
-define(HELLO_LOG, 1).

-include("hello.hrl").
-include("hello_log_ids.hrl").

-define(DEFAULT_TRACES, [{class, hello}]).
-define(DEFAULT_META(Meta, LogId),
        lists:append([[{status_code, element(2, LogId)}, {message_id, element(1, LogId)}], Meta, ?DEFAULT_TRACES])).

%% hello_handler specific log macros
-define(REQ_TRACES(Mod, HandlerId, Request, RequestStatus, Response, LogId),
        lists:append([?REQ_TRACES(Mod, HandlerId, Request, RequestStatus, LogId), [{hello_response, hello_log:format(Response)}]])).

-define(REQ_TRACES(Mod, HandlerId, Request, RequestStatus, LogId),
        ?DEFAULT_META([{hello_request, hello_log:format(Request)},
                       {hello_request_id, hello_log:get_id(Request)},
                       {hello_request_method, hello_log:get_method(Request)},
                       {hello_request_status, RequestStatus},
                       {hello_service_id, HandlerId},
                       {hello_handler_callback, Mod}], LogId)).

-define(LOG_REQUEST_async_reply(CallbackModule, HandlerId, Request, Response, LogId),
    lager:debug(?REQ_TRACES(CallbackModule, HandlerId, Request, ok, Response, LogId),
                "Hello handler with callback '~p' and service id '~p' answered async request on method(s) '~p'.",
                [CallbackModule, HandlerId, hello_log:get_method(Request)])).

-define(LOG_REQUEST_request(CallbackModule, HandlerId, Request, Response, Time, LogId),
    lager:debug(?REQ_TRACES(CallbackModule, HandlerId, Request, ok, Response, LogId),
                "Hello handler with callback '~p' and service id '~p' answered synced request on method(s) '~p' in '~w' ms.",
                [CallbackModule, HandlerId, hello_log:get_method(Request), Time])).

-define(LOG_REQUEST_request_stop(CallbackModule, HandlerId, Request, Response, Reason, Time, LogId),
    lager:info(lists:append(?REQ_TRACES(CallbackModule, HandlerId, Request, ok, Response, LogId), [{hello_error_reason, Reason}]),
               "Hello handler with callback '~p' and service id '~p' answered synced request on method(s) '~p' and stopped in '~w' ms.",
               [CallbackModule, HandlerId, hello_log:get_method(Request), Time])).

-define(LOG_REQUEST_request_no_reply(CallbackModule, HandlerId, Request, Time, LogId),
    lager:debug(?REQ_TRACES(CallbackModule, HandlerId, Request, ok, LogId),
                "Hello handler with callback '~p' and service id '~p' received async request  on method(s) '~p' in ~w ms.",
                [CallbackModule, HandlerId, hello_log:get_method(Request), Time])).

-define(LOG_REQUEST_request_stop_no_reply(CallbackModule, HandlerId, Request, Time, LogId),
    lager:debug(?REQ_TRACES(CallbackModule, HandlerId, Request, ok, LogId),
                "Hello Handler with callback '~p' and service id '~p' received async request on method(s) '~p' and stopped with reason normal in '~w' ms.",
                [CallbackModule, HandlerId, hello_log:get_method(Request), Time])).

-define(LOG_REQUEST_request_stop_no_reply(CallbackModule, HandlerId, Request, Reason, Time, LogId),
    lager:debug(lists:append(?REQ_TRACES(CallbackModule, HandlerId, Request, ok, LogId), [{hello_error_reason, Reason}]),
                "Hello handler with callback '~p' and service id '~p' received async request on method(s) '~p' and stopped in '~w' ms.",
                [CallbackModule, HandlerId, hello_log:get_method(Request), Time])).

-define(LOG_REQUEST_bad_request(CallbackModule, HandlerId, Request, Reason, LogId),
    lager:error(lists:append(?REQ_TRACES(CallbackModule, HandlerId, Request, error, LogId), [{hello_error_reason, Reason}]),
               "Hello handler with callback '~p' and service id '~p' dismissed bad request on method(s) '~p'.",
               [CallbackModule, HandlerId, hello_log:get_method(Request)])).

-define(LOG_WARNING_reason(CallbackModule, HandlerId, Msg, Args, Reason, LogId),
    lager:error(?DEFAULT_META([{hello_handler_callback, CallbackModule}, {hello_error_reason, Reason},
                              {hello_service_id, HandlerId}], LogId), Msg, Args)).

-define(LOG_DEBUG(Msg, Args, Meta, LogId), lager:debug(?DEFAULT_META(Meta, LogId), Msg, Args)).
-define(LOG_INFO(Msg, Args, Meta, LogId), lager:info(?DEFAULT_META(Meta, LogId), Msg, Args)).
-define(LOG_NOTICE(Msg, Args, Meta, LogId), lager:notice(?DEFAULT_META(Meta, LogId), Msg, Args)).
-define(LOG_WARNING(Msg, Args, Meta, LogId), lager:warning(?DEFAULT_META(Meta, LogId), Msg, Args)).
-define(LOG_ERROR(Msg, Args, Meta, LogId), lager:error(?DEFAULT_META(Meta, LogId), Msg, Args)).
-define(LOG_CRITICAL(Msg, Args, Meta, LogId), lager:critical(?DEFAULT_META(Meta, LogId), Msg, Args)).
-define(LOG_ALERT(Msg, Args, Meta, LogId), lager:alert(?DEFAULT_META(Meta, LogId), Msg, Args)).
-define(LOG_EMERGENCY(Msg, Args, Meta, LogId), lager:emergency(?DEFAULT_META(Meta, LogId), Msg, Args)).

-endif.
