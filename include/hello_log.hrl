-ifndef(HELLO_LOG).
-define(HELLO_LOG, 1).

-include("hello.hrl").
-include("hello_log_ids.hrl").

-define(DEFAULT_TRACES, [{class, hello}]).
-define(DEFAULT_META(Meta, LogId),
        lists:append([[{status_code, element(2, LogId)}, {message_id, element(1, LogId)}], Meta, ?DEFAULT_TRACES])).

%% hello_handler specific log macros
-define(REQ_TRACES(Mod, HandlerId, Context, Request, RequestStatus, Response, LogId),
        lists:append([?REQ_TRACES(Mod, HandlerId, Context, Request, RequestStatus, LogId), [{hello_response, hello_log:format(Response)}]])).

-define(REQ_TRACES(Mod, HandlerId, Context, Request, RequestStatus, LogId),
        ?DEFAULT_META([{hello_request, hello_log:format(Request)},
                       {hello_request_id, hello_log:get_id(Request)},
                       {hello_request_peer, hello_log:format_context(peer, Context)},
                       {hello_request_method, hello_log:get_method(Request)},
                       {hello_request_status, RequestStatus},
                       {hello_service_id, HandlerId},
                       {hello_handler_callback, Mod}], LogId)).

-define(LOG_REQUEST_async_reply(CallbackModule, HandlerId, Context, Request, Response, LogId),
    lager:debug(?REQ_TRACES(CallbackModule, HandlerId, Context, Request, ok, Response, LogId),
                "~p / ~p : received request", [CallbackModule, hello_log:get_method(Request)])).

-define(LOG_REQUEST_request(CallbackModule, HandlerId, Context, Request, Response, Time, LogId),
    lager:debug(?REQ_TRACES(CallbackModule, HandlerId, Context, Request, ok, Response, LogId),
                "~p / ~p : received request [~w ms]", [CallbackModule, hello_log:get_method(Request), Time])).

-define(LOG_REQUEST_request_stop(CallbackModule, HandlerId, Context, Request, Response, Reason, Time, LogId),
    lager:info(lists:append(?REQ_TRACES(CallbackModule, HandlerId, Context, Request, ok, Response, LogId), [{hello_error_reason, Reason}]),
               "~p / ~p : received request [~w ms]", [CallbackModule, hello_log:get_method(Request), Time])).

-define(LOG_REQUEST_request_no_reply(CallbackModule, HandlerId, Context, Request, Time, LogId),
    lager:debug(?REQ_TRACES(CallbackModule, HandlerId, Context, Request, ok, LogId),
                "~p / ~p : received request [~w ms]", [CallbackModule, hello_log:get_method(Request), Time])).

-define(LOG_REQUEST_request_stop_no_reply(CallbackModule, HandlerId, Context, Request, Time, LogId),
    lager:debug(?REQ_TRACES(CallbackModule, HandlerId, Context, Request, ok, LogId),
                "~p / ~p : received request [~w ms]", [CallbackModule, hello_log:get_method(Request), Time])).

-define(LOG_REQUEST_request_stop_no_reply(CallbackModule, HandlerId, Context, Request, Reason, Time, LogId),
    lager:debug(lists:append(?REQ_TRACES(CallbackModule, HandlerId, Context, Request, ok, LogId), [{hello_error_reason, Reason}]),
                "~p / ~p : received request [~w ms]", [CallbackModule, hello_log:get_method(Request), Time])).

-define(LOG_REQUEST_bad_request(CallbackModule, HandlerId, Context, Request, Reason, LogId),
    lager:error(lists:append(?REQ_TRACES(CallbackModule, HandlerId, Context, Request, error, LogId), [{hello_error_reason, Reason}]),
                "~p / ~p : received bad request", [CallbackModule, hello_log:get_method(Request)])).

-define(LOG_WARNING_reason(CallbackModule, HandlerId, Context, Msg, Args, Reason, LogId),
    lager:error(?DEFAULT_META([{hello_handler_callback, CallbackModule},
                               {hello_error_reason, Reason},
                               {hello_request_peer, hello_log:format_context(peer, Context)},
                               {hello_service_id, HandlerId}],
                              LogId), Msg, Args)).

-define(LOG_DEBUG(Msg, Args, Meta, LogId), lager:debug(?DEFAULT_META(Meta, LogId), Msg, Args)).
-define(LOG_INFO(Msg, Args, Meta, LogId), lager:info(?DEFAULT_META(Meta, LogId), Msg, Args)).
-define(LOG_NOTICE(Msg, Args, Meta, LogId), lager:notice(?DEFAULT_META(Meta, LogId), Msg, Args)).
-define(LOG_WARNING(Msg, Args, Meta, LogId), lager:warning(?DEFAULT_META(Meta, LogId), Msg, Args)).
-define(LOG_ERROR(Msg, Args, Meta, LogId), lager:error(?DEFAULT_META(Meta, LogId), Msg, Args)).
-define(LOG_CRITICAL(Msg, Args, Meta, LogId), lager:critical(?DEFAULT_META(Meta, LogId), Msg, Args)).
-define(LOG_ALERT(Msg, Args, Meta, LogId), lager:alert(?DEFAULT_META(Meta, LogId), Msg, Args)).
-define(LOG_EMERGENCY(Msg, Args, Meta, LogId), lager:emergency(?DEFAULT_META(Meta, LogId), Msg, Args)).

-endif.
