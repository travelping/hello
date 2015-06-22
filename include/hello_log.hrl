-ifndef(HELLO_LOG).
-define(HELLO_LOG, 1).

-include("hello.hrl").

-define(DEFAULT_TRACES, [{class, hello}]).
-define(REQ_TRACES(Mod, Method), [{class, hello}, {hello_request, api}, {hello_handler, Mod}, {hello_method, Method}]).
-define(BAD_TRACES(Mod, Method), [{class, hello}, {hello_request, error}, {hello_handler, Mod}, {hello_method, Method}]).

-define(LOG_REQUEST_async_reply(CallbackModule, HandlerPid, Request, Response),
    lager:info(?REQ_TRACES(CallbackModule, Request#request.method), 
               "async reply on ~p. ~s - ~s", 
               [HandlerPid, hello_log:fmt_request(Request), hello_log:fmt_response(Response)])).

-define(LOG_REQUEST_request(CallbackModule, HandlerPid, Request, Response, Time),
    lager:info(?REQ_TRACES(CallbackModule, Request#request.method), 
               "request on ~p. ~s - ~s (~p ms)", 
               [HandlerPid, hello_log:fmt_request(Request), hello_log:fmt_response(Response), Time])).

-define(LOG_REQUEST_request_stop(CallbackModule, HandlerPid, Request, Response, Reason, Time),
    lager:info(?REQ_TRACES(CallbackModule, Request#request.method), 
               "request on ~p. ~s - ~s (stopped with reason ~p) (~p ms)", 
               [HandlerPid, hello_log:fmt_request(Request), hello_log:fmt_response(Response), Reason, Time])).

-define(LOG_REQUEST_request_no_reply(CallbackModule, HandlerPid, Request, Time),
    lager:info(?REQ_TRACES(CallbackModule, Request#request.method),
               "request on ~p. ~s - noreply (~p ms)", 
               [HandlerPid, hello_log:fmt_request(Request), Time])).

-define(LOG_REQUEST_request_stop_no_reply(CallbackModule, HandlerPid, Request, Time),
    lager:info(?REQ_TRACES(CallbackModule, Request#request.method), 
               "request on ~p. ~s (stopped with reason normal) (~p ms)", 
               [HandlerPid, hello_log:fmt_request(Request), Time])).

-define(LOG_REQUEST_request_stop_no_reply(CallbackModule, HandlerPid, Request, Reason, Time),
    lager:info(?REQ_TRACES(CallbackModule, Request#request.method), 
               "request on ~p. ~s (stopped with reason ~p) (~p ms)", 
               [HandlerPid, hello_log:fmt_request(Request), Reason, Time])).

-define(LOG_REQUEST_bad_request(CallbackModule, HandlerPid, Request, Reason),
    lager:error(?BAD_TRACES(CallbackModule, Request#request.method), 
                "bad request on ~p. ~s - ~p", 
                [HandlerPid, hello_log:fmt_request(Request), Reason])).

-define(LOG_DEBUG(Msg, Args), lager:debug(?DEFAULT_TRACES, Msg, Args)).
-define(LOG_INFO(Msg, Args), lager:info(?DEFAULT_TRACES, Msg, Args)).
-define(LOG_NOTICE(Msg, Args), lager:notice(?DEFAULT_TRACES, Msg, Args)).
-define(LOG_WARNING(Msg, Args), lager:warning(?DEFAULT_TRACES, Msg, Args)).
-define(LOG_ERROR(Msg, Args), lager:error(?DEFAULT_TRACES, Msg, Args)).
-define(LOG_CRITICAL(Msg, Args), lager:critical(?DEFAULT_TRACES, Msg, Args)).
-define(LOG_ALERT(Msg, Args), lager:alert(?DEFAULT_TRACES, Msg, Args)).
-define(LOG_EMERGENCY(Msg, Args), lager:emergency(?DEFAULT_TRACES, Msg, Args)).

-endif.
