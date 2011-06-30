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

-module(hello_app).

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    hello_service:init(),

    RequestLog = case application:get_env(hello, request_log_enabled) of
                     {ok, true} ->
                        {ok, RequestLogFile} = application:get_env(hello, request_log_file),
                        {ok, Log} = hello_logger:open(RequestLogFile),
                        Log;
                     {ok, false} ->
                        undefined
                 end,
    {ok, Super} = hello_sup:start_link(),
    {ok, Super, RequestLog}.

stop(undefined) ->
    ok;
stop(Log) ->
    hello_logger:close(Log),
    ok.
