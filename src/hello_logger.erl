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
-module(hello_logger).
-export([open/1, close/1, log/2]).

-define(LOG_NAME, hello_request_log).

open(File) ->
    disk_log:open([{name, ?LOG_NAME}, {file, File}, {format, external}, {type, halt}]).

close(Log) ->
    disk_log:close(Log).

log(Request, Response) ->
    case erlang:whereis(?LOG_NAME) of
        undefined ->
            ok;
        _Pid ->
            Date = cowboy_clock:rfc1123(),
            RequestNew  = split_bnr(Request, <<"> ">>),
            ResponseNew = split_bnr(Response, <<"< ">>),
            Msg  = <<Date/binary, "\n", RequestNew/binary,
                     "\n", ResponseNew/binary, "\n-----------------------------\n">>,
            disk_log:blog(?LOG_NAME, Msg)
    end.

split_bnr(Body, Line) when is_binary(Body) -> splitacc(Body, Line, Line).

splitacc(<<>>    , _Line           , Acc)  -> Acc;
splitacc(<<"\n"  , T/binary>>, Line, Acc)  -> splitacc(T, Line, <<Acc/binary, "\n", Line/binary>> );
splitacc(<<H/utf8, T/binary>>, Line, Acc)  -> splitacc(T, Line, <<Acc/binary, H/utf8>> ).
