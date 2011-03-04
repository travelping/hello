%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tpjrpc_logger).
-export([open/1, close/1, log/2]).

-define(LOG_NAME, tpjrpc_request_log).

open(File) ->
    disk_log:open([{name, ?LOG_NAME}, {file, File}, {format, external}, {type, halt}]).

close(Log) ->
    disk_log:close(Log).

log(Request, Response) when is_list(Request) ->
    Date = list_to_binary(httpd_util:rfc1123_date()),
    RequestNew = split_bnr(list_to_binary(Request), <<"> ">>),
    ResponseNew = split_bnr(Response, <<"< ">>),
    Msg  = <<Date/binary, "\n", RequestNew/binary,
             "\n", ResponseNew/binary, "\n-----------------------------\n">>,
    disk_log:blog(?LOG_NAME, Msg).

split_bnr(Body, Line) when is_binary(Body)  ->  splitacc(Body, Line, Line).

splitacc(<<>>    , _Line           , Acc)  ->  Acc;
splitacc(<<"\n"  , T/binary>>, Line, Acc)  ->  splitacc(T, Line, <<Acc/binary, "\n", Line/binary>> );
splitacc(<<H/utf8, T/binary>>, Line, Acc)  ->  splitacc(T, Line, <<Acc/binary, H/utf8>> ).
