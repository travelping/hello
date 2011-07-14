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
-module(hello_request_log).
-export([open/2, close/1, request/4]).

%% --------------------------------------------------------------------------------
%% -- API
-spec open(module(), pid()) -> ok | {error, term()}.
open(CallbackModule, Owner) ->
    Name         = reg_key(CallbackModule),
    {ok, LogDir} = application:get_env(hello, request_log_dir),
    File         = filename:join(LogDir, atom_to_list(CallbackModule) ++ ".log"),
    LogOptions   = [{name, Name}, {linkto, Owner}, {file, File}, {format, external}, {type, halt}],

    %% disk_log does reference counting internally, so we can open the log each time.
    %% the log will be closed when the last endpoint terminates.
    disk_log:open(LogOptions).

-spec close(module()) -> ok | {error, no_such_log}.
close(CallbackModule) ->
    disk_log:close(reg_key(CallbackModule)).

-spec request(module(), binary(), binary(), binary()) -> ok | {error, no_such_log}.
request(CallbackModule, Endpoint, Request, Response) ->
    do_log(reg_key(CallbackModule), Endpoint, Request, Response).

%% --------------------------------------------------------------------------------
%% -- helpers
reg_key(Module) ->
    {hello_request_log, Module}.

do_log(DiskLog, Endpoint, Request, Response) ->
    Date        = cowboy_clock:rfc1123(),
    RequestNew  = add_line_prefix(Request,  <<">> ">>),
    ResponseNew = add_line_prefix(Response, <<"<< ">>),
    Msg         = <<Date/binary, "\n",
                    Endpoint/binary, "\n",
                    RequestNew/binary, "\n",
                    ResponseNew/binary, "\n",
                    "-----------------------------\n">>,
    disk_log:blog(DiskLog, Msg).

add_line_prefix(Body, Prefix) when is_binary(Body) -> prefacc(Body, Prefix, Prefix).

prefacc(<<>>, _Prefix, Acc)                -> Acc;
prefacc(<<"\n", T/binary>>,   Prefix, Acc) -> prefacc(T, Prefix, <<Acc/binary, "\n", Prefix/binary>> );
prefacc(<<H/utf8, T/binary>>, Prefix, Acc) -> prefacc(T, Prefix, <<Acc/binary, H/utf8>> ).
