% Copyright (c) 2010-2015 by Travelping GmbH <info@travelping.com>

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
-module(hello_log).

-export([format/1, get_id/1, get_method/1]).

-include("hello.hrl").

%% --------------------------------------------------------------------------------
%% -- Formaters for messages

%% -- request formatting
format([ Request = #request{} ]) ->
    "[ " ++ format(Request) ++ " ]";
format([ Request = #request{} | Requests]) ->
    "[ " ++ format(Request) ++ " ], " ++ format(Requests);
format(#request{id = ID, method = Method, args = Args}) ->
    lists:append(["ID: ", stringify(ID), "; METHOD: ", stringify(Method),
                    "; ARGS: ", stringify(Args)]);

%% -- response formatting; first for record responses, then for arbitrary data blobs
format([ Response = #response{} ]) ->
    "[ " ++ format(Response) ++ " ]";
format([ Response = #response{} | Responses]) ->
    "[ " ++ format(Response) ++ " ], " ++ format(Responses);
format(#response{id = ID, response = CallbackResponse}) ->
    lists:append(["ID: ", stringify(ID), "; RESPONSE: ", stringify(CallbackResponse)]);
format(ignore) -> ["ignored"];
format({ok, CallbackResponse}) -> stringify(CallbackResponse);
format(Msg) -> stringify(Msg).

%% -- get internal hello request id
get_id([ #request{id = Id} ]) ->            stringify(Id);
get_id([ #request{id = Id} | Requests]) ->  stringify(Id) ++ ", " ++ get_id(Requests);
get_id(#request{id = Id}) ->                stringify(Id);
get_id([ #response{id = Id} ]) ->           stringify(Id);
get_id([ #response{id = Id} | Responses]) ->stringify(Id) ++ ", " ++ get_id(Responses);
get_id(#response{id = Id}) ->               stringify(Id).

%% -- get request method
get_method([ #request{method = Method} ]) ->
    stringify(Method);
get_method([ #request{method = Method} | Requests]) ->
    stringify(Method) ++ ", " ++ get_method(Requests);
get_method(#request{method = Method}) ->
    stringify(Method).

stringify(Term)  ->
    lists:flatten(io_lib:format("~p", [Term])).
