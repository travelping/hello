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

% @hidden
-module(hello_stateless_handler_example).
-behaviour(hello_stateless_handler).

-export([register_yourself/0]).
-export([handle_request/3, param_info/1, method_info/0]).
-include("hello.hrl").

register_yourself() ->
    hello:bind_stateless("zmq-tcp://127.0.0.1:5999", ?MODULE).

method_info() ->
    yang_typespec:rpc_methods(typespec()).

param_info(Method) ->
    yang_typespec:rpc_params(Method, typespec()).

handle_request(_Context, <<"echo">>, Params) ->
    case hello_validate:validate_params(typespec(), <<"echo">>, Params) of
	[{_,Str}] -> {ok, Str};
	R -> R
    end;
handle_request(_Context, <<"append">>, Params) ->
    case hello_validate:validate_params(typespec(), <<"append">>, Params) of
    	[{_,Str1}, {_,Str2}] -> {ok, <<Str1/binary, Str2/binary>>};
    	R -> R
    end;
handle_request(_Context, <<"enum_test">>, Params) ->
    case hello_validate:validate_params(typespec(), <<"enum_test">>, Params) of
	[{_,Atom}] -> {ok, Atom};
	R -> R
    end;
handle_request(_Context, <<"return_error">>, Params) ->
    case hello_validate:validate_params(typespec(), <<"return_error">>, Params) of
	[{<<"code">>, Code}, {<<"message">>, Message}] ->
	    {error, Code, Message};
	R -> R
    end.
