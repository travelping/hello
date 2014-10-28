% Copyright (c) 2012 by Travelping GmbH <info@travelping.com>

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
-module(hello_proto).

-export([init_client/2]).
-export([do_request/5, proceed_request/6, do_async_request/4]).
-export([new_request/3, build_request/2, build_response/2]).
-export([encode/1, generate_request_id/1, decode/2, encoding_info/1, encoding_protocol/1]).
-export([extract_requests/1, handle_response/2, error_response/4, log/5]).

-export([behaviour_info/1]).

-include("internal.hrl").

behaviour_info(callbacks) ->
    [{init_client, 1},
     {generate_request_id, 1},
     {new_request, 2},
     {do_request, 5},
     {do_async_request, 4},
     {encoding_info, 0},
     {encode, 1},
     {decode, 1},
     {extract_requests, 1},
     {error_response, 4},
     {log, 4}
     ];
behaviour_info(_Other) ->
    undefined.

%% ----------------------------------------------------------------------------------------------------
%% -- Request/Response handling
init_client(ProtocolMod, ProtocolOpts) ->
    ProtocolMod:init_client(ProtocolOpts).

%% ----------------------------------------------------------------------------------------------------
%% -- Request/Response handling
do_request(HandlerMod, Mod, HandlerInfo, ReqContext, #request{proto_mod = Protocol, proto_request = ProtoRequest}) ->
    Protocol:do_request(HandlerMod, Mod, HandlerInfo, ReqContext, ProtoRequest).

proceed_request(HandlerMod, Mod, HandlerInfo, ReqContext, Method, Params) ->
    HandlerMod:proceed_request(Mod, HandlerInfo, ReqContext, Method, Params).

do_async_request(#request{proto_mod = Protocol, proto_request = ProtoRequest}, RequestInfo, ProtocolInfo, Result) ->
    Protocol:do_async_request(ProtoRequest, RequestInfo, ProtocolInfo, Result).

%% ----------------------------------------------------------------------------------------------------
%% -- Record Creation/Conversion
new_request(Call, ProtocolMod, ProtocolState) ->
    ProtocolMod:new_request(Call, ProtocolState).

build_request(ProtoRequest, ProtoMod) ->
    #request{proto_request = ProtoRequest, proto_mod = ProtoMod}.
build_response(#request{proto_mod = Protocol, context = Context}, ProtoResponse) ->
    #response{proto_mod = Protocol, proto_response = ProtoResponse, context = Context}.

%% ----------------------------------------------------------------------------------------------------
%% -- Encoding/Decoding
encode(#request{proto_mod = Mod, proto_request = ProtoRequest}) ->
    Mod:encode(ProtoRequest);
encode(#response{proto_mod = Mod, proto_response = ProtoResponse}) ->
    Mod:encode(ProtoResponse).

generate_request_id(#request{proto_mod = Mod, proto_request = ProtoRequest}) ->
    Mod:generate_request_id(ProtoRequest).

decode(hello_proto, Message) ->
    {internal, Message};
decode(Mod, Message) when is_atom(Mod) ->
    Mod:decode(Message).

encoding_info(hello_proto) ->
    list_to_binary(atom_to_list(?INTERNAL));
encoding_info(ProtocolMod) ->
    ProtocolMod:encoding_info().

encoding_protocol(?INTERNAL) ->
    hello_proto;
encoding_protocol(?JSONRPC) ->
    hello_proto_jsonrpc.

%% ----------------------------------------------------------------------------------------------------
%% -- message distribution
extract_requests(Request = #request{proto_mod = ProtocolMod, proto_request = ProtoRequest, context = Context}) ->
    {ProtocolInfo, ExtractedProtoRequests} = ProtocolMod:extract_requests(ProtoRequest),
    Requests = [ {Status, Namespace, Request#request{proto_mod = ProtocolMod, proto_request = SingleReq, context = Context}} || 
                                                                    {Status, Namespace, SingleReq}  <- ExtractedProtoRequests ],
    {ProtocolInfo, Requests}.

handle_response(_ProtocolInfo, []) ->
    undefined;
handle_response(single, [ #response{proto_mod = ProtocolMod, proto_response = ProtoResponse, context = Context} ]) ->
    #response{proto_mod = ProtocolMod, proto_response = ProtoResponse, context = Context};
handle_response(batch, Responses) ->
    #response{proto_mod = ProtocolMod, context = Context} = hd(Responses),
    ProtoResponses = [ ProtoResponse || #response{proto_response = ProtoResponse} <- Responses ],
    #response{proto_mod = ProtocolMod, proto_response = ProtoResponses, context = Context}.

%% ----------------------------------------------------------------------------------------------------
%% -- error handling
error_response(Code, Message, Data, Request = #request{proto_mod = ProtocolMod, proto_request = ProtoRequest}) ->
    ProtoErrorResponse = ProtocolMod:error_response(Code, Message, Data, ProtoRequest),
    build_response(Request, ProtoErrorResponse).

%% ----------------------------------------------------------------------------------------------------
%% -- logging
log(ProtocolMod, Binary, undefined, Mod, ExUriUrl) when is_binary(Binary) ->
    ProtocolMod:log(Binary, undefined, Mod, ExUriUrl);
log(ProtocolMod, Binary, #response{proto_mod = ProtocolMod, proto_response = ProtoResponse}, Mod, ExUriUrl) when is_binary(Binary) ->
    ProtocolMod:log(Binary, ProtoResponse, Mod, ExUriUrl);
log(ProtocolMod, #request{proto_mod = ProtocolMod, proto_request = ProtoRequest}, #response{proto_response = ProtoResponse}, Mod, ExUriUrl) ->
    ProtocolMod:log(ProtoRequest, ProtoResponse, Mod, ExUriUrl).














