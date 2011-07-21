% Copyright (c) 2011 by Travelping GmbH <info@travelping.com>
% Copyright (c) 2010 by Felix Lange <fjl@twurst.com>

% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.

%% @doc
%%    This module provides a JSON codec (UTF-8 only),
%%    and some utitlies to make working with JSON data a bit easier.
%%
%%    == Erlang JSON encoding ==
%%    The JSON data encoding used by this module is summarized by the table below.
%%
%%    <table border="1">
%%      <thead><tr><td><b>JSON Type</b></td><td><b>Erlang Type</b></td></tr></thead>
%%      <tbody>
%%        <tr><td>string</td><td>binary()</td></tr>
%%        <tr><td>number</td><td>integer() | float()</td></tr>
%%        <tr><td>boolean</td><td>'true' | 'false'</td></tr>
%%        <tr><td>array</td><td>list()</td></tr>
%%        <tr><td>object</td><td>{[{binary(), value()}]}</td></tr>
%%      </tbody>
%%    </table>
%%
%%    When <em>encoding</em> JSON objects, the keys may be specified as a binary,
%%    string or atom.
%%
%%    == Object-to-Record Conversion ==
%%    One particularly useful facility is the automated conversion
%%    of JSON objects to and from Erlang records. Because record definitions
%%    exist only at compile time, the conversion routines are defined as
%%    macros in the `hello.hrl' include file. They are documented below.
%%
%%    === ?record_to_json_obj(RecordName::atom(), Record::tuple()) -> value() ===
%%
%%    This macro converts a record to a JSON object. Some things to be aware of:
%%    <ul>
%%      <li>`undefined' is converted to `null'</li>
%%      <li>The values contained in the record should adhere to the {@link value()} type specification.
%%          Among other things, this means that all strings must be encoded as binaries and the only kind
%%          of tuple allowed is `{proplist()}'.<br/>
%%          If any value cannot be encoded, the conversion will exit with error `badjson'.
%%      </li>
%%      <li>The value passed in as `Record' must match the record definition of `RecordName'.
%%          If it doesn't, the conversion will exit with reason `badarg'.
%%      </li>
%%    </ul>
%%
%%    === ?json_obj_to_record(RecordName::atom(), Obj::value()) -> tuple() ===
%%
%%    This macro converts a JSON object into an Erlang record. The conversion will ignore any keys in the object that
%%    that do not have a corresponding field in the record. For missing keys the default value specified <i>in the record definition</i>
%%    will be used. Furthermore,
%%    <ul>
%%      <li>`null' is converted to `undefined'.</li>
%%      <li>The conversion will exit with error `badarg' if `Obj' is not a JSON object.</li>
%%    </ul>
%%
%%    === ?json_obj_into_record(RecordName::atom(), Defaults::tuple(), Obj::value()) -> tuple() ===
%%
%%    This macro performs the same function as <b>`?json_obj_to_record/2'</b>, except that in case of missing keys the value
%%    used is taken <i>from the `Defaults' record</i>. You might find this macro useful if you want to merge an object
%%    <i>into</i> an existing instance of the record type in question.
%% @end

-module(hello_json).
-export([encode/1, decode/1]).
-export([object_to_record/5, record_to_object/4]).
-export_type([value/0, json_string/0, json_number/0, json_boolean/0, json_array/0, json_object/0, json_null/0]).

-type value() :: json_string() | json_number() | json_boolean() | json_array() | json_object() | json_null().
-type json_string()  :: binary().
-type json_number()  :: integer() | float().
-type json_boolean() :: boolean().
-type json_array()   :: list(value()).
-type json_object()  :: {list({binary(), value()})}.
-type json_null()    :: 'null'.

%% --------------------------------------------------------------------------------
%% -- Encoder
-spec encode(value()) -> binary().

encode(Num) when is_integer(Num) -> list_to_binary(integer_to_list(Num));
encode(Num) when is_float(Num)   -> list_to_binary(float_to_list(Num));
encode(true)                     -> <<"true">>;
encode(false)                    -> <<"false">>;
encode(null)                     -> <<"null">>;
encode({Props})                  -> enc_obj(Props, <<"{">>);
encode({obj, Props})             -> enc_obj(Props, <<"{">>);
encode(Lis) when is_list(Lis)    -> enc_array(Lis, <<"[">>);
encode(AnythingElse)             -> enc_string(AnythingElse).

enc_obj([], Result) ->
    <<Result/binary, "}">>;
enc_obj([{K, V}], Result) ->
    <<Result/binary, (enc_string(K))/binary, ":", (encode(V))/binary, "}">>;
enc_obj([{K, V}, Next | Rest],  Result) ->
    NR = <<Result/binary, (enc_string(K))/binary, ":", (encode(V))/binary, ",">>,
    enc_obj([Next | Rest], NR).

enc_array([], Result) ->
    <<Result/binary, "]">>;
enc_array([Elem], Result) ->
    <<Result/binary, (encode(Elem))/binary, "]">>;
enc_array([Elem, Next | Rest],  Result) ->
    NR = <<Result/binary, (encode(Elem))/binary, ",">>,
    enc_array([Next | Rest], NR).

enc_string(Bin) when is_binary(Bin)  -> <<$", (escape(Bin))/binary, $">>;
enc_string(Lis) when is_list(Lis)    -> <<$", (escape(list_to_binary(Lis)))/binary, $">>;
enc_string(Atm) when is_atom(Atm)    -> <<$", (escape(atom_to_binary(Atm, utf8)))/binary, $">>;
enc_string(Int) when is_integer(Int) -> <<$", (list_to_binary(integer_to_list(Int)))/binary, $">>;
enc_string(Flt) when is_float(Flt)   -> <<$", (list_to_binary(io_lib:write(Flt)))/binary, $">>.

-compile({inline, escape/1}).
escape(Bin) ->
    << <<(esc_chr(Chr))/binary>> || <<Chr/utf8>> <= Bin >>.

-compile({inline, esc_chr/1}).
esc_chr($")                -> <<"\\\"">>;
esc_chr($\\)               -> <<"\\\\">>;
esc_chr(Chr) when Chr > 31 -> <<Chr/utf8>>;
esc_chr($\n)               -> <<"\\\n">>;
esc_chr($\r)               -> <<"\\\r">>;
esc_chr($\t)               -> <<"\\\t">>;
esc_chr($\b)               -> <<"\\\b">>;
esc_chr($\f)               -> <<"\\\f">>;
esc_chr(Chr)               -> <<"\\u", (pad4(list_to_binary(integer_to_list(Chr, 16))))/binary>>.

-compile({inline, pad4/1}).
pad4(<<X>>)       -> <<"000", X>>;
pad4(<<X, Y>>)    -> <<"00", X, Y>>;
pad4(<<X, Y, Z>>) -> <<"0", X, Y, Z>>;
pad4(Bin)         -> Bin.

%% --------------------------------------------------------------------------------
%% -- Decoder
-spec decode(string() | binary()) -> {ok, value(), binary()} | {error, syntax_error}.

decode(JSON) when is_list(JSON) ->
    decode1(iolist_to_binary(JSON));
decode(JSON) when is_binary(JSON) ->
    decode1(JSON);
decode(_) ->
    error(badarg).

decode1(<<Bin/binary>>) ->
    try
        {Dec, Rest} = decode2(Bin),
        {ok, Dec, Rest}
    catch
        error:syntax_error ->
            {error, syntax_error}
    end.

decode2(<<Bin/binary>>) ->
    case skipspace(Bin) of
        <<${, Text/binary>>   -> dec_object(Text, []);
        <<$[, Text/binary>>   -> dec_array(Text, []);
        <<$", Text/binary>>   -> dec_stringb(Text);
        <<D,  Text/binary>> when (D =:= $-) orelse ((D >= $0) and (D =< $9)) -> dec_number(Text, D);
        <<"true",  R/binary>> -> {true, R};
        <<"false", R/binary>> -> {false, R};
        <<"null",  R/binary>> -> {null, R};
        <<>>                  -> error(syntax_error); % eof
        <<_/binary>>          -> error(syntax_error)
    end.

-compile({inline, dec_stringb/1}).
dec_stringb(Bin) ->
    {StrLis, Rest} = dec_string(Bin, []),
    {unicode:characters_to_binary(StrLis, utf8), Rest}.

dec_string(<<Bin/binary>>, Res) ->
    case Bin of
        <<$\\, R1/binary>> ->
            case R1 of
                <<C, R2/binary>> when (C =:= $"); (C =:= $\\); (C =:= $n);
                                      (C =:= $r); (C =:= $t);  (C =:= $b); (C =:= $f) ->
                    dec_string(R2, [C | Res]);
                <<$u, A, B, C, D, R2/binary>> ->
                    dec_string(R2, [list_to_integer([A,B,C,D], 16) | Res])
            end;
        <<$", R/binary>> ->
            {lists:reverse(Res), R};
        <<C/utf8, R/binary>> ->
            dec_string(R, [C | Res]);
        <<>> ->
            error(syntax_error) % eof
    end.

-compile({inline, dec_number/2}).
dec_number(Bin, C) ->
    {NumLis, Rest} = numchars(Bin, [C]),
    case (catch list_to_integer(NumLis)) of
        {'EXIT', {badarg, _}} ->
            {list_to_float(NumLis), Rest};
        Int ->
            {Int, Rest}
    end.

-compile({inline, numchars/2}).
numchars(Bin, Res) ->
    case Bin of
        <<C, R/binary>> when ((C >= $0) and (C =< $9)) or (C =:= $.)
                          or (C =:= $e) or (C =:= $E) or (C =:= $-) or (C =:= $+) ->
            numchars(R, [C|Res]);
        Rest ->
            {lists:reverse(Res), Rest}
    end.

dec_array(<<Bin/binary>>, Res) ->
    case skipspace(Bin) of
        <<"]", R1/binary>> -> {[], R1};
        NonEmpty ->
            {Term, R2} = decode2(NonEmpty),
            case skipspace(R2) of
                <<",", R3/binary>> -> dec_array(R3, [Term | Res]);
                <<"]", R3/binary>> -> {lists:reverse([Term | Res]), R3};
                _                  -> error(syntax_error)
            end
    end.

dec_object(<<Bin/binary>>, Res) ->
    case skipspace(Bin) of
        <<"}", Rest/binary>> ->
            {{[]}, Rest};
        <<$", R1/binary>> ->
            {Key, R2} = dec_string(R1, []),
            <<":", R3/binary>> = skipspace(R2),
            {Value, R4} = decode2(R3),
            case skipspace(R4) of
                <<",", R5/binary>> -> dec_object(R5, [{iolist_to_binary(Key), Value} | Res]);
                <<"}", R5/binary>> -> {{[{iolist_to_binary(Key), Value} | Res]}, R5};
                _                  -> error(syntax_error)
            end;
        _ ->
            error(syntax_error)
    end.

skipspace(<<Bin/binary>>) ->
    case Bin of
        <<" ",  R/binary>> -> skipspace(R);
        <<"\t", R/binary>> -> skipspace(R);
        <<"\n", R/binary>> -> skipspace(R);
        <<"\r", R/binary>> -> skipspace(R);
        <<"\f", R/binary>> -> skipspace(R);
        Else -> Else
    end.

%% --------------------------------------------------------------------------------
%% -- Object to Record
%% @hidden
-spec object_to_record(atom(), [atom()], integer(), tuple(), json_object()) -> tuple().

object_to_record(RecName, RecAttrs, RecSize, Templ, {Props}) when is_list(Props) ->
    (not is_tuple(Templ))          andalso error(badarg),
    (element(1, Templ) /= RecName) andalso error(badarg),
    (tuple_size(Templ) /= RecSize) andalso error(badarg),

    {_EndP, Rec} = lists:foldl(fun (Attr, {Posn, TheRec}) ->
                                       case proplists:get_value(atom_to_list(Attr), Props) of
                                           undefined -> {Posn + 1, TheRec};
                                           null      -> {Posn + 1, setelement(Posn, TheRec, undefined)};
                                           Value     -> {Posn + 1, setelement(Posn, TheRec, Value)}
                                       end
                               end, {2, Templ}, RecAttrs),
    Rec;
object_to_record(_RecName, _RecAttrs, _RecSize, _Defaults, _NonObject) ->
    error(badarg).

%% @hidden
-spec record_to_object(atom(), [atom()], integer(), tuple()) -> json_object().

record_to_object(RecName, RecAttrs, RecSize, Tuple) when is_tuple(Tuple) ->
    (element(1, Tuple) /= RecName) andalso error(badarg),
    (tuple_size(Tuple) /= RecSize) andalso error(badarg),
    {_EndP, ObjProps} =
    lists:foldl(fun (Attr, {Posn, Proplis}) ->
                    {Posn + 1, [{atom_to_list(Attr), ensure_json_compat(element(Posn, Tuple))} | Proplis]}
                end, {2, []}, RecAttrs),
    {ObjProps};
record_to_object(_RecNam, _RecAttr, _RecSiz, _Tuple) ->
    error(badarg).

ensure_json_compat(undefined)           -> null;
ensure_json_compat(null)                -> null;
ensure_json_compat(true)                -> true;
ensure_json_compat(false)               -> false;
ensure_json_compat(A) when is_atom(A)   -> atom_to_binary(A, utf8);
ensure_json_compat(B) when is_binary(B) -> B;
ensure_json_compat(N) when is_number(N) -> N;
ensure_json_compat(L) when is_list(L) ->
    lists:map(fun ensure_json_compat/1, L);
ensure_json_compat({Props}) when is_list(Props) ->
    {lists:map(fun ({K, V}) -> {K, ensure_json_compat(V)} end, Props)};
ensure_json_compat(_Val) ->
    error(badjson).
