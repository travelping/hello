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

-module(tpjrpc_json).
-export([encode/1, decode/1]).

%% --------------------------------------------------------------------------------
%% -- Encoder
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
                <<",", R5/binary>> -> dec_object(R5, [{Key, Value} | Res]);
                <<"}", R5/binary>> -> {{[{Key, Value} | Res]}, R5};
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
