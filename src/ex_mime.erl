%% Copyright (c) 2011, Anthony Ramine <nox@dev-extend.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ex_mime).
-author('Anthony Ramine <nox@dev-extend.eu>').

-type media_type() :: {mime_type(), parameters()}.
-type mime_type() :: {Type::binary(), Subtype::binary()}.
-type parameters() :: [{Argument::binary(), Value::binary()}].

-type disposition() :: {binary(), parameters()}.

-export_type([media_type/0, mime_type/0, parameters/0]).

-export([media_type/1, disposition/1, lwsp_k/3]).


-spec media_type(binary()) -> badarg | media_type().
media_type(Bin) ->
  Cont = fun ({Type, I}) ->
               WCont = fun (NewI) -> media_type_slash(Bin, NewI, Type) end,
               lwsp_k(Bin, I, WCont) end,
  token_ci_k(Bin, 0, Cont).

media_type_slash(Bin, I, _Type) when I =:= byte_size(Bin) ->
  badarg;
media_type_slash(Bin, I, Type) ->
  case binary:at(Bin, I) of
    $/ ->
      NewCont = fun (NewI) -> media_type_subtype(Bin, NewI, Type) end,
      lwsp_k(Bin, I + 1, NewCont);
    _ ->
      badarg end.

media_type_subtype(Bin, I, Type) ->
  Cont = fun ({Subtype, NewI}) ->
               media_type_parameters(Bin, NewI, {Type, Subtype}) end,
  token_ci_k(Bin, I, Cont).

media_type_parameters(Bin, I, MimeType) ->
  parameters_k(Bin, I, fun (Parameters) -> {MimeType, Parameters} end).


-spec disposition(binary()) -> badarg | disposition().
disposition(Bin) ->
  Cont = fun ({Type, I}) ->
               WCont = fun (NewI) ->
                             disposition_parameters(Bin, NewI, Type) end,
               lwsp_k(Bin, I, WCont) end,
  token_ci_k(Bin, 0, Cont).

disposition_parameters(Bin, I, Type) ->
  parameters_k(Bin, I, fun (Parameters) -> {Type, Parameters} end).


-spec lwsp_k(binary(), non_neg_integer(), fun((non_neg_integer()) -> T)) -> T.
lwsp_k(Bin, I, Cont) ->
  lwsp_k(Bin, I, Cont, I).

lwsp_k(Bin, Start, Cont, I) when I =:= byte_size(Bin) ->
  Cont(Start);
lwsp_k(Bin, Start, Cont, I) ->
  case binary:at(Bin, I) of
    $\s -> lwsp_k(Bin, I + 1, Cont, I + 1);
    $\t -> lwsp_k(Bin, I + 1, Cont, I + 1);
    $\r -> lwsp_k_2(Bin, I, Cont, I + 1);
    _ -> Cont(Start) end.

lwsp_k_2(Bin, I, Cont) ->
  lwsp_k_2(Bin, I, Cont, I).

lwsp_k_2(Bin, Start, Cont, I) when I =:= byte_size(Bin) ->
  Cont(Start);
lwsp_k_2(Bin, Start, Cont, I) ->
  case binary:at(Bin, I) of
    $\n -> lwsp_k(Bin, Start, Cont, I + 1);
    _ -> Cont(Start) end.


parameters_k(Bin, I, Cont) ->
  parameters_k_lwsp(Bin, I, Cont, []).

parameters_k_lwsp(Bin, I, Cont, Acc) ->
  lwsp_k(Bin, I, fun (NewI) -> parameters_k_sep(Bin, NewI, Cont, Acc) end).

parameters_k_sep(Bin, I, Cont, Acc) when I =:= byte_size(Bin) ->
  Cont(Acc);
parameters_k_sep(Bin, I, Cont, Acc) ->
  case binary:at(Bin, I) of
    $; ->
      NewCont = fun (NewI) -> parameters_k_argument(Bin, NewI, Cont, Acc) end,
      lwsp_k(Bin, I + 1, NewCont);
    _ ->
      badarg end.

parameters_k_argument(Bin, I, Cont, Acc) ->
  NewCont = fun ({Arg, NewI}) ->
                  WCont = fun (WI) ->
                                parameters_k_equal(Bin, WI, Cont, Acc, Arg) end,
                  lwsp_k(Bin, NewI, WCont) end,
  token_ci_k(Bin, I, NewCont).

parameters_k_equal(Bin, I, _Cont, _Acc, _Arg) when I =:= byte_size(Bin) ->
  badarg;
parameters_k_equal(Bin, I, Cont, Acc, Arg) ->
  case binary:at(Bin, I) of
    $= ->
      NewCont = fun (NewI) ->
                      parameters_k_value(Bin, NewI, Cont, Acc, Arg) end,
      lwsp_k(Bin, I + 1, NewCont);
    _ ->
      badarg end.

parameters_k_value(Bin, I, _Cont, _Acc, _Arg) when I =:= byte_size(Bin) ->
  badarg;
parameters_k_value(Bin, I, Cont, Acc, Arg) ->
  NewCont = fun ({Value, NewI}) ->
                  parameters_k_lwsp(Bin, NewI, Cont, [{Arg, Value} | Acc]) end,
  case binary:at(Bin, I) of
    $" ->
      quoted_string_k_2(Bin, I + 1, NewCont);
    _ ->
      token_k(Bin, I, NewCont) end.

quoted_string_k_2(Bin, I, Cont) ->
  quoted_string_k_lwsp(Bin, I, Cont, I, []).

quoted_string_k_lwsp(Bin, _Start, _Cont, I, _Acc) when I =:= byte_size(Bin) ->
  badarg;
quoted_string_k_lwsp(Bin, Start, Cont, I, Acc) ->
  case binary:at(Bin, I) of
    $\r ->
      NewCont = fun (NewI) ->
                      NewAcc = [$\s, binary_part(Bin, Start, I - Start) | Acc],
                      quoted_string_k_quoted(Bin, NewI, Cont, NewI, NewAcc) end,
      lwsp_k_2(Bin, I + 1, NewCont);
    _ ->
      quoted_string_k_quoted(Bin, Start, Cont, I, Acc) end.

quoted_string_k_quoted(Bin, _Start, _Cont, I, _Acc) when I =:= byte_size(Bin) ->
  badarg;
quoted_string_k_quoted(Bin, Start, Cont, I, Acc) ->
  case binary:at(Bin, I) of
    $" ->
      IoList = lists:reverse(Acc, binary_part(Bin, Start, I - Start)),
      Cont({iolist_to_binary(IoList), I + 1});
    $\\ ->
      NewI = I + 1,
      case byte_size(Bin) of
        NewI -> badarg;
        _ ->
          NewAcc = [binary_part(Bin, Start, I - Start) | Acc],
          quoted_string_k_acc(Bin, NewI, Cont, NewI, NewAcc) end;
    C when C =/= $\r ->
      quoted_string_k_acc(Bin, Start, Cont, I, Acc);
    _ ->
      badarg end.

quoted_string_k_acc(Bin, Start, Cont, I, Acc) ->
  case binary:at(Bin, I) of
    C when C < 128 ->
      quoted_string_k_lwsp(Bin, Start, Cont, I + 1, Acc);
    _ ->
      badarg end.


token_ci_k(Bin, I, Cont) ->
  NewCont = fun ({Value, NewI}) -> Cont({binary_to_lower(Value), NewI}) end,
  token_k(Bin, I, NewCont).


token_k(Bin, I, Cont) ->
  token_k(Bin, I, Cont, I).

token_k(Bin, Start, Cont, I) when I =:= byte_size(Bin) ->
  token_k_end(Bin, Start, Cont, I);
token_k(Bin, Start, Cont, I) ->
  case is_tspecial_or_lwsp(binary:at(Bin, I)) of
    false -> token_k(Bin, Start, Cont, I + 1);
    true -> token_k_end(Bin, Start, Cont, I) end.

token_k_end(_Bin, Start, _Cont, Start) ->
  badarg;
token_k_end(Bin, Start, Cont, I) ->
  Cont({binary_part(Bin, Start, I - Start), I}).


-spec is_tspecial_or_lwsp(char()) -> boolean().
is_tspecial_or_lwsp($() -> true;
is_tspecial_or_lwsp($)) -> true;
is_tspecial_or_lwsp($<) -> true;
is_tspecial_or_lwsp($>) -> true;
is_tspecial_or_lwsp($@) -> true;
is_tspecial_or_lwsp($,) -> true;
is_tspecial_or_lwsp($;) -> true;
is_tspecial_or_lwsp($:) -> true;
is_tspecial_or_lwsp($\\) -> true;
is_tspecial_or_lwsp($") -> true;
is_tspecial_or_lwsp($/) -> true;
is_tspecial_or_lwsp($[) -> true;
is_tspecial_or_lwsp($]) -> true;
is_tspecial_or_lwsp($?) -> true;
is_tspecial_or_lwsp($=) -> true;
is_tspecial_or_lwsp($\s) -> true;
is_tspecial_or_lwsp($\t) -> true;
is_tspecial_or_lwsp($\r) -> true;
is_tspecial_or_lwsp(_C) -> false.


-spec binary_to_lower(binary()) -> binary().
binary_to_lower(Bin) ->
  binary_to_lower(Bin, 0).

binary_to_lower(Bin, I) when I =:= byte_size(Bin) ->
  Bin;
binary_to_lower(Bin, I) ->
  C = binary:at(Bin, I),
  case char_to_lower(C) of
    C -> binary_to_lower(Bin, I + 1);
    LowerC ->
      <<Head:I/binary, _, Tail/binary>> = Bin,
      LowerTail = << <<(char_to_lower(C2))>> || <<C2>> <= Tail >>,
      <<Head/binary, LowerC, LowerTail/binary>> end.

-spec char_to_lower(char()) -> char().
char_to_lower($A) -> $a;
char_to_lower($B) -> $b;
char_to_lower($C) -> $c;
char_to_lower($D) -> $d;
char_to_lower($E) -> $e;
char_to_lower($F) -> $f;
char_to_lower($G) -> $g;
char_to_lower($H) -> $h;
char_to_lower($I) -> $i;
char_to_lower($J) -> $j;
char_to_lower($K) -> $k;
char_to_lower($L) -> $l;
char_to_lower($M) -> $m;
char_to_lower($N) -> $n;
char_to_lower($O) -> $o;
char_to_lower($P) -> $p;
char_to_lower($Q) -> $q;
char_to_lower($R) -> $r;
char_to_lower($S) -> $s;
char_to_lower($T) -> $t;
char_to_lower($U) -> $u;
char_to_lower($V) -> $v;
char_to_lower($W) -> $w;
char_to_lower($X) -> $x;
char_to_lower($Y) -> $y;
char_to_lower($Z) -> $z;
char_to_lower(Ch) -> Ch.
