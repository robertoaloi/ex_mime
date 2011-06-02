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

-module(ex_multipart).
-author('Anthony Ramine <nox@dev-extend.eu>').

-type part() :: {http_headers(), binary()}.
-type http_headers() :: [{binary() | atom(), binary()}].

-type cont(T) :: fun((folding_result(T), T) -> T).
-type folding_result(T) :: {data, part(), fun((T) -> badarg | T)}
                         | {more, fun((binary()) -> badarg | T)}
                         | eof.

-export_type([part/0, http_headers/0, cont/1, folding_result/1]).

-export([fold_k/4]).


-spec fold_k(binary(), Boundary::binary(), cont(T), T) -> T.
fold_k(Bin, Boundary, Cont, Acc0)
       when byte_size(Bin) >= byte_size(Boundary) + 2 ->
  BoundarySize = byte_size(Boundary),
  case Bin of
    <<"--", Boundary:BoundarySize/binary, _/binary>> ->
      % Data starts with initial boundary, skip preamble parsing.
      fold_k_boundary_tail(Bin, pattern(Boundary), Cont, Acc0,
                           BoundarySize + 2);
    _ ->
      % Parse preamble.
      fold_k_boundary(Bin, pattern(Boundary), Cont, Acc0,
                      fun fold_k_preamble/6, 0) end;
fold_k(Bin, Boundary, Cont, Acc0) ->
  % Not enough data to know if the data begins with a boundary.
  More = fun (NewData) ->
               NewBin = <<Bin/binary, NewData/binary>>,
               fold_k(NewBin, Boundary, Cont, Acc0) end,
  Cont({more, More}, Acc0).

fold_k_boundary_tail(Bin, Pattern, Cont, Acc, I) when I >= byte_size(Bin) + 2 ->
  % Boundary may be followed by "--", need more data.
  More = fun (NewData) ->
               NewBin = <<Bin/binary, NewData/binary>>,
               fold_k_boundary_tail(NewBin, Pattern, Cont, Acc, I) end,
  Cont({more, More}, Acc);
fold_k_boundary_tail(Bin, Pattern, Cont, Acc, I) ->
  case binary:at(Bin, I) =:= $- andalso binary:at(Bin, I + 1) =:= $- of
    true ->
      % Boundary is followed by "--", end parsing.
      Cont(eof, Acc);
    false ->
      % No dash after boundary, proceed with unknown chars and lwsp removal.
      fold_k_boundary_eol(Bin, Pattern, Cont, Acc, I) end.

fold_k_boundary_eol(Bin, Pattern, Cont, Acc, I) ->
  case binary:match(Bin, <<"\r\n">>, [{scope, {I, byte_size(Bin) - I}}]) of
    {CrlfStart, _Length} ->
      % End of line found, remove LWSP
      WCont = fun (NewI) ->
                    fold_k_boundary_crlf(Bin, Pattern, Cont, Acc, NewI) end,
      ex_mime:lwsp_k(Bin, CrlfStart, WCont);
    nomatch ->
      % No end of line found, still in boundary, more data needed.
      More = fun (NewData) ->
                   NewBin = <<Bin/binary, NewData/binary>>,
                   NewI = byte_size(Bin) - 1,
                   fold_k_boundary_eol(NewBin, Pattern, Cont, Acc, NewI) end,
      Cont({more, More}, Acc) end.

fold_k_boundary_crlf(Bin, Pattern, Cont, Acc, I) when I + 1 >= byte_size(Bin) ->
  More = fun (NewData) ->
               NewBin = <<Bin/binary, NewData/binary>>,
               fold_k_boundary_crlf(NewBin, Pattern, Cont, Acc, I) end,
  Cont({more, More}, Acc);
fold_k_boundary_crlf(Bin, Pattern, Cont, Acc, I) ->
  case binary:at(Bin, I) =:= $\r andalso binary:at(Bin, I + 1) =:= $\n of
    true ->
      % CRLF found after boundary.
      fold_k_boundary(Bin, Pattern, Cont, Acc, fun fold_k_part/6, I + 2);
    false ->
      % Unspecified behaviour here: RFC 2046 doesn't say what to do when LWSP is
      % not followed directly by a new line. In this implementation it is
      % considered part of the boundary so EOL needs to be searched again.
      fold_k_boundary_eol(Bin, Pattern, Cont, Acc, I) end.

fold_k_boundary(Bin, Pattern, Cont, Acc, PartFun, Start) ->
  fold_k_boundary(Bin, Pattern, Cont, Acc, PartFun, Start, Start).

fold_k_boundary(Bin, Pattern = {_P, PSize}, Cont, Acc, PartFun, Start, I)
      when PSize > byte_size(Bin) - I ->
  % Boundary can't be matched because there is not enough data to parse.
  More = fun (NewData) ->
               NewBin = <<Bin/binary, NewData/binary>>,
               fold_k_boundary(NewBin, Pattern, Cont, Acc, PartFun,
                               Start, I) end,
  Cont({more, More}, Acc);
fold_k_boundary(Bin, Pattern = {P, PSize}, Cont, Acc, PartFun, Start, I) ->
  % Try to find boundary in the current data.
  case binary:match(Bin, P, [{scope, {I, byte_size(Bin) - I}}]) of
    {BoundaryStart, _Length} ->
      % Boundary found, proceed with part parsing.
      Part = binary_part(Bin, Start, BoundaryStart - Start),
      PartFun(Bin, Pattern, Cont, Acc, BoundaryStart + PSize, Part);
    nomatch ->
      % Boundary not found, need more data.
      More = fun (NewData) ->
                   NewBin = <<Bin/binary, NewData/binary>>,
                   fold_k_boundary(NewBin, Pattern, Cont, Acc, PartFun,
                                   Start, byte_size(Bin) - PSize + 1) end,
      Cont({more, More}, Acc) end.

fold_k_preamble(Bin, Pattern, Cont, Acc, I, _Preamble) ->
  % Preamble is just thrown away.
  fold_k_boundary_tail(Bin, Pattern, Cont, Acc, I).

fold_k_part(Bin, Pattern, Cont, Acc, I, PartBin) ->
  case decode_part(PartBin) of
    badarg ->
      fold_k_boundary_tail(Bin, Pattern, Cont, Acc, I);
    Part ->
      Fold = fun (NewAcc) ->
                   fold_k_boundary_tail(Bin, Pattern, Cont, NewAcc, I) end,
      Cont({data, Part, Fold}, Acc) end.


-spec pattern(binary()) -> {binary:cp(), non_neg_integer()}.
pattern(Boundary) ->
  MatchPattern = <<"\r\n--", Boundary/binary>>,
  {binary:compile_pattern(MatchPattern), byte_size(MatchPattern)}.


-spec decode_part(binary()) -> badarg | part().
decode_part(Bin) ->
  decode_part(Bin, []).

decode_part(Bin, Acc) ->
  case erlang:decode_packet(httph_bin, Bin, []) of
    {ok, {http_header, _, Name, _, Value}, Rest} ->
      decode_part(Rest, [{Name, Value} | Acc]);
    {ok, http_eoh, Body} ->
      {Acc, Body};
    _ErrorOrMore ->
      % Malformed and incomplete parts are not valid.
      badarg end.
