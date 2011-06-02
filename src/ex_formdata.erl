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

-module(ex_formdata).
-author('Anthony Ramine <nox@dev-extend.eu>').

-type data() :: {Name::binary(), [ex_mime:parameter()],
                 [ex_multipart:http_header()], Body::binary()}.
-type cont(T) :: fun((folding_result(T), T) -> T).
-type folding_result(T) :: {data, data(), fun((T) -> T)}
                         | {more, fun((binary()) -> T)}
                         | {prologue, binary()}.

-export_type([data/0, cont/1, folding_result/1]).

-export([fold_k/4]).


-spec fold_k(binary(), Boundary::binary(), cont(T), T) -> T.
fold_k(Bin, Boundary, Cont, Acc0) ->
  NewCont = fun (Value, Acc) -> fold_k_part(Value, Acc, Cont) end,
  ex_multipart:fold_k(Bin, Boundary, NewCont, Acc0).

fold_k_part({data, {Headers, Body}, Fold}, Acc, Cont) ->
  case lists:keyfind(<<"Content-Disposition">>, 1, Headers) of
    {<<"Content-Disposition">>, Value} ->
      case ex_mime:disposition(Value) of
        {<<"form-data">>, Params} ->
          case lists:keyfind(<<"name">>, 1, Params) of
            {<<"name">>, Name} ->
              Cont({data, {Name, Params, Headers, Body}, Fold}, Acc);
            false ->
              Fold(Acc) end;
        _ErrorOrOtherDisposition ->
          Fold(Acc) end;
    false ->
      Fold(Acc) end;
fold_k_part(More, Acc, Cont) ->
  Cont(More, Acc).
