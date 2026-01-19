% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(e9p_utils).

-export([normalize_path/1, to_qtype/1]).

normalize_path(List) -> normalize_path(List, []).

normalize_path([], Acc) -> lists:reverse(Acc);
normalize_path([Dot | Rest], Acc)
  when Dot =:= "." orelse Dot =:= ~"."
       ->
    normalize_path(Rest, Acc);
normalize_path([P | Rest], Acc) ->
    normalize_path(Rest, [P | Acc]).

to_qtype(List) when is_list(List) ->
    lists:foldl(
      fun(El, Acc) when is_integer(Acc) -> to_qtype(El) bor Acc end,
      0,
      List
     );

to_qtype(dir)     -> 16#80;
to_qtype(append)  -> 16#40;
to_qtype(excl)    -> 16#20;
to_qtype(device)  -> 16#10;
to_qtype(auth)    -> 16#08;
to_qtype(tmp)     -> 16#04;
to_qtype(symlink) -> 16#02;
to_qtype(regular) -> 16#00.
