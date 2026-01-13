% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(e9p_utils).

-export([normalize_path/1, qtype_from_atom/1]).

normalize_path(List) -> normalize_path(List, []).

normalize_path([], Acc) -> lists:reverse(Acc);
normalize_path([Dot | Rest], Acc)
  when Dot =:= "." orelse Dot =:= ~"."
       ->
    normalize_path(Rest, Acc);
normalize_path([P | Rest], Acc) ->
    normalize_path(Rest, [P | Acc]).

qtype_from_atom(dir)     -> 16#80;
qtype_from_atom(append)  -> 16#40;
qtype_from_atom(excl)    -> 16#20;
qtype_from_atom(device)  -> 16#10;
qtype_from_atom(auth)    -> 16#08;
qtype_from_atom(tmp)     -> 16#04;
qtype_from_atom(symlink) -> 16#02;
qtype_from_atom(regular) -> 16#00.
