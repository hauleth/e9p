% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(e9p_utils).

-export([normalize_path/1]).

normalize_path(List) -> normalize_path(List, []).

normalize_path([], Acc) -> lists:reverse(Acc);
normalize_path([Dot | Rest], Acc)
  when Dot =:= "." orelse Dot =:= ~"."
       ->
    normalize_path(Rest, Acc);
normalize_path([P | Rest], Acc) ->
    normalize_path(Rest, [P | Acc]).
