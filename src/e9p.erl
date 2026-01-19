% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(e9p).

-export([make_qid/4, is_type/2]).

-export_type([qid/0, fid/0]).

-export_type([u8/0, u16/0, u32/0, u64/0]).

-type u8()  :: 16#00..16#FF.
-type u16() :: 16#0000..16#FFFF.
-type u32() :: 16#00000000..16#FFFFFFFF.
-type u64() :: 16#0000000000000000..16#FFFFFFFFFFFFFFFF.

-type qid() :: #{
                 type => u8(),
                 version => u16(),
                 path => u64(),
                 state => term()
                }.

-type fid() :: 16#00000000..16#FFFFFFFF.

%-spec make_qid()
make_qid(Type, Version, Path, State) ->
    #{
      type => to_qtype(Type),
      version => Version,
      path => Path,
      state => State
     }.

is_type(#{type := QType}, Type) ->
    (to_qtype(Type) band QType) =/= 0.

to_qtype(List) when is_list(List) ->
    lists:foldl(
      fun(El, Acc) when is_integer(Acc) -> to_qtype(El) bor Acc end,
      0,
      List
     );

to_qtype(directory) -> 16#80;
to_qtype(append)    -> 16#40;
to_qtype(excl)      -> 16#20;
to_qtype(device)    -> 16#10;
to_qtype(auth)      -> 16#08;
to_qtype(tmp)       -> 16#04;
to_qtype(symlink)   -> 16#02;
to_qtype(regular)   -> 16#00.
