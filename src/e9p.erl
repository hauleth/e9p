% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(e9p).

-export([make_qid/4]).

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
      type => e9p_utils:to_qtype(Type),
      version => Version,
      path => Path,
      state => State
     }.
