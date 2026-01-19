% SPDX-FileCopyrightText: 2026 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(e9p_msg_SUITE).

-compile(export_all).

-include("e9p_internal.hrl").

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [stat_encode_decode].

stat_encode_decode(_Conf) ->
    Stat = #{
                           type => 0,
                           dev => 0,
                           qid => e9p:make_qid(directory, 0, 0, []),
                           mode => 0,
                           atime => 0,
                           mtime => 0,
                           length => 0,
                           name => <<>>,
                           uid => <<>>,
                           gid => <<>>,
                           muid => <<>>
                          },
    <<Len:?len, Out:Len/binary>> = iolist_to_binary(e9p_msg:encode_stat(Stat)),
    Decoded = e9p_msg:parse_stat(Out),
    ?assertEqual({ok, Stat}, Decoded).

