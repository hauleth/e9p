% SPDX-FileCopyrightText: 2026 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: GPL-3.0-only

-module(prop_e9p_msg).

-include("e9p_internal.hrl").
-include_lib("proper/include/proper.hrl").
% -include_lib("stdlib/include/assert.hrl").

int(N) ->
    Min = 0,
    Max = 1 bsl (N * 8) - 1,
    integer(Min, Max).

afid() -> int(2).

bin_str() -> ?LET({Charlist}, {string()},
                  unicode:characters_to_binary(Charlist)).

prop_can_decode_encoded_tversion() ->
    ?FORALL({Version, MPS}, {bin_str(), afid()},
            begin
                enc_dec(#tversion{version = Version, max_packet_size = MPS})
            end).

prop_can_decode_encoded_rversion() ->
    ?FORALL({Version, MPS}, {bin_str(), afid()},
            begin
                enc_dec(#rversion{version = Version, max_packet_size = MPS})
            end).

prop_can_decode_encoded_tauth() ->
    ?FORALL({Afid, Uname, Aname}, {afid(), bin_str(), bin_str()},
            begin
                enc_dec(#tauth{afid = Afid, uname = Uname, aname = Aname})
            end).

prop_can_decode_encoded_rerror() ->
    ?FORALL({Msg}, {bin_str()},
            begin
                enc_dec(#rerror{msg = Msg})
            end).

prop_can_decode_encoded_rstat() ->
    ?FORALL({Type, Dev, Mode, Atime, Mtime, Len, Name, Uid, Gid, Muid},
            {int(2), int(2), int(4), int(4), int(4), int(8), bin_str(), bin_str(),
             bin_str(), bin_str()},
            begin
                enc_dec(#rstat{
                  stat = #{
                           type => Type,
                           dev => Dev,
                           qid => e9p:make_qid(directory, 0, 0, []),
                           mode => Mode,
                           atime => Atime,
                           mtime => Mtime,
                           length => Len,
                           name => Name,
                           uid => Uid,
                           gid => Gid,
                           muid => Muid
                          }})
            end).

enc_dec(Data) ->
    Tag = 1,
    Out = e9p_msg:encode(Tag, Data),
    Encoded = iolist_to_binary(Out),
    {ok, Tag, Data} =:= e9p_msg:parse(Encoded).
