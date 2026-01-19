% SPDX-FileCopyrightText: 2026 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(prop_e9p_msg).

-include("e9p_internal.hrl").
-include_lib("proper/include/proper.hrl").
% -include_lib("stdlib/include/assert.hrl").

afid() -> integer(16#0000, 16#FFFF).

prop_can_decode_encoded_tversion() ->
    ?FORALL({Version, MPS}, {binary(), afid()},
            begin
                enc_dec(#tversion{version = Version, max_packet_size = MPS})
            end).

prop_can_decode_encoded_rversion() ->
    ?FORALL({Version, MPS}, {binary(), afid()},
            begin
                enc_dec(#rversion{version = Version, max_packet_size = MPS})
            end).

prop_can_decode_encoded_tauth() ->
    ?FORALL({Afid, Uname, Aname}, {integer(0, 16#FFFF), binary(), binary()},
            begin
                enc_dec(#tauth{afid = Afid, uname = Uname, aname = Aname})
            end).

prop_can_decode_encoded_rerror() ->
    ?FORALL({Msg}, {binary()},
            begin
                enc_dec(#rerror{msg = Msg})
            end).


enc_dec(Data) ->
    Tag = 1,
    Out = e9p_msg:encode(Tag, Data),
    Encoded = iolist_to_binary(Out),
    {ok, Tag, Data} =:= e9p_msg:parse(Encoded).
