% SPDX-FileCopyrightText: 2026 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(prop_e9p_msg).

-include_lib("proper/include/proper.hrl").
% -include_lib("stdlib/include/assert.hrl").

prop_can_decode_encoded_tauth() ->
    ?FORALL({Uname, Aname}, {binary(), binary()},
            begin
                enc_dec(tauth, #{afid => 1, uname => Uname, aname => Aname})
            end).

enc_dec(Kind, Data) ->
    Tag = 1,
    Out = e9p_msg:encode(Tag, Kind, Data),
    Encoded = iolist_to_binary(Out),
    {ok, Tag, Kind, Data} =:= e9p_msg:parse(Encoded).
