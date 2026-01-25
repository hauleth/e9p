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

fid() -> int(2).

bin_str() -> ?LET({Charlist}, {string()},
                  unicode:characters_to_binary(Charlist)).

qid_type() -> union([directory,
                     append,
                     excl,
                     device,
                     auth,
                     tmp,
                     symlink,
                     regular
                    ]).

qid() ->
    ?LET({Type, Version, Path}, {qid_type(), int(4), int(8)},
         e9p:make_qid(Type, Version, Path)).
qid(Type) ->
    ?LET({Version, Path}, {int(4), int(8)},
         e9p:make_qid(Type, Version, Path)).

prop_tversion() ->
    ?FORALL({Version, MPS}, {bin_str(), int(4)},
            enc_dec(#tversion{version = Version, max_packet_size = MPS})).
prop_rversion() ->
    ?FORALL({Version, MPS}, {bin_str(), fid()},
            enc_dec(#rversion{version = Version, max_packet_size = MPS})).

prop_tauth() ->
    ?FORALL({Afid, Uname, Aname}, {fid(), bin_str(), bin_str()},
            enc_dec(#tauth{afid = Afid,
                           uname = Uname,
                           aname = Aname})).
prop_rauth() ->
    ?FORALL({AQID}, {qid()},
            enc_dec(#rauth{aqid = AQID})).

prop_rerror() ->
    ?FORALL({Msg}, {bin_str()},
            begin
                enc_dec(#rerror{msg = Msg})
            end).

prop_tattach() ->
    ?FORALL({FID, AFID, Uname, Aname}, {fid(), fid(), bin_str(), bin_str()},
            enc_dec(#tattach{
                       fid = FID,
                       afid = AFID,
                       uname = Uname,
                       aname = Aname
                      })).
prop_rattach() ->
    ?FORALL({QID}, {qid()},
            enc_dec(#rattach{qid = QID})).

prop_twalk() ->
    ?FORALL({FID, NewFID, Names}, {fid(), fid(), list(bin_str())},
            enc_dec(#twalk{fid = FID, new_fid = NewFID, names = Names})).
prop_rwalk() ->
    ?FORALL({QIDs}, {list(qid())},
            enc_dec(#rwalk{qids = QIDs})).

prop_topen() ->
    ?FORALL({FID, Mode}, {fid(), int(1)},
            enc_dec(#topen{fid = FID, mode = Mode})).
prop_ropen() ->
    ?FORALL({QID, IOUnit}, {qid(), int(4)},
            enc_dec(#ropen{qid = QID, io_unit = IOUnit})).

prop_tcreate() ->
    ?FORALL({FID, Name, Perm, Mode}, {fid(), bin_str(), int(4), int(1)},
            enc_dec(#tcreate{fid = FID,
                             name = Name,
                             perm = Perm,
                             mode = Mode})).
prop_rcreate() ->
    ?FORALL({QID, IOUnit}, {qid(), int(4)},
            enc_dec(#rcreate{qid = QID, io_unit = IOUnit})).

prop_tremove() ->
    ?FORALL({FID}, {fid()},
            enc_dec(#tremove{fid = FID})).

prop_tclunk() ->
    ?FORALL({FID}, {fid()},
            enc_dec(#tclunk{fid = FID})).

prop_tread() ->
    ?FORALL({FID, Offset, Len}, {fid(), int(8), int(4)},
            enc_dec(#tread{fid = FID, offset = Offset, len = Len})).
prop_rread() ->
    ?FORALL({Data}, {binary()},
            enc_dec(#rread{data = Data})).

prop_twrite() ->
    ?FORALL({FID, Offset, Data}, {fid(), int(8), binary()},
            enc_dec(#twrite{fid = FID, offset = Offset, data = Data})).
prop_rwrite() ->
    ?FORALL({Len}, {int(4)},
            enc_dec(#rwrite{len = Len})).

prop_tstat() ->
    ?FORALL({FID}, {fid()},
            enc_dec(#tstat{fid = FID})).
prop_rstat() ->
    ?FORALL({QID, Type, Dev, Mode, Atime, Mtime, Len, Name, Uid, Gid, Muid},
            {qid(), int(2), int(2), int(4), int(4), int(4), int(8), bin_str(), bin_str(),
             bin_str(), bin_str()},
            begin
                enc_dec(#rstat{
                  stat = #{
                           type => Type,
                           dev => Dev,
                           qid => QID,
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

prop_tflush() ->
    ?FORALL({Tag}, {int(2)}, enc_dec(#tflush{tag = Tag})).

enc_dec(Data) ->
    Tag = 1,
    Out = e9p_msg:encode(Tag, Data),
    Encoded = iolist_to_binary(Out),
    {ok, Tag, Data} =:= e9p_msg:parse(Encoded).
