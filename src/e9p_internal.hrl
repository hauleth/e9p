% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-define(version, ~"9P2000").

-define(notag, 16#FFFF).
-define(nofid, 16#FFFFFFFF).

-define(max_packet_size, 8168).

-define(int, little-unsigned-unit:8).
-define(len, 2/?int).

-define(Tversion, 100).
-define(Rversion, 101).

-define(Tauth, 102).
-define(Rauth, 103).

-define(Tattach, 104).
-define(Rattach, 105).

-define(Rerror, 107).

-define(Tflush, 108).
-define(Rflush, 109).

-define(Twalk, 110).
-define(Rwalk, 111).

-define(Topen, 112).
-define(Ropen, 113).

-define(Tcreate, 114).
-define(Rcreate, 115).

-define(Tread, 116).
-define(Rread, 117).

-define(Twrite, 118).
-define(Rwrite, 119).

-define(Tclunk, 120).
-define(Rclunk, 121).

-define(Tremove, 122).
-define(Rremove, 123).

-define(Tstat, 124).
-define(Rstat, 125).

-define(Twstat, 126).
-define(Rwstat, 127).

-record(tversion, {max_packet_size, version}).
-record(rversion, {max_packet_size, version}).

-record(tauth, {afid, uname, aname}).
-record(rauth, {aqid}).

-record(rerror, {msg}).

-record(tflush, {tag}).
-record(rflush, {}).

-record(tattach, {fid, afid, uname, aname}).
-record(rattach, {qid}).

-record(twalk, {fid, new_fid, names}).
-record(rwalk, {qids}).

-record(topen, {fid, mode}).
-record(ropen, {qid, io_unit}).

-record(tcreate, {fid, name, perm, mode}).
-record(rcreate, {qid, io_unit}).

-record(tread, {fid, offset, len}).
-record(rread, {data}).

-record(twrite, {fid, offset, data}).
-record(rwrite, {len}).

-record(tclunk, {fid}).
-record(rclunk, {}).

-record(tremove, {fid}).
-record(rremove, {}).

-record(tstat, {fid}).
-record(rstat, {stat}).

-record(twstat, {fid, stat}).
-record(rwstat, {}).
