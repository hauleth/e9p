% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-define(version, <<"9P2000">>).

-define(notag, 16#FFFF).
-define(nofid, 16#FFFFFFFF).

-define(max_packet_size, 8168).

-define(int, little-unsigned-unit:8).
-define(len, 2/?int).

%% Requests
-define(Tversion, 100).
-define(Tauth, 102).
-define(Tattach, 104).
-define(Tflush, 108).
-define(Twalk, 110).
-define(Topen, 112).
-define(Tcreate, 114).
-define(Tread, 116).
-define(Twrite, 118).
-define(Tclunk, 120).
-define(Tremove, 122).
-define(Tstat, 124).
-define(Twstat, 126).

%% Responses
-define(Rversion, 101).
-define(Rauth, 103).
-define(Rattach, 105).
-define(Rerror, 107).
-define(Rflush, 109).
-define(Rwalk, 111).
-define(Ropen, 113).
-define(Rcreate, 115).
-define(Rread, 117).
-define(Rwrite, 119).
-define(Rclunk, 121).
-define(Rremove, 123).
-define(Rstat, 125).
-define(Rwstat, 127).
