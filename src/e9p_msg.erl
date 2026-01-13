% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

%% @doc Protocol messages parsing and encoding.
%% @end
-module(e9p_msg).

-export([parse/1, encode/3]).

-export_type([tag/0,
              message_type/0,
              request_message_type/0,
              response_message_type/0
             ]).

-include("e9p_internal.hrl").

-type tag() :: 16#0000..16#FFFF.

-type request_message_type() ::
    tversion |
    tauth |
    tattach |
    tflush |
    twalk |
    topen |
    tcreate |
    tread |
    twrite |
    tclunk |
    tremove |
    tstat |
    twstat.

-type response_message_type() ::
    rversion |
    rauth |
    rattach |
    rerror |
    rflush |
    rwalk |
    ropen |
    rcreate |
    rread |
    rwrite |
    rclunk |
    rstat |
    rwstat.

-type message_type() :: request_message_type() | response_message_type().

parse(<<Type:1/?int, Tag:2/?int, Data/binary>>) ->
    case do_parse(Type, Data) of
        {ok, T, Parsed} ->
            {ok, Tag, T, Parsed};
        {error, Reason} ->
            {error, Reason}
    end.

%% version - negotiate protocol version
do_parse(?Tversion, <<MSize:4/?int, VSize:?len, Version:VSize/binary>>) ->
    {ok, tversion, #{max_packet_size => MSize, version => Version}};
do_parse(?Rversion, <<MSize:4/?int, VSize:?len, Version:VSize/binary>>) ->
    {ok, rversion, #{max_packet_size => MSize, version => Version}};

%% attach, auth - messages to establish a connection
do_parse(?Tauth, <<AFID:4/?int,
                   UnameLen:?len, Uname:UnameLen/binary,
                   AnameLen:?len, Aname:AnameLen/binary>>) ->
    {ok, tauth, #{afid => AFID,
                  uname => Uname,
                  aname => Aname}};
do_parse(?Rauth, <<AQID:13/binary>>) ->
    {ok, rauth, #{aqid => binary_to_qid(AQID)}};

do_parse(?Tattach, <<FID:4/?int,
                     AFID:4/?int,
                     ULen:?len, Uname:ULen/binary,
                     ALen:?len, Aname:ALen/binary>>) ->
    {ok, tattach, #{fid => FID,
                    afid => AFID,
                    uname => Uname,
                    aname => Aname}};
do_parse(?Rattach, <<QID:13/binary>>) ->
    {ok, rattach, #{qid => binary_to_qid(QID)}};

%% clunk - forget about a fid
do_parse(?Tclunk, <<FID:4/?int>>) ->
    {ok, tclunk, #{fid => FID}};
do_parse(?Rclunk, <<>>) ->
    {ok, rclunk, #{}};

%% error - return an error
do_parse(?Rerror, <<ELen:?len, Error:ELen/binary>>) ->
    {ok, rerror, #{error => Error}};

%% flush - abort a message
do_parse(?Tflush, <<Tag:2/?int>>) ->
    {ok, tflush, #{tag => Tag}};
do_parse(?Rflush, <<>>) ->
    {ok, rflush, #{}};

%% open, create - prepare a fid for I/O on an existing or new file
do_parse(?Topen, <<FID:4/?int, Mode:1/?int>>) ->
    {ok, topen, #{fid => FID, mode => Mode}};
do_parse(?Ropen, <<QID:13/binary, IOUnit:4/?int>>) ->
    {ok, ropen, #{qid => binary_to_qid(QID), io_unit => IOUnit}};

do_parse(?Tcreate, <<FID:4/?int,
                     NLen:?len, Name:NLen/binary,
                     Perm:4/?int,
                     Mode:1/?int>>) ->
    {ok, tcreate, #{fid => FID, name => Name, perm => Perm, mode => Mode}};
do_parse(?Rcreate, <<QID:13/binary, IOUnit:4/?int>>) ->
    {ok, rcreate, #{qid => binary_to_qid(QID), io_unit => IOUnit}};

%% remove - remove a file from a server
do_parse(?Tremove, <<FID:4/?int>>) ->
    {ok, tremove, #{fid => FID}};
do_parse(?Rremove, <<>>) ->
    {ok, rremove, #{}};

%% stat, wstat - inquire or change file attributes
do_parse(?Tstat, <<FID:4/?int>>) ->
    {ok, tstat, #{fid => FID}};
do_parse(?Rstat, <<DLen:?len, Data:DLen/binary>>) ->
    case parse_stat(Data) of
        {ok, Stat} ->
            {ok, rstat, #{stat => Stat}};

        {error, _} = Error ->
            Error
    end;

do_parse(?Twstat, <<FID:4/?int, DLen:?len, Data:DLen/binary>>) ->
    case parse_stat(Data) of
        {ok, Stat} ->
            {ok, twstat, #{fid => FID, stat => Stat}};

        {error, _} = Error ->
            Error
    end;
do_parse(?Rwstat, <<>>) ->
    {ok, rwstat, #{}};

%% walk - descend a directory hierarchy
do_parse(?Twalk, <<FID:4/?int, NewFID:4/?int, NWNLen:?len, Rest/binary>>) ->
    NWNames = [Name || <<NLen:?len, Name:NLen/binary>> <= Rest],
    Len = length(NWNames),
    if
        Len == NWNLen ->
            {ok, twalk, #{fid => FID, new_fid => NewFID, names => NWNames}};
        true ->
            {error, {invalid_walk_length, NWNLen, Len}}
    end;
do_parse(?Rwalk, <<NWQLen:?len, QIDs:(NWQLen * 13)/binary>>) ->
    {ok, rwalk, #{qids => [binary_to_qid(QID) || <<QID:13/binary>> <= QIDs]}};

do_parse(?Tread, <<FID:4/?int, Offset:8/?int, Count:4/?int>>) ->
    {ok, tread, #{fid => FID, offset => Offset, count => Count}};
do_parse(?Rread, <<Count:4/?int, Data:Count/?int>>) ->
    {ok, rread, #{data => Data}};

do_parse(Type, Data) ->
    {error, {invalid_message, Type, Data}}.

parse_stat(<<_Size:2/?int,
             Type:2/?int,
             Dev:4/?int,
             QID:13/binary,
             Mode:4/?int,
             Atime:4/?int,
             Mtime:4/?int,
             Len:8/?int,
             NLen:?len, Name:NLen/binary,
             ULen:?len, Uid:ULen/binary,
             GLen:?len, Gid:GLen/binary,
             MULen:?len, MUid:MULen/binary>>)
->
    {ok, #{
           type => Type,
           dev => Dev,
           qid => binary_to_qid(QID),
           mode => Mode,
           atime => calendar:system_time_to_universal_time(Atime, seconds),
           mtime => calendar:system_time_to_universal_time(Mtime, seconds),
           length => Len,
           name => Name,
           uid => Uid,
           gid => Gid,
           muid => MUid
          }};
parse_stat(_) -> {error, invalid_stat_data}.

-spec encode(Tag :: tag() | notag, Type :: message_type(), Data :: map()) -> iodata().
encode(Tag, Type, Data) ->
    {MT, Encoded} = do_encode(Type, Data),
    Tag0 = case Tag of
               notag -> ?notag;
               V -> V
           end,
    [<<MT:1/?int, Tag0:2/?int>> | Encoded].

do_encode(tversion, #{max_packet_size := MSize, version := Version}) ->
    {?Tversion, [<<MSize:4/?int>> | encode_str(Version)]};
do_encode(rversion, #{max_packet_size := MSize, version := Version}) ->
    {?Rversion, [<<MSize:4/?int>> | encode_str(Version)]};

do_encode(tauth, #{afid := AFID, uname := Uname, aname := Aname}) ->
    {?Tauth, [<<AFID:4/?int>>, encode_str(Uname), encode_str(Aname)]};
do_encode(rauth, #{aqid := AQID}) ->
    {?Rauth, qid_to_binary(AQID)};

do_encode(tattach, #{fid := FID, afid := AFID, uname := Uname, aname := Aname}) ->
    {?Tattach, [<<FID:4/?int, AFID:4/?int>>, encode_str(Uname), encode_str(Aname)]};
do_encode(rattach, #{qid := QID}) ->
    {?Rattach, qid_to_binary(QID)};

do_encode(tclunk, #{fid := FID}) ->
    {?Tclunk, <<FID:4/?int>>};
do_encode(rclunk, _) ->
    {?Rclunk, []};

do_encode(rerror, #{error := Error}) ->
    {?Rerror, encode_str(Error)};

do_encode(tflush, #{tag := Tag}) ->
    {?Tflush, <<Tag:2/?int>>};
do_encode(rflush, _) ->
    {?Rflush, []};

do_encode(topen, #{fid := FID, mode := Mode}) ->
    {?Topen, <<FID:4/?int, Mode:1/?int>>};
do_encode(ropen, #{qid := QID, io_unit := IOUnit}) ->
    {?Ropen, [qid_to_binary(QID), <<IOUnit:4/?int>>]};

do_encode(tcreate, #{fid := FID, name := Name, perm := Perm, mode := Mode}) ->
    {?Tcreate, [<<FID:4/?int>>, encode_str(Name), <<Perm:4/?int, Mode:1/?int>>]};
do_encode(rcreate, #{qid := QID, io_unit := IOUnit}) ->
    {?Rcreate, [qid_to_binary(QID), <<IOUnit:4/?int>>]};

do_encode(tremove, #{fid := FID}) ->
    {?Tremove, <<FID:4/?int>>};
do_encode(rremove, _) ->
    {?Rremove, []};

do_encode(tstat, #{fid := FID}) ->
    {?Tstat, <<FID:4/?int>>};
do_encode(rstat, #{stat := Stat}) ->
    {?Rstat, encode_stat(Stat)};

do_encode(twstat, #{fid := FID, stat := Stat}) ->
    {?Twstat, [<<FID:4/?int>>, encode_stat(Stat)]};
do_encode(rwstat, _) ->
    {?Rwstat, []};

do_encode(twalk, #{fid := FID, new_fid := NewFID, names := Names}) ->
    ENames = [encode_str(Name) || Name <- Names],
    Len = length(ENames),
    {?Twalk, [<<FID:4/?int, NewFID:4/?int, Len:?len>> | ENames]};
do_encode(rwalk, #{qids := QIDs}) ->
    EQIDs = [qid_to_binary(QID) || QID <- QIDs],
    Len = length(EQIDs),
    {?Rwalk, [<<Len:?len>> | EQIDs]};

do_encode(tread, #{fid := FID, offset := Offset, count := Count}) ->
    {?Tread, <<FID:4/?int, Offset:8/?int, Count:4/?int>>};
do_encode(rread, #{data := Data}) ->
    {?Rread, encode_str(Data)}.

encode_stat(#{
           type := Type,
           dev := Dev,
           qid := QID,
           mode := Mode,
           atime := Atime,
           mtime := Mtime,
           length := Len,
           name := Name,
           uid := Uid,
           gid := Gid,
           muid := MUid
          }) ->
    Encoded = [<<
                 Type:2/?int,
                 Dev:2/?int
               >>,
               qid_to_binary(QID),
               <<Mode:4/?int>>,
               time_to_encoded_sec(Atime),
               time_to_encoded_sec(Mtime),
               <<Len:8/?int>>,
               encode_str(Name),
               encode_str(Uid),
               encode_str(Gid),
               encode_str(MUid)
              ],
    encode_str(Encoded).


%% ========== Utilities ==========

encode_str(Data) ->
    Len = iolist_size(Data),
    [<<Len:?len>> | Data].

binary_to_qid(<<Type:1/?int, Version:4/?int, Path:8/?int>>) ->
    #{type => Type, version => Version, path => Path}.

qid_to_binary(#{type := Type, version := Version, path := Path}) ->
    <<Type:1/?int, Version:4/?int, Path:8/?int>>.

time_to_encoded_sec(Time) ->
    Sec = calendar:universal_time_to_system_time(Time, [{unit, second}]),
    <<Sec:4/?int>>.
