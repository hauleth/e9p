% SPDX-FileCopyrightText: 2026 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(e9p_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/2,
         setup_acceptor/3,
         accept_loop/2,
         loop/3
        ]).

start_link(Port, Handler) ->
    proc_lib:start_link(?MODULE, setup_acceptor, [self(), Port, Handler]).

setup_acceptor(Parent, Port, Handler0) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {active, false}]),
    {ok, Handler} = e9p_fs:init(Handler0),

    proc_lib:init_ack(Parent, {ok, self()}),

    ?MODULE:accept_loop(LSock, Handler).

accept_loop(LSock, Handler) ->
    case gen_tcp:accept(LSock, 5000) of
        {ok, Sock} ->
            ok = ?MODULE:loop(Sock, #{}, Handler),
            ?MODULE:accept_loop(LSock, Handler);
        {error, timeout} ->
            ?MODULE:accept_loop(LSock, Handler);
        {error, closed} ->
            ok
    end.

loop(Sock, FIDs, Handler) ->
    case e9p_transport:read(Sock) of
        {ok, Tag, Type, Data} ->
            case handle_message(Type, Data, FIDs, Handler) of
                {ok, {RType, RData}, RFIDs, RHandler} ->
                    e9p_transport:send(Sock, Tag, RType, RData),
                    ?MODULE:loop(Sock, RFIDs, RHandler)
            end;
        {error, closed} ->
            ?LOG_WARNING("Connection closed"),
            ok
    end.

handle_message(tversion, #{version := ~"9P2000"} = Data, FIDs, Handler) ->
    {ok, {rversion, Data}, FIDs, Handler};
handle_message(tattach, Data, FIDs, Handler0) ->
    #{fid := FID, uname := _UName, aname := AName} = Data,
    {ok, QID, Handler} = e9p_fs:root(Handler0, AName),
    NFIDs = FIDs#{FID => QID},
    {ok, {rattach, #{qid => QID}}, NFIDs, Handler};
handle_message(tclunk, #{fid := FID}, FIDs, Handler) ->
    NFIDs = maps:remove(FID, FIDs),
    {ok, {rflush, #{}}, NFIDs, Handler};
handle_message(twalk, Data, FIDs, Handler0) ->
    #{
      fid := FID,
      new_fid := NewFID,
      names := Paths
     } = Data,
    #{FID := QID} = FIDs,
    {ok, NewQID, QIDs, Handler} = e9p_fs:walk(Handler0, QID, Paths),
    {ok, {rwalk, #{qids => QIDs}}, FIDs#{NewFID => NewQID}, Handler};
handle_message(topen, #{fid := FID}, FIDs, Handler0) ->
    #{FID := QID} = FIDs,
    {ok, IOUnit, Handler} = e9p_fs:open(Handler0, QID),
    {ok, {ropen, #{qid => QID, iounit => IOUnit}}, FIDs, Handler};
handle_message(tcreate, #{fid := FID}, FIDs, Handler0) ->
    #{FID := QID, name := Name, perm := Perm, mode := Mode} = FIDs,
    {ok, {NewQID, IOUnit}, Handler} = e9p_fs:create(Handler0, QID, Name, Perm, Mode),
    {ok, {rcreate, #{qid => NewQID, iounit => IOUnit}}, FIDs, Handler};
handle_message(tread, Data, FIDs, Handler0) ->
    #{
      fid := FID,
      offset := Offset,
      count := Count
     } = Data,
    #{FID := QID} = FIDs,
    {ok, Data, Handler} = e9p_fs:read(Handler0, QID, Offset, Count),
    {ok, {rread, #{data => Data}}, FIDs, Handler};
handle_message(_Type, _Data, FIDs, Handler) ->
    {ok, {rerror, #{error => ~"Unknown request type"}}, FIDs, Handler}.
