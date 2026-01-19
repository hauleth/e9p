% SPDX-FileCopyrightText: 2026 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(e9p_server).

-include("e9p_internal.hrl").

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
            % TODO: Handle connected clients in separate process
            ok = ?MODULE:loop(Sock, #{}, Handler),
            ?MODULE:accept_loop(LSock, Handler);
        {error, timeout} ->
            ?MODULE:accept_loop(LSock, Handler);
        {error, closed} ->
            ok
    end.

loop(Sock, FIDs, Handler) ->
    case e9p_transport:read(Sock) of
        {ok, Tag, Data} ->
            case handle_message(Data, FIDs, Handler) of
                {ok, Reply, RFIDs, RHandler} ->
                    e9p_transport:send(Sock, Tag, Reply),
                    ?MODULE:loop(Sock, RFIDs, RHandler);
                {error, Err, RHandler} ->
                    e9p_transport:send(Sock, Tag, error_msg(Err)),
                    ?MODULE:loop(Sock, FIDs, RHandler)
            end;
        {error, closed} ->
            ?LOG_WARNING("Connection closed"),
            ok
    end.

handle_message(#tversion{version = ~"9P2000", max_packet_size = MPS}, FIDs, Handler) ->
    % Currently only "basic" 9p2000 version is supported, without any extensions
    % like `.u` or `.L`
    {ok, #rversion{version = ~"9P2000", max_packet_size = MPS}, FIDs, Handler};

handle_message(#tflush{}, FIDs, Handler) ->
    % Currently there is no support for parallel messages, so this does simply
    % nothing
    {ok, #rflush{}, FIDs, Handler};

handle_message(#tattach{fid = FID, uname = _UName, aname = AName}, FIDs, Handler0) ->
    maybe
        {ok, QID, Handler} ?= e9p_fs:root(Handler0, AName),
        NFIDs = FIDs#{FID => QID},
        {ok, #rattach{qid = QID}, NFIDs, Handler}
    end;

handle_message(#twalk{fid = FID, new_fid = NewFID, names = Paths}, FIDs, Handler0) ->
    maybe
        {ok, QID} ?= get_qid(FIDs, FID),
        {ok, NewQID, QIDs, Handler} ?= e9p_fs:walk(Handler0, QID, Paths),
        {ok, #rwalk{qids = QIDs}, FIDs#{NewFID => NewQID}, Handler}
    end;

handle_message(#topen{fid = FID}, FIDs, Handler0) ->
    maybe
        {ok, QID} ?= get_qid(FIDs, FID),
        {ok, {NewQID, IOUnit}, Handler} ?= e9p_fs:open(Handler0, QID),
        {ok, #ropen{qid = QID, io_unit = IOUnit}, FIDs#{FID => NewQID}, Handler}
    end;
handle_message(#tcreate{fid = FID, name = Name, perm = Perm, mode = Mode}, FIDs, Handler0) ->
    maybe
        {ok, QID} ?= get_qid(FIDs, FID),
        {ok, {NewQID, IOUnit}, Handler} ?= e9p_fs:create(Handler0, QID, Name, Perm, Mode),
        {ok, #rcreate{qid = NewQID, io_unit = IOUnit}, FIDs, Handler}
    end;

handle_message(#tread{fid = FID, offset = Offset, len = Len}, FIDs, Handler0) ->
    maybe
        {ok, QID} ?= get_qid(FIDs, FID),
        {ok, Data, Handler} ?= e9p_fs:read(Handler0, QID, Offset, Len),
        {ok, #rread{data = Data}, FIDs, Handler}
    end;
handle_message(#twrite{fid = FID, offset = Offset, data = Data}, FIDs, Handler0) ->
    maybe
        {ok, QID} ?= get_qid(FIDs, FID),
        {ok, Data, Handler} ?= e9p_fs:write(Handler0, QID, Offset, Data),
        {ok, #rread{data = Data}, FIDs, Handler}
    end;

handle_message(#tclunk{fid = FID}, FIDs, Handler0) ->
    maybe
        {ok, QID} ?= get_qid(FIDs, FID),
        {ok, Handler} ?= e9p_fs:clunk(Handler0, QID),
        NFIDs = maps:remove(FID, FIDs),
        {ok, #rclunk{}, NFIDs, Handler}
    end;

handle_message(#tremove{fid = FID}, FIDs, Handler0) ->
    maybe
        {ok, QID} ?= get_qid(FIDs, FID),
        {ok, Handler} ?= e9p_fs:remove(Handler0, QID),
        {ok, #rremove{}, FIDs, Handler}
    end;

handle_message(#tstat{fid = FID}, FIDs, Handler0) ->
    maybe
        {ok, QID} ?= get_qid(FIDs, FID),
        {ok, Stat, Handler} ?= e9p_fs:stat(Handler0, QID),
        {ok, #rstat{stat = Stat}, FIDs, Handler}
    end;

handle_message(#twstat{fid = FID, stat = Stat}, FIDs, Handler0) ->
    maybe
        {ok, QID} ?= get_qid(FIDs, FID),
        {ok, Handler} ?= e9p_fs:wstat(Handler0, QID, Stat),
        {ok, #rwstat{}, FIDs, Handler}
    end;

handle_message(_Msg, _FIDs, Handler) ->
    {error, ~"Unknown request type", Handler}.

get_qid(FIDs, FID) ->
    case FIDs of
        #{FID := QID} -> {ok, QID};
        _ -> {error, io_lib:fwrite(~"Unknown FID: ~B", [FID])}
    end.

error_msg(Data) ->
    #rerror{msg = io_lib:fwrite(~"~p", [Data])}.
