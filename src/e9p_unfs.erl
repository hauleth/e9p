% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(e9p_unfs).

-behaviour(e9p_fs).

% -include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/file.hrl").

-export([init/1, root/2, walk/3, stat/2, open/3, read/4, clunk/2]).

qid(Path) ->
    case file:read_file_info(Path, [{time, posix}]) of
        {ok, #file_info{type = Type, inode = Inode}} ->
            NQid = e9p:make_qid(Type, 0, Inode, {Path, []}),
            {ok, NQid};
        {error, _} = Error -> Error
    end.

qid_stat(Root, Path) ->
    case file:read_file_info(Path, [{time, posix}]) of
        {ok, #file_info{type = Type, inode = Inode} = FI} ->
            NQid = e9p:make_qid(Type, 0, Inode, {Path, []}),
            Stat = file_info_to_stat(Root, NQid, FI),
            {ok, NQid, Stat};
        {error, _} = Error -> Error
    end.

init(#{path := Path}) ->
    {ok, #{root => Path}}.

root(_AName, #{root := Root} = State) ->
    maybe
        {ok, Qid} ?= qid(Root),
        {ok, Qid, State}
    end.

walk(#{state := {Path, _}}, File, State) ->
    Next = filename:join(Path, File),
    case qid(Next) of
        {ok, NQid} -> {NQid, State};
        {error, _} -> {false, State}
    end.

stat(#{state := {Path, _}} = QID, #{root := Root} = State) ->
    case file:read_file_info(Path, [{time, posix}]) of
        {ok, FileInfo} ->
            Stat = file_info_to_stat(Root, QID, FileInfo),
            {ok, Stat, State};
        {error, Error} ->
            {error, Error, State}
    end.

open(#{state := {Path, []}} = QID, _Mode, State) ->
    QS = case e9p:is_type(QID, directory) of
             true ->
                 {ok, List} = file:list_dir(Path),
                 {dir, List};
             false ->
                 {ok, FD} = file:open(Path, [raw, read, binary]),
                 {regular, FD}
         end,
    NQID = QID#{state => {Path, QS}},
    {ok, {NQID, 0}, State}.

read(#{state := {_, {regular, FD}}} = QID, Offset, Len, State) ->
    case file:pread(FD, Offset, Len) of
        {ok, Data} -> {ok, {QID, Data}, State};
        eof -> {ok, {QID, []}, State};
        {error, Err} -> {error, Err, State}
    end;
read(#{state := {Path, {dir, List}}} = QID, _Offset, Len, #{root := Root} = State) ->
    {Remaining, Data} = readdir(Root, Path, List, Len, []),
    {ok, {QID#{state => {Path, {dir, Remaining}}}, Data}, State}.

readdir(_Root, _Path, List, 0, Acc) -> {List, Acc};
readdir(_Root, _Path, [], _Len, Acc) -> {[], Acc};
readdir(Root, Path, [Next | Rest], Len, Acc) ->
    {ok, _QID, Stat} = qid_stat(Root, filename:join(Path, Next)),
    Encoded = e9p_msg:encode_stat(Stat),
    Size = iolist_size(Encoded),
    if
        Size > Len -> [];
        true -> readdir(Root, Path, Rest, Len - Size, [Encoded | Acc])
    end.

clunk(#{state := {_Path, {refular, FD}}}, State) ->
    ok = file:close(FD),
    {ok, State};
clunk(_QID, State) ->
    {ok, State}.

file_info_to_stat(
  Root,
  #{state := {Path, _}} = QID,
  #file_info{
     size = Len,
     atime = Atime,
     mtime = Mtime,
     mode = Mode
    }) ->
    Name = if
               Root == Path -> ~"/";
               true -> filename:basename(Path)
           end,
    #{
      qid => QID,
      mode => Mode,
      atime => Atime,
      mtime => Mtime,
      length => Len,
      name => Name
     }.
