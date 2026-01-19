% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(e9p_unfs).

-behaviour(e9p_fs).

-include_lib("kernel/include/file.hrl").

-export([init/1, root/2, walk/3, stat/2, open/2, read/4]).

init(#{path := Path}) ->
    {ok, #{root => Path}}.

root(_AName, #{root := Root} = State) ->
    Qid = e9p:make_qid(dir, 0, 0, Root),
    {ok, Qid, State}.

walk(#{state := Path}, File, State) ->
    Next = filename:join(Path, File),
    case file:read_file_info(Next, [{time, posix}]) of
        {ok, #file_info{type = Type, inode = Inode}} ->
            NQid = e9p:make_qid(Type, 0, Inode, {Next, []}),
            {NQid, State};

        {error, _} ->
            {false, State}
    end.

stat(#{state := {Path, []}} = QID, State) ->
    case file:read_file_info(Path, [{time, posix}]) of
        {ok, FileInfo} ->
            Stat = file_info_to_stat(QID, FileInfo),
            {ok, Stat, State};
        {error, Error} ->
            {error, Error, State}
    end.

open(_Qid, State) ->
    {error, unimplemented, State}.

read(_Qid, _Offset, _Len, State) ->
    {error, unimplemented, State}.

file_info_to_stat(
  #{state := Path} = QID,
  #file_info{
     size = Len,
     atime = Atime,
     mtime = Mtime,
     mode = Mode
    }) ->
    #{
      type => 0,
      dev => 0,
      qid => QID,
      mode => Mode,
      atime => Atime,
      mtime => Mtime,
      length => Len,
      name => filename:basename(Path),
      uid => ~"",
      gid => ~"",
      muid => ~""
     }.
