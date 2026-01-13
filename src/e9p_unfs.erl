% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(e9p_unfs).

-behaviour(e9p_fs).

-include_lib("kernel/include/file.hrl").

-export([init/1, root/2, walk/3, stat/2, read/4, write/4]).

init(#{path := Path}) ->
    {ok, #{root => Path}}.

root(_AName, #{root := Root} = State) ->
    Qid = e9p:make_qid(dir, 0, 0, Root),
    {ok, Qid, State}.

walk(#{state := Path}, File, State) ->
    Next = filename:join(Path, File),
    case file:read_file_info(Next, [{time, posix}]) of
        {ok, #file_info{type = Type, inode = Inode}} ->
            NQid = e9p:make_qid(Type, 0, Inode, Next),
            {NQid, State};

        {error, _} ->
            {false, State}
    end.

stat(_Qid, State) ->
    {error, unimplemented, State}.

read(_Qid, _Offset, _Len, State) ->
    {error, unimplemented, State}.

write(_Qid, _Offset, _Data, State) ->
    {error, unimplemented, State}.
