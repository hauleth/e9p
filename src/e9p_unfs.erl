% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(e9p_unfs).

-moduledoc """
Expose Unix Filesystem as 9p2000 mount
""".

-behaviour(e9p_fs).

-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/file.hrl").

-export([init/1, root/3, walk/4, stat/3, open/4, read/5, clunk/2, create/6,
         write/5, remove/3, wstat/4]).

% Create QID and Stat data for given path.
qid(Root, Path) ->
    FullPath = filename:join([Root] ++ Path),
    case file:read_file_info(FullPath, [{time, posix}]) of
        {ok, #file_info{type = Type, inode = Inode} = FI} ->
            QID = e9p:make_qid(Type, 0, Inode),
            Stat = file_info_to_stat(Path, QID, FI),
            {ok, QID, Stat};
        {error, _} = Error -> Error
    end.

file_info_to_stat(
  Path,
  QID,
  #file_info{
     size = Len,
     atime = Atime,
     mtime = Mtime,
     mode = Mode
    }) ->
    Name = if
               Path == [] -> ~"/";
               true -> lists:last(Path)
           end,
    #{
      qid => QID,
      mode => Mode,
      atime => Atime,
      mtime => Mtime,
      length => Len,
      name => Name
     }.

stat_to_file_info(Stat) ->
    #{
      mode := Mode,
      atime := Atime,
      mtime := Mtime
     } = Stat,
    #file_info{
       mode = Mode,
       atime = Atime,
       mtime = Mtime
      }.

%% ====== Filesystem handlers ======

-doc false.
init(#{path := Path}) ->
    {ok, #{root => unicode:characters_to_binary(Path)}}.

-doc false.
root(UName, AName, #{root := Root} = State) ->
    ?LOG_INFO(#{uname => UName, aname => AName}),
    maybe
        {ok, Qid, _Stat} ?= qid(Root, []),
        {ok, {Qid, []}, State}
    end.

-doc false.
walk(_QID, Path, ~"..", #{root := Root} = State) ->
    case qid(Root, lists:droplast(Path)) of
        {ok, NQid, _Stat} -> {{NQid, []}, State};
        {error, _} -> {false, State}
    end;
walk(_QID, Path, File, #{root := Root} = State) ->
    case qid(Root, Path ++ [File]) of
        {ok, NQid, _Stat} -> {{NQid, []}, State};
        {error, _} -> {false, State}
    end.

-doc false.
stat({QID, _}, Path, #{root := Root} = State) ->
    FullPath = filename:join([Root] ++ Path),
    case file:read_file_info(FullPath, [{time, posix}]) of
        {ok, FileInfo} ->
            Stat = file_info_to_stat(Path, QID, FileInfo),
            {ok, Stat, State};
        {error, Error} ->
            {error, Error, State}
    end.

-doc false.
wstat(_QID, Path, Stat, #{root := Root} = State) ->
    FileInfo = stat_to_file_info(Stat),
    FullPath = filename:join([Root] ++ Path),

    case file:write_file_info(FullPath, FileInfo, [{time, posix}]) of
        ok -> {ok, State};
        {error, Reason} ->
            {error, io_lib:format("Couldn't write file stat: ~p", [Reason]),
             State}
    end.

-doc false.
open({QID, []}, Path, Mode, #{root := Root} = State) ->
    FullPath = filename:join([Root] ++ Path),
    QS = case e9p:is_type(QID, directory) of
             true ->
                 {ok, List} = file:list_dir(FullPath),
                 {dir, List};
             false ->
                 {Trunc, Opts} = translate_mode(Mode),
                 {ok, FD} = file:open(FullPath, [raw, binary | Opts]),
                 if Trunc -> file:truncate(FD); true -> ok end,
                 {regular, FD}
         end,
    {ok, {QS, 0}, State}.

-doc false.
clunk({_, {regular, FD}}, State) ->
    ok = file:close(FD),
    {ok, State};
clunk(_QID, State) ->
    {ok, State}.

-doc false.
create(_QID, _Path, _Name, _Perm, _Mode, State) ->
    {error, "Unsupported", State}.

-doc false.
remove({QID, _} = FID, Path, #{root := Root} = State0) ->
    FullPath = filename:join([Root] ++ Path),
    {ok, State} = clunk(FID, State0),
    case case e9p:is_type(QID, directory) of
             true -> file:del_dir(FullPath);
             false -> file:delete(FullPath)
         end of
        ok -> {ok, State};
        {error, Reason} ->
            {error, io_lib:format("Failed to remove path: ~p", [Reason]), State}
    end.

translate_mode([trunc | Rest]) ->
    {_, Mode} = translate_mode(Rest),
    {true, Mode};
translate_mode([read]) -> {false, [read]};
translate_mode([write]) -> {false, [read, write]};
translate_mode([append]) -> {false, [read, write]};
translate_mode([exec]) -> {false, [read]}.

-doc false.
read({_QID, {regular, FD}}, _Path, Offset, Len, State) ->
    case file:pread(FD, Offset, Len) of
        {ok, Data} -> {ok, {{regular, FD}, Data}, State};
        eof -> {ok, {{regular, FD}, []}, State};
        {error, Err} -> {error, Err, State}
    end;
read({_QID, {dir, List}}, Path, _Offset, Len, #{root := Root} = State) ->
    {Remaining, Data} = readdir(Root, Path, List, Len, []),
    {ok, {{dir, Remaining}, Data}, State}.

readdir(_Root, _Path, List, 0, Acc) -> {List, Acc};
readdir(_Root, _Path, [], _Len, Acc) -> {[], Acc};
readdir(Root, Path, [Next | Rest], Len, Acc) ->
    {ok, _QID, Stat} = qid(Root, Path ++ [Next]),
    Encoded = e9p_msg:encode_stat(Stat),
    Size = iolist_size(Encoded),
    if
        Size > Len -> [];
        true -> readdir(Root, Path, Rest, Len - Size, [Encoded | Acc])
    end.

-doc false.
write({_QID, {regular, FD}}, _Path, Offset, Data, State) ->
    case file:pwrite(FD, Offset, Data) of
        ok -> {ok, {{regular, FD}, iolist_size(Data)}, State};
        {error, Err} -> {error, io_lib:format("Write error ~p", [Err]), State}
    end.
