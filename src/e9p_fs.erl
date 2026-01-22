% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

%% @doc Definition of 9p filesystem
%% @end
-module(e9p_fs).

-export([
         init/1,
         root/3,
         walk/3,
         open/3,
         create/5,
         read/4,
         write/4,
         clunk/2,
         remove/2,
         stat/2,
         wstat/3
        ]).

-include("e9p_internal.hrl").
-include_lib("kernel/include/logger.hrl").

-export_type([state/0, fid/0, path/0, result/0, result/1]).

-type state() :: term().
-type fid() :: {QID :: e9p:qid(), State :: fid_state()}.
-type path() :: [unicode:chardata()].
-type fid_state() :: term().
-type result() :: {ok, state()} | {error, term(), state()}.
-type result(T) :: {ok, T, state()} | {error, term(), state()}.

-define(if_supported(Code),
        case erlang:function_exported(Mod, ?FUNCTION_NAME, ?FUNCTION_ARITY) of
             true ->
                case (fun() -> Code end)() of
                    {ok, Ret, NewState} ->
                        {ok, Ret, {Mod, NewState}};
                    {error, Error, NewState} ->
                        {error, Error, {Mod, NewState}}
                end;
             false -> {error, nosupport, {Mod, State}}
         end).

%% Setup state for given filesystem.
-callback init(term()) ->
    {ok, state()} |
    {error, Reason :: term()}.

%% Returns `QID' for root node.
%%
%% If implementation provides multiple trees then the `AName' will be set to the
%% tree defined by the client. It is left to the implementation to ensure the
%% constraints of the file root (aka `walk(Root, "..", State0) =:= {Root, State1}'.
-callback root(UName :: unicode:chardata(), AName :: unicode:chardata(), state()) ->
    {ok, fid_state(), state()}.

-callback flush(state()) -> result().

%% Walk through the given path starting at the `QID'
-callback walk(fid(), File :: unicode:chardata(), unicode:chardata(), state()) ->
    {fid() | false, state()}.

-callback open(fid(), path(), Mode :: integer(), state()) -> result({fid_state(), e9p:u32()}).

-callback create(fid(),
                 path(),
                 Name :: unicode:chardata(),
                 Perm :: e9p:u32(),
                 Mode :: e9p:u8(),
                 state()) -> result({fid(), IOUnit :: e9p:u32()}).

%% Read data from file indicated by `QID'
-callback read(fid(),
               path(),
               Offset :: non_neg_integer(),
               Length :: non_neg_integer(),
               state()) -> result({fid_state(), iodata()}).

%% Write data to file indicated by `QID'
-callback write(fid(),
                path(),
                Offset :: non_neg_integer(),
                Data :: iodata(),
                state()) -> result({fid_state(), non_neg_integer()}).

-callback clunk(fid(), path(), state()) -> result().

-callback remove(fid(), path(), state()) -> result().

%% Return stat data for file indicated by `QID'
-callback stat(fid(), path(), state()) -> result(map()).

%% Write stat data for file indicated by `QID'
-callback wstat(fid(), path(), map(), state()) -> result().

-optional_callbacks([
                     flush/1,
                     clunk/3
                    ]).

init({Mod, State}) ->
    case Mod:init(State) of
        {ok, NewState} -> {ok, {Mod, NewState}};
        Error -> Error
    end.

root({Mod, State}, UName, AName) ->
    case Mod:root(UName, AName, State) of
        {ok, {QID, FState}, NewState} ->
            {ok, #fid{qid = QID, path = [], state = FState}, {Mod, NewState}}
    end.

-doc """
Walk through paths starting at QID.
""".
walk({Mod, State0}, FID0, Paths) when is_atom(Mod) ->
    case do_walk(Mod, FID0, Paths, State0, []) of
        {ok, {FID, QIDs}, State} -> {ok, {FID, QIDs}, {Mod, State}};
        {error, Reason, State} -> {error, Reason, {Mod, State}}
    end.

do_walk(_Mod, FID, [], State, Acc) ->
    {ok, {FID, lists:reverse(Acc)}, State};
do_walk(_Mod, #fid{path = []}, [~".." | _Names], State, _Acc) ->
    {error, "Cannot walt to root parent of root directory", State};
do_walk(Mod, #fid{qid = QID0, path = Path, state = FState0} = FID0, [P | Rest], State0, Acc) ->
    case e9p:is_type(QID0, directory) of
        true ->
            case Mod:walk({QID0, FState0}, Path, P, State0) of
                {false, State} when Acc =:= [] ->
                    % Per specification walk to first entry in name list must succeed
                    % (if any) otherwise return error. In subsequent steps we return
                    % successful list and last succeeded QID
                    {error, io_lib:format("Failed walk to ~p", [P]), State};
                {false, State} ->
                    {ok, {FID0, lists:reverse(Acc)}, State};
                {{QID, FState}, State} ->
                    FID = #fid{qid = QID, state = FState, path = Path ++ [P]},
                    do_walk(Mod, FID, Rest, State, [QID | Acc])
            end;
        false ->
            {error, io_lib:format("Not directory ~p", [Path]), State0}
    end.

open({Mod, State0}, #fid{qid = QID, path = Path, state = FState0} = FID, Mode) ->
    EMode = translate_mode(Mode),
    case Mod:open({QID, FState0}, Path, EMode, State0) of
        {ok, {FState, IOUnit}, State} ->
            {ok, {FID#fid{state = FState}, IOUnit}, {Mod, State}};
        {error, Reason, StateE} -> {error, Reason, {Mod, StateE}}
    end.

translate_mode(Mode) when Mode >= 16#10 ->
    [trunc | translate_mode(Mode band 16#EF)];
translate_mode(0) -> [read];
translate_mode(1) -> [write];
translate_mode(2) -> [append];
translate_mode(3) -> [exec].

create({Mod, State}, #fid{qid = QID, path = Path, state = FState}, Name, Perm, Mode) ->
    ?if_supported(Mod:create({QID, FState}, Path, Name, Perm, Mode, State)).

read({Mod, State0}, #fid{qid = QID, path = Path, state = FState0} = FID, Offset, Length) ->
    case Mod:read({QID, FState0}, Path, Offset, Length, State0) of
        {ok, {FState, Data}, State} -> {ok, {FID#fid{state = FState}, Data}, {Mod, State}};
        {error, Reason, StateE} -> {error, Reason, {Mod, StateE}}
    end.

write({Mod, State0}, #fid{qid = QID, path = Path, state = FState0} = FID, Offset, Data) ->
    case Mod:write({QID, FState0}, Path, Offset, Data, State0) of
        {ok, {FState, Len}, State} -> {ok, {FID#fid{state = FState}, Len}, {Mod, State}};
        {error, Reason, StateE} -> {error, Reason, {Mod, StateE}}
    end.

clunk({Mod, State0}, #fid{qid = QID, path = Path, state = FState}) ->
    case erlang:function_exported(Mod, clunk, 3) of
        true ->
            case Mod:clunk({QID, FState}, Path, State0) of
                {ok, State} -> {ok, {Mod, State}};
                {error, Reason, StateE} -> {error, Reason, {Mod, StateE}}
            end;
        false -> {ok, {Mod, State0}}
    end.

remove({Mod, State0}, #fid{qid = QID, path = Path, state = FState}) ->
    case Mod:remove({QID, FState}, Path, State0) of
        {ok, State} -> {ok, {Mod, State}};
        {error, Reason, State} -> {error, Reason, {Mod, State}}
    end.

stat({Mod, State0}, #fid{qid = QID, path = Path, state = FState0}) ->
    case Mod:stat({QID, FState0}, Path, State0) of
        {ok, Stat, State} -> {ok, Stat, {Mod, State}};
        {error, Reason, StateE} -> {error, Reason, {Mod, StateE}}
    end.

wstat({Mod, State0}, #fid{qid = QID, path = Path, state = FState}, Stat) ->
    case Mod:wstat({QID, FState}, Path, Stat, State0) of
        {ok, State} -> {ok, {Mod, State}};
        {error, Reason, State} -> {error, Reason, {Mod, State}}
    end.
