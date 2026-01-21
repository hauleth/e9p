% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

%% @doc Definition of 9p filesystem
%% @end
-module(e9p_fs).

-export([
         init/1,
         root/2,
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

-export_type([state/0]).

-type state() :: term().
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
-callback root(UName :: unicode:chardata(), AName :: unicode:chardata(), state()) -> {ok, e9p:qid(), state()}.

-callback flush(state()) -> {ok, state()} | {error, term(), state()}.

%% Walk through the given path starting at the `QID'
-callback walk(QID :: e9p:qid(), unicode:chardata(), state()) ->
    {e9p:qid() | false, state()}.

-callback open(QID :: e9p:qid(), Mode :: integer(), state()) -> result({e9p:qid(), e9p:u32()}).

-callback create(QID :: e9p:qid(),
                 Name :: unicode:chardata(),
                 Perm :: e9p:u32(),
                 Mode :: e9p:u8(),
                 state()) -> result({e9p:qid(), e9p:u32()}).

%% Read data from file indicated by `QID'
-callback read(QID :: e9p:qid(),
               Offset :: non_neg_integer(),
               Length :: non_neg_integer(),
               state()) -> result({e9p:qid(), iodata()}).

%% Write data to file indicated by `QID'
-callback write(QID :: e9p:qid(),
                Offset :: non_neg_integer(),
                Data :: iodata(),
                state()) -> result({e9p:qid(), non_neg_integer()}).

-callback clunk(QID :: e9p:qid(), state()) -> result().

-callback remove(QID :: e9p:qid(), state()) -> result().

%% Return stat data for file indicated by `QID'
-callback stat(QID :: e9p:qid(), state()) -> result(map()).

%% Write stat data for file indicated by `QID'
-callback wstat(QID :: e9p:qid(), map(), state()) -> result().

-optional_callbacks([
                     flush/1,
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

init({Mod, State}) ->
    case Mod:init(State) of
        {ok, NewState} -> {ok, {Mod, NewState}};
        Error -> Error
    end.

root({Mod, State}, UName, AName) ->
    case Mod:root(UName, AName, State) of
        {ok, QID, NewState} ->
            {ok, QID, {Mod, NewState}}
    end.

-doc """
Walk through paths starting at QID.
""".
walk({Mod, State}, QID, Paths) when is_atom(Mod) ->
    ?if_supported(do_walk(Mod, QID, Paths, State, [])).

do_walk(_Mod, QID, [], State, Acc) ->
    {ok, {QID, lists:reverse(Acc)}, State};
do_walk(Mod, QID0, [P | Rest], State0, Acc) ->
    case Mod:walk(QID0, P, State0) of
        {false, State} ->
            {ok, {QID0, lists:reverse(Acc)}, State};
        {QID, State} ->
            do_walk(Mod, QID, Rest, State, [QID | Acc])
    end.

open({Mod, State}, QID, Mode) ->
    ?if_supported(Mod:open(QID, Mode, State)).

create({Mod, State}, QID, Name, Perm, Mode) ->
    ?if_supported(Mod:create(QID, Name, Perm, Mode, State)).

read({Mod, State}, QID, Offset, Length) ->
    ?if_supported(Mod:read(QID, Offset, Length, State)).

write({Mod, State}, QID, Offset, Data) ->
    ?if_supported(Mod:write(QID, Offset, Data, State)).

clunk({Mod, State0}, QID) ->
    case erlang:function_exported(Mod, clunk, 3) of
        true ->
            maybe
                {ok, State} ?= Mod:clunk(QID, State0),
                {ok, {Mod, State}}
            else
                {error, Reason, StateE} ->
                    {error, Reason, {Mod, StateE}}
            end;
        false -> {ok, {Mod, State0}}
    end.

remove({Mod, State}, QID) ->
    ?if_supported(Mod:remove(QID, State)).

stat({Mod, State}, QID) ->
    ?if_supported(Mod:stat(QID, State)).

wstat({Mod, State}, QID, Stat) ->
    ?if_supported(Mod:wstat(QID, Stat, State)).
