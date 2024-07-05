%% @doc Definition of 9p filesystem
%% @end
-module(e9p_fs).

% -behaviour(gen_server).

-export([start_link/2, walk/3]).

-export([init/1, handle_call/3]).

-export_type([fs/0, state/0]).

-opaque fs() :: pid().

-type state() :: term().

%% Setup state for given filesystem.
-callback init(term()) -> state().

%% Returns `QID' for root node.
%%
%% If implementation provides multiple trees then the `AName' will be set to the
%% tree defined by the client. It is left to the implementation to ensure the
%% constraints of the file root (aka `walk(Root, "..", State0) =:= {Root, State1}'.
-callback root(AName :: unicode:chardata(), state()) -> {e9p:qid(), state()}.

%% Walk through the given path starting at the `QID'
-callback walk(QID :: e9p:qid(), unicode:chardata(), state()) ->
    {e9p:qid() | false, state()}.

%% Return stat data for file indicated by `QID'
-callback stat(QID :: e9p:qid(), state()) -> {ok, map(), state()} | {error, term(), state()}.

%% Read data from file indicated by `QID'
-callback read(QID :: e9p:qid(),
               Offset :: non_neg_integer(),
               Length :: non_neg_integer(),
               state()) -> {ok, iodata(), state()} | {error, term(), state()}.

%% Write data to file indicated by `QID'
-callback write(QID :: e9p:qid(),
                Offset :: non_neg_integer(),
                Data :: iodata(),
                state()) -> {ok, non_neg_integer(), state()} | {error, term(), state()}.

%% @doc Walk through the filesystem.
%%
%% Walks through `List' entries starting at `QID'. It will stop at first path
%% that cannot be walked into. This mean, that returned list length will be
%% equal <b>or less</b> than the `length(List)'.
-spec walk(fs(), e9p:qid(), [unicode:chardata()]) -> {ok, [e9p:qid()]} | {error, term()}.
walk(FS, QID, List) ->
    List0 = e9p_utils:normalize_path(List, []),
    case List of
        [] -> {ok, []};
        [_|_] -> gen_server:call(FS, {walk, QID, List0})
    end.

%% @private
start_link(Impl, Init) ->
    gen_server:start_link(?MODULE, {Impl, Init}, []).

%% @private
init({Impl, Init}) ->
    case Impl:init(Init) of
        {ok, State0} ->
            {ok, #{mod => Impl, state => State0}};
        {error, _} = Error ->
            Error
    end.

%% @private
handle_call({walk, QID, List}, _From, #{mod := Mod, state := State0}) ->
    {QIDs, State} = do_walk(Mod, QID, List, State0),
    {reply, {ok, QIDs}, State}.

%% Walk through the FS tree.
do_walk(Mod, QID, List, State) ->
    do_walk(Mod, QID, List, State, []).

do_walk(_Mod, _QID, [], State, Acc) ->
    {lists:reverse(Acc), State};
do_walk(Mod, QID0, [P | Rest], State0, Acc) ->
    case Mod:walk(QID0, P, State0) of
        {false, State} ->
            {lists:reverse(Acc), State};
        {QID1, State} ->
            do_walk(Mod, QID1, Rest, State, [QID1 | Acc])
    end.
