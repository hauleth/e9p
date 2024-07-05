%% @hidden

-module(e9p_io_server).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(QID, Client) ->
    gen_server:start_link(?MODULE, {QID, Client}, []).

init({QID, Client}) ->
    {ok, {QID, Client}}.

handle_call(_Msg, _From, State) ->
    {reply, notsupported, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({io_request, From, ReplyAs, Request}, State) ->
    ?LOG_NOTICE("~p", [Request]),
    From ! {io_reply, ReplyAs, {error, notimplemented}},
    {noreply, State}.
