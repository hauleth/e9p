-module(e9p_proto).

-behaviour(ranch_protocol).

-export([start_link/3]).

-export([init/1, loop/3]).

-include("e9p_internal.hrl").
-include_lib("kernel/include/logger.hrl").

start_link(Ref, Transport, Opts) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [{Ref, Transport, Opts}]),
    {ok, Pid}.

init({Ref, Transport, Opts}) ->
    {ok, Socket} = ranch:handshake(Ref),
    ?MODULE:loop(Socket, Transport, Opts).

loop(Socket, Transport, Opts) ->
    Timeout = 300000,
    case Transport:recv(Socket, 7, Timeout) of
        {ok, <<Size:4/?int, Type:1/?int, Tag:2/?int>>} ->
            {ok, Data} = Transport:recv(Socket, Size - 7, 0),
            {ok, #{type := TType, data := TMsg}} = e9p_msg:parse(Type, Data),
            ?LOG_DEBUG("-> ~4.16.0B: ~s ~p~n", [Tag, TType, TMsg]),
            {RType, RMsg} = handle_msg(TType, TMsg),
            ?LOG_DEBUG("<- ~4.16.0B: ~s ~p~n", [Tag, RType, RMsg]),
            Resp = e9p_msg:encode(Tag, RType, RMsg),
            Transport:send(Socket, Resp),
            ?MODULE:loop(Socket, Transport, Opts);
        _Other ->
            ok = Transport:close(Socket)
    end.

handle_msg(tversion, #{version := <<"9P2000">>, max_packet_size := MP}) ->
    {rversion, #{version => <<"9P2000">>, max_packet_size => MP}};
handle_msg(tversion, #{version := Version}) ->
    {rerror, #{error => ["Unsupported version: ", Version]}};
handle_msg(tattach, _) ->
    {rattach, #{qid => #{type => 0, version => 0, path => 0}}};
handle_msg(Msg, _) ->
    {rerror, #{error => io_lib:format("Unsupported message ~s", [Msg])}}.
