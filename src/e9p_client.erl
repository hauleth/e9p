% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(e9p_client).

-include("e9p_internal.hrl").
-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([attach/3]).
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

attach(Client, Uname, Aname) ->
    gen_server:call(Client, {attach, noauth, Uname, Aname}).

start_link(Host, Port, Opts) ->
    gen_server:start_link(?MODULE, {Host, Port, Opts}, []).

init({Host, Port, _Opts}) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, false}, binary]),
    case version_negotiation(Socket) of
        {ok, #rversion{max_packet_size = MaxPacketSize, version = ?version}} ->
            inet:setopts(Socket, [{active, once}]),
            {ok,
             #{socket => Socket,
               buffer => <<>>,
               tag => 0,
               fid => 0,
               msgs => #{},
               max_packet_size => MaxPacketSize,
               version => ?version}};
        {ok, #rversion{version = OtherVersion}} ->
            {error, {unsupported_version, OtherVersion}};
        {error, _} = Error ->
            Error
    end.

handle_call({attach, Auth, Uname, Aname}, From, State) ->
    #{tag := Tag,
      fid := Fid,
      socket := Socket,
      msgs := Msgs} =
        State,
    Afid =
        case Auth of
            noauth ->
                ?nofid;
            Id ->
                Id
        end,
    Msg = #tattach{fid = Fid,
                  afid = Afid,
                  uname = Uname,
                  aname = Aname},
    e9p_transport:send(Socket, Tag, Msg),
    {noreply,
     State#{tag := Tag + 1,
            fid := Fid + 1,
            msgs := Msgs#{Tag => {From, #{fid => Fid}}}}};
handle_call(_Msg, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, #{socket := Socket} = State) ->
    #{buffer := Buffer, msgs := Msgs0} = State,
    inet:setopts(Socket, [{active, once}]),
    case e9p_transport:read_stream(<<Buffer/binary, Data/binary>>) of
        {ok, Tag, Msg, Rest} ->
            Msgs =
                case maps:take(Tag, Msgs0) of
                    {{From, _}, M} ->
                        gen_server:reply(From, {ok, Msg}),
                        M;
                    error ->
                        ?LOG_WARNING("Unknown tag ~p", [Tag]),
                        Msgs0
                end,
            {noreply, State#{buffer := Rest, msgs := Msgs}};
        {more, Data} ->
            {noreply, State#{buffer := Data}}
    end.

version_negotiation(Socket) ->
    Msg = #tversion{max_packet_size = ?max_packet_size, version = ?version},
    e9p_transport:send(Socket, notag, Msg),
    case e9p_transport:read(Socket) of
        {ok, _, Resp} ->
            {ok, Resp};
        {error, _} = Error ->
            Error
    end.
