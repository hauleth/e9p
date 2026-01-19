% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(e9p_transport).

-include("e9p_internal.hrl").

% -include_lib("kernel/include/logger.hrl").

-export([send/3, read/1, read_stream/1]).

send(Socket, Tag, Message) ->
    Encoded = e9p_msg:encode(Tag, Message),
    Size = iolist_size(Encoded) + 4,
    gen_tcp:send(Socket, [<<Size:4/?int>>, Encoded]).

read(Socket) ->
    case gen_tcp:recv(Socket, 4) of
        {ok, <<Size:4/?int>>} ->
            case gen_tcp:recv(Socket, Size - 4) of
                {ok, Data} when is_binary(Data) ->
                    case e9p_msg:parse(Data) of
                        {ok, Tag, Msg} ->
                            {ok, Tag, Msg};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

read_stream(<<Size:4/?int, Data:(Size - 4)/binary, Rest/binary>> = Input) ->
    case e9p_msg:parse(Data) of
        {ok, Tag, Msg} ->
            {ok, Tag, Msg, Rest};
        {error, Error} ->
            {error, Error, Input}
    end;
read_stream(Input) ->
    {more, Input}.
