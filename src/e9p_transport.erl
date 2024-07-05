-module(e9p_transport).

-include("e9p_internal.hrl").

-export([send/3, read/1, read_stream/1]).

send(Socket, Tag, Message) ->
    Encoded = e9p_msg:encode(Tag, Message),
    gen_tcp:send(Socket, Encoded).

read(Socket) ->
    case gen_tcp:recv(Socket, 7) of
        {ok, <<Size:4/?int, Type, Tag:2/?int>>} ->
            case gen_tcp:recv(Socket, Size - 7) of
                {ok, Data} ->
                    case e9p_msg:parse(Type, Data) of
                        {ok, Msg} ->
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

read_stream(<<Size:4/?int, Type, Tag:2/?int, Data:(Size - 7)/binary, Rest/binary>> = Input) ->
    case e9p_msg:parse(Type, Data) of
        {ok, Msg} ->
            {ok, Tag, Msg, Rest};
        {error, Error} ->
            {error, Error, Input}
    end;
read_stream(Input) ->
    {more, Input}.
