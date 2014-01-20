-module(parsing).
-export([parse_http/1]).

read_line(Data) ->
    read_line1(Data, <<>>).

read_line1(<<>>, Tail) -> Tail;
read_line1(<<Chunk:1/binary, Rest/binary>>, Tail) ->
    case Chunk of
        <<"\r">> ->
            Tail;
        <<"\n">> ->
            Tail;
        _ ->
            read_line1(Rest, <<Chunk:1/binary,Tail/binary>>)
    end.

parse_http(Data) ->
    read_line(Data).
