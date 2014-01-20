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
    end;
read_line1(_, Tail) -> Tail.

parse_http(Data) ->
    Line = read_line(Data),
    io:format("[-] Data: ~p~n", [Line]),
    Line.
