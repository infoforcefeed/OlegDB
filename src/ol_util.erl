%%% Common utility functions.
-module(ol_util).
-include("olegdb.hrl").
-export([read_all_data/2, read_remaining_data/2, bits_to_lower/1, list_to_bad_json/1]).
read_remaining_data(Header, Socket) ->
    ExpectedLength = Header#ol_record.content_length,
    if
        byte_size(Header#ol_record.value) < ExpectedLength ->
            Header#ol_record{value =
                             read_all_data1(Socket,
                                            Header#ol_record.content_length,
                                            Header#ol_record.value)
                            };
        true ->
            Header
    end.

read_all_data(Socket, ExpectedLength) ->
    read_all_data1(Socket, ExpectedLength, <<>>).
read_all_data1(Socket, ExpectedLength, Data) ->
    case gen_tcp:recv(Socket, 0, 60000) of
        {ok, ReadData} ->
            Combined = <<Data/binary, ReadData/binary>>,
            if
                byte_size(Combined) < ExpectedLength ->
                    %io:format("[-] Continuing to read. Byte size: ~p~n", [byte_size(Combined)]),
                    read_all_data1(Socket, ExpectedLength, Combined);
                true -> 
                    Combined
            end;
        {error, closed} ->
            ok;
        {error, timeout} ->
            io:format("[-] Client timed out.~n"),
            ok
    end.

bits_to_lower(<<X,Rest/bits>>) -> <<(string:to_lower(X)), (bits_to_lower(Rest))/bits>>;
bits_to_lower(_) -> <<>>.

list_to_bad_json1([], ByteString) -> <<ByteString/binary, "]">>;
list_to_bad_json1([ByteString|ListOfBits], Accumulator) when Accumulator == <<"[">> ->
    % Don't encode a comma after the first value.
    list_to_bad_json1(ListOfBits, <<Accumulator/binary,"\"",ByteString/binary,"\"">>);
list_to_bad_json1([ByteString|ListOfBits], Accumulator) ->
    list_to_bad_json1(ListOfBits, <<Accumulator/binary,",\"",ByteString/binary,"\"">>).

list_to_bad_json(ListOfBits) -> list_to_bad_json1(ListOfBits, <<"[">>).
