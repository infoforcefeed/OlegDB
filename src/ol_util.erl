%%% The MIT License (MIT)
%%% 
%%% Copyright (c) 2014 Quinlan Pfiffer, Kyle Terry
%%% 
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
-module(ol_util).
-include("olegdb.hrl").
-export([read_all_data/2, read_remaining_data/2]).
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
