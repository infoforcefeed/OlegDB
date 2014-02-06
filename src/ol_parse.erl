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
-module(ol_parse).
-include("olegdb.hrl").
-export([parse_http/1]).

parse_db_name_and_key(Data) ->
    case binary:split(Data, [<<"\r\n">>]) of
        [<<"GET ", Rest/binary>>|_] -> parse_url(Rest);
        [<<"POST ", Rest/binary>>|_] -> parse_url(Rest);
        [<<"DELETE ", Rest/binary>>|_]-> parse_url(Rest);
        Chunk -> {error, "Didn't understand your verb.", Chunk}
    end.

parse_url(FirstLine) ->
    [URL|_] = binary:split(FirstLine, [<<" ">>]),
    Split = binary:split(URL, [<<"/">>], [global]),
    io:format("S: ~p~n", [Split]),
    case Split of
        [<<>>, <<>> |_] -> {error, "No database or key specified."};
        % Url was like /users/1 or /pictures/thing
        [_, DB_Name, Key |_] -> {ok, DB_Name, Key};
        % The url was like /test or /what, so just assume the default DB.
        [_, Key |_] -> {ok, Key}
    end.

parse_http(Data) ->
    case parse_db_name_and_key(Data) of
        {ok, DB_Name, Key} ->
            parse_header(Data, #ol_record{database=DB_Name,
                                          key=Key});
        {ok, Key} ->
            parse_header(Data, #ol_record{key=Key});
        {error, ErrMsg} -> {error, ErrMsg}
    end.

parse_header(Data, Record) ->
    case binary:split(Data, [<<"\r\n\r\n">>]) of
        [Header|PostedData] ->
            parse_header1(binary:split(Header, [<<"\r\n">>], [global]),
                          Record#ol_record{value=PostedData});
        X -> X
    end.

%% Tail recursive function that maps over the lines in a header to fill out
%% an ol_record to later.
parse_header1([], Record) -> Record;
parse_header1([Line|Header], Record) ->
    case Line of
        <<"Content-Length: ", CLength/binary>> ->
            Len = list_to_integer(binary_to_list(CLength)),
            parse_header1(Header, Record#ol_record{content_length=Len});
        <<"Content-Type: ", CType/binary>> ->
            parse_header1(Header, Record#ol_record{content_type=binary_to_list(CType)});
        _ ->
            parse_header1(Header, Record)
    end.
