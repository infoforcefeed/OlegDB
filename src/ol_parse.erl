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
-define(KEY_SIZE, 32). % Should match the one in include/oleg.h

parse_db_name_and_key(Data) ->
    [FirstLine|_] = binary:split(Data, [<<"\r\n">>]),
    % Actually Verb Url HttpVersion\r\n:
    [Verb, Url|_] = binary:split(FirstLine, [<<" ">>], [global]),
    ParsedUrl = parse_url(Url),
    case Verb of
        <<"GET">>    -> {get, ParsedUrl};
        <<"POST">>   -> {post, ParsedUrl};
        <<"DELETE">> -> {delete, ParsedUrl};
        Chunk ->
            {error, <<"Didn't understand your verb.">>, Chunk}
    end.

parse_url(Url) ->
    Split = binary:split(Url, [<<"/">>], [global]),
    %io:format("S: ~p~n", [Split]),
    case Split of
        [<<>>, <<>>] -> {error, <<"No database or key specified.">>};
        % Url was like /users/1 or /pictures/thing
        [_, <<DB_Name/binary>>, <<Key/binary>> |_] -> {ok, DB_Name, Key};
        % Url was like //key. Bad!
        [_, <<>>, <<Key/binary>> |_] -> {ok, Key};
        % The url was like /test or /what, so just assume the default DB.
        [_, <<Key/binary>> |_] -> {ok, Key}
    end.

parse_http(Data) ->
    case parse_db_name_and_key(Data) of
        {ReqType, {ok, DB_Name, Key}} ->
            {ok, ReqType, parse_header(Data,
                    #ol_record{database=DB_Name,key=Key}
                )};
        {ReqType, {ok, Key}} ->
            {ok, ReqType, parse_header(Data, #ol_record{key=Key})};
        X -> X
    end.

parse_header(Data, Record) ->
    Split = binary:split(Data, [<<"\r\n\r\n">>]),
    %io:format("Split: ~p~n", [Split]),
    case Split of
        [Header,PostedData|_] ->
            parse_header1(binary:split(Header, [<<"\r\n">>], [global]),
                          { Record#ol_record{value=PostedData}, []});
        X -> X
    end.

%% Tail recursive function that maps over the lines in a header to fill out
%% an ol_record to later.
parse_header1([], {Record, Options}) -> {Record, Options};
parse_header1([Line|Header], {Record, Options}) ->
    case Line of
        <<"Expect: 100-continue">> ->
            parse_header1(Header, {Record, Options ++ [send_100]});
        <<"Content-Length: ", CLength/binary>> ->
            % This is only used for 100 requests
            Len = list_to_integer(binary_to_list(CLength)),
            %io:format("Length is ~p~n", [Len]),
            parse_header1(Header, {Record#ol_record{content_length=Len}, Options});
        <<"Content-Type: ", CType/binary>> ->
            parse_header1(Header, {Record#ol_record{content_type=CType}, Options});
        _ ->
            parse_header1(Header, {Record, Options})
    end.
