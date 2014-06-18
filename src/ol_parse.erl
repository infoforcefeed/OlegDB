%%% HTTP parsing.
-module(ol_parse).
-include("olegdb.hrl").
-export([parse_http/1]).

parse_db_name_and_key(Data) ->
    [FirstLine|_] = binary:split(Data, [<<"\r\n">>]),
    % Actually Verb Url HttpVersion\r\n:
    [Verb, Url|_] = binary:split(FirstLine, [<<" ">>], [global]),
    ParsedUrl = parse_url(Url),
    case Verb of
        <<"GET">>    -> {get, ParsedUrl};
        <<"POST">>   -> {post, ParsedUrl};
        <<"HEAD">>   -> {head, ParsedUrl};
        <<"DELETE">> -> {delete, ParsedUrl};
        Chunk ->
            {error, <<"Didn't understand your verb.">>, Chunk}
    end.

parse_url(Url) ->
    Split = binary:split(Url, [<<"/">>], [global]),
    %io:format("S: ~p~n", [Split]),
    case Split of
        [<<>>, <<>>] -> {error, <<"No database or key specified.">>};
        % Url was like //key. Bad!
        [_, <<>>, <<_/binary>>|_] -> {error, <<"No database specified.">>};
        % These handle cursor iteration:
        [_, <<DB_Name/binary>>, <<Key/binary>>, <<"_next">>|_] -> {ok, DB_Name, Key, {cursor, next}};
        [_, <<DB_Name/binary>>, <<Key/binary>>, <<"_prev">>|_] -> {ok, DB_Name, Key, {cursor, prev}};
        [_, <<DB_Name/binary>>, <<Key/binary>>, <<"_first">>|_] -> {ok, DB_Name, Key, {cursor, first}};
        [_, <<DB_Name/binary>>, <<Key/binary>>, <<"_last">>|_] -> {ok, DB_Name, Key, {cursor, last}};
        % Prefix matching:
        [_, <<DB_Name/binary>>, <<Key/binary>>, <<"_match">>|_] -> {ok, DB_Name, Key, {prefix, match}};
        % Url was like /users/1 or /pictures/thing
        [_, <<DB_Name/binary>>, <<Key/binary>> |_] -> {ok, DB_Name, Key};
        % The url was like /test or /what, so just assume the default DB.
        [_, <<Key/binary>> |_] -> {ok, Key}
    end.

parse_http(Data) ->
    case parse_db_name_and_key(Data) of
        % The only difference between these two is /<db_name>/<key> vs. /<key>
        {ReqType, {ok, DB_Name, Key}} ->
            {ok, ReqType, parse_header(Data, #ol_record{database=DB_Name, key=Key}
                                      )};
        % Default to whatever database name is specified in include/olegdb.hrl:
        {ReqType, {ok, Key}} ->
            {ok, ReqType, parse_header(Data, #ol_record{key=Key})};
        {ReqType, {ok, DB_Name, Key, Operand}} ->
            Parsed = parse_header(Data, #ol_record{database=DB_Name, key=Key}),
            {ok, ReqType, Parsed, Operand};
        {ReqType, {error, ErrorDesc}} -> {ReqType, {error, ErrorDesc}};
        X -> io:format("[-] Could not parse http in a sane way: ~p~n", [X]),
            throw(oleg_bad_parse)
    end.

parse_header(Data, Record) ->
    Split = binary:split(Data, [<<"\r\n\r\n">>]),
    %io:format("Split: ~p~n", [Split]),
    case Split of
        [Header,PostedData|_] ->
            LowercaseHeader = ol_util:bits_to_lower(Header),
            parse_header1(binary:split(LowercaseHeader, [<<"\r\n">>], [global]),
                          { Record#ol_record{value=PostedData}, []});
        [Header|_] ->
            LowercaseHeader = ol_util:bits_to_lower(Header),
            parse_header1(binary:split(LowercaseHeader, [<<"\r\n">>], [global]),
                          { Record#ol_record{}, []});
        X -> io:format("[-] Could not parse header in a sane way: ~p~n", [X]),
            throw(bad_parse)
    end.

%% Tail recursive function that maps over the lines in a header to fill out
%% an ol_record to later.
parse_header1([], {Record, Options}) -> {Record, Options};
parse_header1([Line|Header], {Record, Options}) ->
    case Line of
        <<"expect: 100-continue">> ->
            parse_header1(Header, {Record, Options ++ [send_100]});
        <<"content-length: ", CLength/binary>> ->
            % This is only used for 100 requests
            Len = list_to_integer(binary_to_list(CLength)),
            parse_header1(Header, {Record#ol_record{content_length=Len}, Options});
        <<"x-olegdb-use-by: ", Timestamp/binary>> ->
            Time = list_to_integer(binary_to_list(Timestamp)),
            parse_header1(Header, {Record#ol_record{expiration_time=Time}, Options});
        <<"content-type: ", CType/binary>> ->
            parse_header1(Header, {Record#ol_record{content_type=CType}, Options});
        _ ->
            parse_header1(Header, {Record, Options})
    end.
