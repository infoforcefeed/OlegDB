-module(parsing).
-include("olegdb.hrl").
-export([parse_get/1, parse_post/1]).

parse_get(Data) ->
    case binary:split(Data, [<<"\r\n\r\n">>]) of
        [Header|_ ] -> parse_header(Header);
        X -> X
    end.

parse_post(Data) ->
    case binary:split(Data, [<<"\r\n\r\n">>]) of
        [Header|_] -> parse_header(Header);
        X -> X
    end.

parse_header(Header) ->
    parse_header1(binary:split(Header, [<<"\r\n">>], [global]), #ol_record{}).

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
