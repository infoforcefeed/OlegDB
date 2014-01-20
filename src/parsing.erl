-module(parsing).
-include("olegdb.hrl").
-export([parse_get/1, parse_post/1]).

parse_get(Data) ->
    case binary:split(Data, [<<"\r\n\r\n">>]) of
        [Header|_ ] -> Header;
        X -> X
    end.

parse_post(Data) ->
    case binary:split(Data, [<<"\r\n\r\n">>]) of
        [Header|_] -> Header;
        X -> X
    end.

%parse_header(_, Record) ->
%    Record.
