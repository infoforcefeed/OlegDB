%%% HTTP responses/headers.
-module(ol_http).
-export([get_response/1,
         not_found_response/0,
         post_response/0,
         deleted_response/0,
         continue_you_shit_response/0,
         bucket_meta_response/2,
         bucket_meta_response/1,
         cursor_bucket_response/2,
         error_response/1,
         prefix_response/2
        ]).

get_response(Data) ->
    io_lib:format(
        <<"HTTP/1.1 200 OK\r\n"
        "Server: OlegDB/fresh_cuts_n_jams\r\n"
        "Content-Type: application/octet-stream\r\n"
        "Content-Length: ~p\r\n"
        "Connection: close\r\n"
        "\r\n~s">>, [byte_size(Data), Data]).

cursor_bucket_response(CursorKey, CursorData) ->
    io_lib:format(
        <<"HTTP/1.1 200 OK\r\n"
        "Server: OlegDB/fresh_cuts_n_jams\r\n"
        "Content-Type: application/octet-stream\r\n"
        "Content-Length: ~p\r\n"
        "Connection: close\r\n"
        "X-OlegDB-Key: ~s\r\n"
        "\r\n~s">>, [byte_size(CursorData), CursorKey, CursorData]).

prefix_response(NumMatches, MatchesList) ->
    Converted = ol_util:list_to_bad_json(MatchesList),
    io_lib:format(
        <<"HTTP/1.1 200 OK\r\n"
        "Server: OlegDB/fresh_cuts_n_jams\r\n"
        "Content-Type: application/json\r\n"
        "Content-Length: ~p\r\n"
        "Connection: close\r\n"
        "X-OlegDB-Num-Matches: ~p\r\n"
        "\r\n~s">>, [byte_size(Converted), NumMatches, Converted]).

not_found_response() ->
    <<"HTTP/1.1 404 Not Found\r\n"
    "Status: 404 Not Found\r\n"
    "Server: OlegDB/fresh_cuts_n_jams\r\n"
    "Content-Length: 26\r\n"
    "Connection: close\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n"
    "These aren't your ghosts.\n">>.

post_response() ->
    <<"HTTP/1.1 200 OK\r\n"
    "Server: OlegDB/fresh_cuts_n_jams\r\n"
    "Content-Type: text/plain\r\n"
    "Connection: close\r\n"
    "Content-Length: 7\r\n"
    "\r\n"
    "無駄\n">>.

deleted_response() ->
    <<"HTTP/1.1 200 OK\r\n"
    "Server: OlegDB/fresh_cuts_n_jams\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: 44\r\n"
    "Connection: close\r\n"
    "\r\n"
    "The wind whispers through your empty forest.\n">>.

continue_you_shit_response() ->
    <<"HTTP/1.1 100 Continue\r\n"
    "Server: OlegDB/fresh_cuts_n_jams\r\n"
    "Content-Length: 0\r\n"
    "\r\n">>.

bucket_meta_response(RcrdCnt, Expires) ->
    io_lib:format(
      <<"HTTP/1.1 200 OK\r\n"
        "Server: OlegDB/fresh_cuts_n_jams\r\n"
        "Content-Length: 0\r\n"
        "Content-Type: application/octet-stream\r\n"
        "X-OlegDB-Rcrd-Cnt: ~p\r\n"
        "Expires: ~p\r\n"
        "\r\n">>, [RcrdCnt, Expires]).

bucket_meta_response(RcrdCnt) ->
    io_lib:format(
      <<"HTTP/1.1 200 OK\r\n"
        "Server: OlegDB/fresh_cuts_n_jams\r\n"
        "Content-Length: 0\r\n"
        "Content-Type: application/octet-stream\r\n"
        "X-OlegDB-Rcrd-Cnt: ~p\r\n"
        "\r\n">>, [RcrdCnt]).

error_response(Data) ->
    io:format("[x] Error: ~p~n", [Data]),
    io_lib:format(
        <<"HTTP/1.1 500 Internal Server Error\r\n"
        "Server: OlegDB/fresh_cuts_n_jams\r\n"
        "Content-Length: ~p\r\n"
        "\r\n"
        "~s">>, [byte_size(Data), Data]).
