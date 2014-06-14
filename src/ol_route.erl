%% General routery.
-module(ol_route).
-include("olegdb.hrl").
-export([cursor_operand/1, route/2]).
cursor_operand({ReqType, Header, Operand}) ->
    if
        ReqType == get ->
            %io:format("[-] Requesting cursor ~p~n", [Header#ol_record.key]),
            case Operand of
                next ->
                    case ol_database:ol_next_key(Header) of
                        {ok, NextContentType, NextKey, NextData} ->
                            ol_http:next_bucket_response(NextContentType, NextKey, NextData);
                        ErrMsg -> ol_http:error_response(ErrMsg)
                    end;
                prev ->
                    case ol_database:ol_prev_key(Header) of
                        %{ok, ContentType, Data, Prev} ->
                        %    ol_http:get_response(ContentType, Data);
                        _ -> ol_http:error_response(<<"Prev not implemented.">>)
                    end
            end;
        true ->
            ol_http:error_response(<<"Cursors do not support the requested verb.">>)
    end.

route(Bits, Socket) ->
    case ol_parse:parse_http(Bits) of
        {ok, _, {Header, [send_100|_]}} ->
            hundred_handler(Header, Socket);
        {ok, ReqType, {Header, _}, Operand} ->
            cursor_operand({ReqType, Header, Operand});
        {ok, ReqType, {Header, _}} ->
            case ReqType of
                get ->
                    %io:format("[-] Requesting ~p~n", [Header#ol_record.key]),
                    %ol_http:not_found_response();
                    case ol_database:ol_unjar(Header) of
                        {ok, ContentType, Data} ->
                            ol_http:get_response(ContentType, Data);
                        _ -> ol_http:not_found_response()
                    end;
                head ->
                    %io:format("[-] HEAD ~p~n", [Header#ol_record.key]),
                    case ol_database:ol_bucket_meta(Header) of
                        {ok, ContentType, RcrdCnt} ->
                            ol_http:bucket_meta_response(ContentType, RcrdCnt);
                        {ok, ContentType, RcrdCnt, Expires} ->
                            ol_http:bucket_meta_response(ContentType, RcrdCnt, Expires);
                        _ -> ol_http:not_found_response()
                    end;
                post ->
                    %io:format("[-] Posting to ~p~n", [Header#ol_record.key]),
                    NewHeader = ol_util:read_remaining_data(Header, Socket),
                    case ol_database:ol_jar(NewHeader) of
                        ok -> ol_http:post_response();
                        _ -> ol_http:not_found_response()
                    end;
                delete ->
                    %io:format("[-] Deleting ~p~n", [Header#ol_record.key]),
                    case ol_database:ol_scoop(Header) of
                        ok -> ol_http:deleted_response();
                        _ -> ol_http:not_found_response()
                    end;
                {error, ErrMsg} ->
                    ol_http:error_response(ErrMsg)
            end;
        {_, {error, ErrMsg}} ->
            io:format("[X] Error ~p~n", [ErrMsg]),
            ol_http:error_response(ErrMsg);
        X ->
            io:format("[X] Somebody requested something weird: ~p~n", [X]),
            ol_http:error_response(<<"Make a better request next time.">>)
    end.

hundred_handler(Header, Socket) ->
    case gen_tcp:send(Socket, ol_http:continue_you_shit_response()) of
        ok ->
            Data = ol_util:read_all_data(Socket, Header#ol_record.content_length),
            case ol_database:ol_jar(Header#ol_record{value=Data}) of
                ok -> ol_http:post_response();
                _ -> ol_http:not_found_response()
            end;
        {error, Reason} ->
            io:format("[-] Could not send to socket: ~p~n", [Reason])
    end.
