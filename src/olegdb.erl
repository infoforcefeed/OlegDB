%%% This is the main runtime of OlegDB. main/1 is where the magic happens.
-module(olegdb).
-include("olegdb.hrl").
-export([main/0, main/1, request_handler/1, route/2, do_accept/1]).

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 8080).
-define(ACCEPTOR_POOL_NUM, 64).

server_manager(Caller) ->
    server_manager(Caller, ?DEFAULT_HOST, ?DEFAULT_PORT).

server_manager(Caller, Port) ->
    server_manager(Caller, ?DEFAULT_HOST, Port).

server_manager(Caller, Hostname, Port) ->
    {ok, Ip} = inet:getaddr(Hostname, inet),
    case gen_tcp:listen(Port, [binary, {ip, Ip}, {active, false}, {reuseaddr, true}, {nodelay, true}, {backlog, 100}]) of
        {ok, Sock} ->
            io:format("[-] Listening on IP ~p, port ~p~n", [Ip, Port]),
            gen_tcp:controlling_process(Sock, Caller),
            % Spawn our acceptor pool workers
            [spawn(?MODULE, do_accept, [Sock]) || _ <- lists:seq(0, ?ACCEPTOR_POOL_NUM)];
        {error, Reason} ->
            io:format("[X] Could not listen: ~p~n", [Reason])
    end.

%% Responsible for accepting new connections and spawning request handlers.
do_accept(Sock) ->
    case gen_tcp:accept(Sock) of
        {ok, Accepted} ->
            %io:format("[-] Connection accepted!~n"),
            spawn(?MODULE, request_handler, [Accepted]),
            do_accept(Sock);
        {error, Error} ->
            io:format("[X] Could not accept a connection. Error: ~p~n", [Error]);
        X -> X
    end.

request_handler(Accepted) ->
    % Read in all data, timeout after 60 seconds
    case gen_tcp:recv(Accepted, 0, 60000) of
        {ok, Data} ->
            send_handler(Data, Accepted);
        {error, closed} -> ok;
        {error, timeout} ->
            io:format("[-] Client timed out.~n"),
            ok
    end.

send_handler(Data, Accepted) ->
    Resp = route(Data, Accepted),
    case gen_tcp:send(Accepted, Resp) of
        {error, Reason} ->
            io:format("[-] Could not send to socket: ~p~n", [Reason]);
        _ -> ok
    end,
    ok = gen_tcp:close(Accepted).

route(Bits, Socket) ->
    case ol_parse:parse_http(Bits) of
        {ok, _, {Header, [send_100|_]}} ->
            hundred_handler(Header, Socket);
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
            io:format("[-] Error ~p~n", [ErrMsg]),
            ol_http:error_response(ErrMsg);
        X ->
            io:format("[-] Somebody requested something weird: ~p~n", [X]),
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


supervise() ->
    %% Eventually this function will do something interesting.
    %% We just sit here and block so that the parent process doesn't die
    %% while it's children are off living fulfilling lives.
    receive
        {shutdown, From} ->
            olegdb_port_driver ! {shutdown, self()},
            receive
                {ok, _} ->
                    From ! {ok, self()},
                    io:format("[-] Night night.~n"),
                    halt()
            end;
        X -> io:format("[-] Receieved message: ~p~n", [X]),
            supervise()
    end.

main() -> main([]).
main([]) ->
    io:format("[X] You must specify a location to store aol/dump files.~n"),
    exit(not_enough_args);
main([DbLocation|Args]) ->
    case filelib:is_dir(DbLocation) of
        true ->
            io:format("[-] Starting server.~n"),
            ol_database:start(),
            ol_database:ol_init(DbLocation),
            io:format("[-] Args: ~p~n", [Args]),
            case Args of
                [] -> server_manager(self());
                [Port] ->
                    {PortNum, _} = string:to_integer(Port),
                    server_manager(self(), PortNum);
                [Hostname, Port] ->
                    {PortNum, _} = string:to_integer(Port),
                    server_manager(self(), Hostname, PortNum)
            end,
            register(satan, self()),
            io:format("[-] Death spiral. Running as ~p~n", [node()]),
            supervise();
        _ ->
            io:format("[X] Specified database directory does not exist.~n"),
            exit(location_does_not_exst)
    end.
