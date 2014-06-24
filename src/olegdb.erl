%%% This is the main runtime of OlegDB. main/1 is where the magic happens.
-module(olegdb).
-include("olegdb.hrl").
-export([main/0, main/1, request_handler/1, do_accept/1]).

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 8080).
-define(ACCEPTOR_POOL_NUM, 64).

% Recurring tasks:
-define(COMPACTION_INTERVAL, 60 * 10000). % Five minutes
-define(SYNC_INTERVAL, 10 * 1000). % 10 Seconds

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
    Resp = ol_route:route(Data, Accepted),
    case gen_tcp:send(Accepted, Resp) of
        {error, Reason} ->
            io:format("[-] Could not send to socket: ~p~n", [Reason]);
        _ -> ok
    end,
    ok = gen_tcp:close(Accepted).

supervise() ->
    %% Eventually this function will do something interesting.
    %% We just sit here and block so that the parent process doesn't die
    %% while it's children are off living fulfilling lives.
    receive
        {shutdown, From} ->
            io:format("[-] Telling port driver to shut down.~n"),
            io:format("[-] No.~n"),
            olegdb_port_driver ! {shutdown, self()},
            receive
                {ok, _} ->
                    From ! {ok, self()},
                    io:format("[-] Night night.~n"),
                    halt()
            end;
        {compact} -> % We don't really care who the message is from.
            %io:format("[-] Compacting...~n"),
            ol_database:ol_squish(),
            %io:format("[-] Done compacting.~n"),
            erlang:send_after(?COMPACTION_INTERVAL, satan, {compact});
        {fsync} ->
            % Sync every ten seconds
            ol_database:ol_sync(),
            erlang:send_after(?SYNC_INTERVAL, satan, {fsync});
        X -> io:format("[-] Receieved message: ~p~n", [X])
    end,
    supervise().

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

            % Setup the compaction routine:
            erlang:send_after(?COMPACTION_INTERVAL, satan, {compact}),
            % Setup the fsync routine:
            erlang:send_after(?SYNC_INTERVAL, satan, {fsync}),

            supervise();
        _ ->
            io:format("[X] Specified database directory does not exist.~n"),
            exit(location_does_not_exst)
    end.
