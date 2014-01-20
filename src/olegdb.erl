-module(olegdb).
-compile(export_all).

-define(LISTEN_PORT, 8080).

server_manager(Port) ->
    case gen_tcp:listen(Port, [list, {active, false}, {reuseaddr, true}]) of
        {ok, Sock} ->
            do_accept(Sock);
        {error, Reason} ->
            io:format("[X] Could not listen: ~p~n", [Reason])
    end.

do_accept(Sock) ->
    case gen_tcp:accept(Sock) of
        {ok, _} ->
            io:format("[-] Connection accepted!~n"),
            do_accept(Sock);
        {error, _} ->
            io:format("[X] Could not accept a connection.~n")
    end.

main() ->
    io:format("[-] Starting server.~n"),
    server_manager(?LISTEN_PORT).
