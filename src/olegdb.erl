-module(olegdb).
-include("olegdb.hrl").
-export([main/0]).

-define(LISTEN_PORT, 8080).

server_manager(Port) ->
    case gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]) of
        {ok, Sock} ->
            io:format("[-] Listening on port ~p~n", [?LISTEN_PORT]),
            do_accept(Sock);
        {error, Reason} ->
            io:format("[X] Could not listen: ~p~n", [Reason])
    end.

%% Responsible for accepting new connections and spawning request handlers.
do_accept(Sock) ->
    case gen_tcp:accept(Sock) of
        {ok, Accepted} ->
            io:format("[-] Connection accepted!~n"),
            spawn(fun() -> request_handler(Accepted) end),
            do_accept(Sock);
        {error, _} ->
            io:format("[X] Could not accept a connection.~n")
    end.

request_handler(Accepted) ->
    % Read in all data, timeout after 60 seconds
    case gen_tcp:recv(Accepted, 0, 60000) of
        {ok, Data} ->
            gen_tcp:send(Accepted, route(Data)),
            request_handler(Accepted);
        {error, closed} ->
            ok;
        {error, timeout} ->
            io:format("[-] Client timed out.~n"),
            ok
    end.

route(Bits) -> 
    case Bits of
        <<"GET", _/binary>> ->
            io:format("[-] Header: ~p~n", [parsing:parse_get(Bits)]),
            "Get request.";
        <<"POST", _/binary>> ->
            io:format("[-] Header: ~p~n", [parsing:parse_post(Bits)]),
            "Post request";
        _ ->
            "These aren't your ghosts."
    end.

main() ->
    io:format("[-] Starting server.~n"),
    server_manager(?LISTEN_PORT).
