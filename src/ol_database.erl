%%% This is mainly where communcation with liboleg happens. You will
%%% see functions dealing with encoding, internal calls and the port
%%% driver interface.
-module(ol_database).
-export([start/0,
         init/1,
         ol_init/1,
         ol_jar/1,
         ol_unjar/1,
         ol_bucket_meta/1,
         ol_next_key/1,
         ol_prev_key/1,
         ol_first_key/1,
         ol_last_key/1,
         ol_squish/0,
         ol_sync/0,
         ol_prefix_match/1,
         ol_scoop/1]).

-include("olegdb.hrl").
-define(SHAREDLIB, "libolegserver").

start() ->
    code:add_path(?LIBLOCATION),
    case erl_ddll:load_driver(?LIBLOCATION, ?SHAREDLIB) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, ErrorDesc} -> exit(erl_ddll:format_error(ErrorDesc))
    end,
    %% Spawn of the looper
    Me = self(),
    spawn(fun() -> ?MODULE:init(Me) end),
    receive
        spawned -> ok
    end.

init(Papa) ->
    register(olegdb_port_driver, self()),
    Port = open_port({spawn, ?SHAREDLIB}, [binary]),
    Papa ! spawned,
    loop(Port).

encode({ol_init, X})  ->        [0, term_to_binary(X)];
encode({ol_jar, X})   ->        [1, term_to_binary(X)];
encode({ol_unjar, X}) ->        [2, term_to_binary(X)];
encode({ol_scoop, X}) ->        [3, term_to_binary(X)];
encode({ol_bucket_meta, X}) ->  [4, term_to_binary(X)];
encode({ol_next_key, X}) ->     [5, term_to_binary(X)];
encode({ol_prev_key, X}) ->     [6, term_to_binary(X)];
encode({ol_first_key, X}) ->    [7, term_to_binary(X)];
encode({ol_last_key, X}) ->     [8, term_to_binary(X)];
encode({ol_squish}) ->          [9];
encode({ol_prefix_match, X}) -> [10, term_to_binary(X)];
encode({ol_sync}) ->            [11];
encode(_) ->
    io:format("Don't know how to decode that.~n"),
    exit(unknown_call).

%% TODO: Keep some kind of 'last-compacted' state here so we avoid having
%% unnecessary IO.
loop(Port) ->
    %% Wait for someone to call for something
    %io:format("Queue size: ~p~n", [erlang:process_info(self(), message_queue_len)]),
    receive
        {call, Caller, Msg} ->
            %% Send that to liboleg
            Port ! {self(), {command, encode(Msg)}},
            receive
                %% Give the caller our result
                {Port, {data, Data}} ->
                    Caller ! {olegdb_port_driver, binary_to_term(<<Data/binary>>)};
                badarg ->
                    io:format("Badarg ~n"),
                        exit(port_terminated)
            end,
            loop(Port);
        {shutdown, From} ->
            io:format("[-] Shutting down.~n"),
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    From ! {ok, self()},
                    loop(Port)
            end;
        {'EXIT', Port, Reason} ->
            io:format("~p ~n", [Reason]),
                exit(port_terminated)
    end.

call_port(Msg) ->
    olegdb_port_driver ! {call, self(), Msg},
    receive
        {olegdb_port_driver, Result} ->
            Result
    end.

ol_init(DbLocation) ->
    call_port({ol_init, DbLocation}).

ol_jar(OlRecord) ->
    %io:format("[-] Expiration at this point: ~p~n", [OlRecord#ol_record.expiration_time]),
    if
        byte_size(OlRecord#ol_record.value) > 0 ->
            call_port({ol_jar, OlRecord});
        true -> {error, <<"No data posted.">>}
    end.

ol_unjar(OlRecord) ->
    call_port({ol_unjar, OlRecord}).

ol_scoop(OlRecord) ->
    call_port({ol_scoop, OlRecord}).

ol_bucket_meta(OlRecord) ->
    call_port({ol_bucket_meta, OlRecord}).

ol_next_key(OlRecord) ->
    call_port({ol_next_key, OlRecord}).

ol_prev_key(OlRecord) ->
    call_port({ol_prev_key, OlRecord}).

ol_first_key(OlRecord) ->
    call_port({ol_first_key, OlRecord}).

ol_last_key(OlRecord) ->
    call_port({ol_last_key, OlRecord}).

ol_squish() ->
    call_port({ol_squish}).

ol_prefix_match(OlRecord) ->
    call_port({ol_prefix_match, OlRecord}).

ol_sync() -> call_port({ol_sync}).
