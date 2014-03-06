%%% The MIT License (MIT)
%%% 
%%% Copyright (c) 2014 Quinlan Pfiffer, Kyle Terry
%%% 
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
-module(ol_database).
-export([start/0, init/1, ol_init/1, ol_jar/1, ol_unjar/1, ol_scoop/1]).

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
    register(complex, self()),
    Port = open_port({spawn, ?SHAREDLIB}, [binary]),
    Papa ! spawned,
    loop(Port).

encode({ol_init, X})  -> [0, term_to_binary(X)];
encode({ol_jar, X})   -> [1, term_to_binary(X)];
encode({ol_unjar, X}) -> [2, term_to_binary(X)];
encode({ol_scoop, X}) -> [3, term_to_binary(X)];
encode(_) ->
    io:format("Don't know how to decode that.~n"),
    exit(unknown_call).

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
                    Caller ! {complex, binary_to_term(Data)};
                badarg ->
                    io:format("Badarg ~n"),
                        exit(port_terminated)
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            io:format("~p ~n", [Reason]),
                exit(port_terminated)
    end.

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
        {complex, Result} ->
            Result
    end.

ol_init(DbLocation) ->
    call_port({ol_init, DbLocation}).

ol_jar(OlRecord) ->
    if
        byte_size(OlRecord#ol_record.value) > 0 ->
            call_port({ol_jar, OlRecord});
        true -> {error, no_data_posted}
    end.

ol_unjar(OlRecord) ->
    call_port({ol_unjar, OlRecord}).

ol_scoop(OlRecord) ->
    call_port({ol_scoop, OlRecord}).
