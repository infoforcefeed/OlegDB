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
-export([start/0, init/0, ol_jar/1, ol_unjar/1, ol_scoop/1]).

-include("olegdb.hrl").
-define(SHAREDLIB, "libolegserver").

start() ->
    code:add_path("./build/lib/"),
    case erl_ddll:load_driver("./build/lib/", ?SHAREDLIB) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, ErrorDesc} -> exit(erl_ddll:format_error(ErrorDesc))
    end,
    spawn(fun() -> ?MODULE:init() end).

init() ->
    register(complex, self()),
    Port = open_port({spawn, ?SHAREDLIB}, [binary]),
    loop(Port).

encode({ol_jar, X}) -> [1, X];
encode({ol_unjar, Y}) -> [2, Y];
encode({ol_scoop, Z}) -> [3, Z];
encode(_) ->
    io:format("Don't know how to decode that.~n"),
    exit(unknown_call).

loop(Port) ->
    %% Wait for someone to call for something
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

ol_jar(OlRecord) ->
    if
        byte_size(OlRecord#ol_record.value) > 0 ->
            call_port({ol_jar, term_to_binary(OlRecord)});
        true -> {error, no_data_posted}
    end.

ol_unjar(OlRecord) ->
    call_port({ol_unjar, term_to_binary(OlRecord)}).

ol_scoop(OlRecord) ->
    call_port({ol_scoop, term_to_binary(OlRecord)}).