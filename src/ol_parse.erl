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
-module(ol_parse).
-include("olegdb.hrl").
-export([parse_get/1, parse_post/1]).

parse_get(Data) ->
    case binary:split(Data, [<<"\r\n\r\n">>]) of
        [Header|_ ] -> parse_header(Header);
        X -> X
    end.

parse_post(Data) ->
    case binary:split(Data, [<<"\r\n\r\n">>]) of
        [Header|PostedData] ->
            parse_header(Header, #ol_record{value=PostedData});
        X -> X
    end.

%% Same as parse_header/2, but with a default record.
parse_header(Header) -> parse_header(Header, #ol_record{}).
parse_header(Header, Record) ->
    parse_header1(binary:split(Header, [<<"\r\n">>], [global]), Record).

%% Tail recursive function that maps over the lines in a header to fill out
%% an ol_record to later.
parse_header1([], Record) -> Record;
parse_header1([Line|Header], Record) ->
    case Line of
        <<"Content-Length: ", CLength/binary>> ->
            Len = list_to_integer(binary_to_list(CLength)),
            parse_header1(Header, Record#ol_record{content_length=Len});
        <<"Content-Type: ", CType/binary>> ->
            parse_header1(Header, Record#ol_record{content_type=binary_to_list(CType)});
        _ ->
            parse_header1(Header, Record)
    end.
