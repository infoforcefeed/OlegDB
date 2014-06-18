In addition to [cursor iteration](#cursor_iteration) `0.1.2` added the ability
to return the values of keys that match a given prefix. Use of this feature
follows the same URL layout as it's predeccesors, mainly via the use of the
`_match` qualifier.

For example, say I have three keys in the database, `test_a`, `test_b` and
`test_c`. I can easily find the values of all of these keys in one operation by
using the `_match` operand. To demonstrate:

````
$ curl -v localhost:8080/oleg/test/_match
&gt; GET /oleg/test/_match HTTP/1.1
&gt; User-Agent: curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1
&gt; zlib/1.2.3.4 libidn/1.23 librtmp/2.3
&gt; Host: localhost:8080
&gt; Accept: */*
&gt;
&lt; HTTP/1.1 200 OK
&lt; Server: OlegDB/fresh_cuts_n_jams
&lt; Content-Type: application/json
&lt; Content-Length: 25
&lt; Connection: close
&lt; X-OlegDB-Num-Matches: 3
&lt;
["test3","test2","test1"]
````

This returns a JSON-encoded list of all of your values. Also of note is the
`X-OlegDB-Num-Matches` header which specifies the number of keys that matched
the given prefix.

If no matches are present, a 404 is returned.
