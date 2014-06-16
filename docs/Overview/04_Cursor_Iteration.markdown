In `0.1.2`, we added the ability to iterate through keys inserted into the
database via the frontend. It's a pretty simple interface and follows the rest
of the current idioms.

Each cursor operand is of the form `/<database>/<key>/<operand>`. In some
operands (\_last and \_first) the `<key>` option is ignored. Using them is
trivial.

Using any of the operands will return both the value of the key you requested
(\_next will return the next value, \_prev will return the previous value, etc.)
and the HTTP header `X-OlegDB-Key` followed by the key paired to the value you
just retrieved. For example, say we have two keys in the database, `aaa` and
`bbb`. To begin with, I can request the first key in the database:

````
$ curl -v localhost:8080/oleg//_first
&gt; GET /oleg//_first HTTP/1.1
&gt; User-Agent: curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1
&gt; zlib/1.2.3.4 libidn/1.23 librtmp/2.3
&gt; Host: localhost:8080
&gt; Accept: */*
&gt; 
&lt; HTTP/1.1 200 OK
&lt; Server: OlegDB/fresh_cuts_n_jams
&lt; Content-Type: application/x-www-form-urlencoded
&lt; Content-Length: 22
&lt; Connection: close
&lt; X-OlegDB-Key: aaa
&lt; 
* Closing connection #0
I am the value of aaa.
````

As you can see, the key `aaa` is the first one in the tree of ordered keys. If
you're paying attention, you've also noticed that I've ommitted the parameter
between the `<database>` specifier and the cursor operand `_first`. This is
because the `<key>` is not used in this command. It will, however, be used in
the next:

````
$ curl -v localhost:8080/oleg/aaa/_next
&gt; GET /oleg/aaa/_next HTTP/1.1
&gt; User-Agent: curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1
&gt; zlib/1.2.3.4 libidn/1.23 librtmp/2.3
&gt; Host: localhost:8080
&gt; Accept: */*
&gt; 
&lt; HTTP/1.1 200 OK
&lt; Server: OlegDB/fresh_cuts_n_jams
&lt; Content-Type: application/x-www-form-urlencoded
&lt; Content-Length: 22
&lt; Connection: close
&lt; X-OlegDB-Key: bbb
&lt; 
* Closing connection #0
I am the value of bbb.
````

Logically, the key `bbb` follows the key `aaa`. Nice. In our request, we asked
Oleg for the key after (`_next`) the key `aaa`. The value of the next key was
returned, and we can see the header `X-OlegDB-Key` is set to the key that
corresponds to that value. Lets see what happens if we try to get the next key,
knowing that we only have two keys (`aaa` and `bbb`) in our database:

````
$ curl -v localhost:8080/oleg/bbb/_next
&gt; GET /oleg/bbb/_next HTTP/1.1
&gt; User-Agent: curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1
&gt; zlib/1.2.3.4 libidn/1.23 librtmp/2.3
&gt; Host: localhost:8080
&gt; Accept: */*
&gt; 
&lt; HTTP/1.1 404 Not Found
&lt; Status: 404 Not Found
&lt; Server: OlegDB/fresh_cuts_n_jams
&lt; Content-Length: 26
&lt; Connection: close
&lt; Content-Type: text/plain
&lt; 
These aren't your ghosts.
````

We get a 404 statuscode and a message to match. This informs us that we cannot
iterate any farther and that we have reached the end of the list.

In addition to these to commands, you can use `_last` to find the last key in
the database and `_prev` to iterate backwards. The usage of these commands is
identical to those above:

````
$ curl -v localhost:8080/oleg//_last
&gt; GET /oleg//_last HTTP/1.1
&gt; User-Agent: curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1
&gt; zlib/1.2.3.4 libidn/1.23 librtmp/2.3
&gt; Host: localhost:8080
&gt; Accept: */*
&gt; 
&lt; HTTP/1.1 200 OK
&lt; Server: OlegDB/fresh_cuts_n_jams
&lt; Content-Type: application/x-www-form-urlencoded
&lt; Content-Length: 22
&lt; Connection: close
&lt; X-OlegDB-Key: bbb
&lt; 
I am the value of bbb.
$ curl -v localhost:8080/oleg/bbb/_prev
&gt; GET /oleg/bbb/_prev HTTP/1.1
&gt; User-Agent: curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1
&gt; zlib/1.2.3.4 libidn/1.23 librtmp/2.3
&gt; Host: localhost:8080
&gt; Accept: */*
&gt; 
&lt; HTTP/1.1 200 OK
&lt; Server: OlegDB/fresh_cuts_n_jams
&lt; Content-Type: application/x-www-form-urlencoded
&lt; Content-Length: 22
&lt; Connection: close
&lt; X-OlegDB-Key: aaa
&lt; 
I am the value of aaa.
$ curl -v localhost:8080/oleg/aaa/_prev
* About to connect() to localhost port 8080 (#0)
*   Trying 127.0.0.1... connected
&gt; GET /oleg/aaa/_prev HTTP/1.1
&gt; User-Agent: curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1
&gt; zlib/1.2.3.4 libidn/1.23 librtmp/2.3
&gt; Host: localhost:8080
&gt; Accept: */*
&gt; 
&lt; HTTP/1.1 404 Not Found
&lt; Status: 404 Not Found
&lt; Server: OlegDB/fresh_cuts_n_jams
&lt; Content-Length: 26
&lt; Connection: close
&lt; Content-Type: text/plain
&lt; 
These aren't your ghosts.
````
