In `0.1.2`, we added the ability to iterate through keys inserted into the
database via the frontend. It's a pretty simple interface and follows the rest
of the current URL idioms.

Each cursor operand is of the form `/database/key/operand`. In some
operands (`_last` and `_first`) the `key` option is the operand. Using them is
trivial.

Using any of the operands will return both the value of the key you requested
(`_next` will return the next value, `_prev` will return the previous value, etc.)
and the HTTP header `X-Olegdb-Key` followed by the key paired to the value you
just retrieved. For example, say we have two keys in the database, `aaa` and
`bbb`. To begin with, I can request the first key in the database:

````
$ curl -i localhost:8080/oleg/_first
HTTP/1.1 200 OK
X-Olegdb-Key: aaa
Date: Sun, 28 Sep 2014 07:23:39 GMT
Content-Length: 21
Content-Type: text/plain; charset=utf-8

I am the value of aaa
````

As you can see, the key `aaa` is the first one in the tree of ordered keys. If
you're paying attention, you've also noticed that I've omitted the parameter
between the `database` specifier and the cursor operand `_first`. This is
because the `key` is not used in this command. It will, however, be used in
the next:

````
$ curl -i localhost:8080/oleg/aaa/_next
HTTP/1.1 200 OK
X-Olegdb-Key: bbb
Date: Sun, 28 Sep 2014 07:24:16 GMT
Content-Length: 21
Content-Type: text/plain; charset=utf-8

I am the value of bbb
````

Logically, the key `bbb` follows the key `aaa`. Nice. In our request, we asked
Oleg for the key after (`_next`) the key `aaa`. The value of the next key was
returned, and we can see the header `X-OlegDB-Key` is set to the key that
corresponds to that value. Lets see what happens if we try to get the next key,
knowing that we only have two keys (`aaa` and `bbb`) in our database:

````
$ curl -i localhost:8080/oleg/bbb/_next
HTTP/1.1 404 Not Found
Content-Type: text/plain; charset=utf-8
Date: Sun, 28 Sep 2014 07:24:26 GMT
Content-Length: 17

No records found
````

We get a 404 statuscode and a message to match. This informs us that we cannot
iterate any farther and that we have reached the end of the list.

In addition to these to commands, you can use `_last` to find the last key in
the database and `_prev` to iterate backwards. The usage of these commands is
identical to those above:

````
$ curl -i localhost:8080/oleg/_last
HTTP/1.1 200 OK
X-Olegdb-Key: bbb
Date: Sun, 28 Sep 2014 07:24:50 GMT
Content-Length: 21
Content-Type: text/plain; charset=utf-8

I am the value of bbb

$ curl -i localhost:8080/oleg/bbb/_prev
HTTP/1.1 200 OK
X-Olegdb-Key: aaa
Date: Sun, 28 Sep 2014 07:25:06 GMT
Content-Length: 21
Content-Type: text/plain; charset=utf-8

I am the value of aaa

$ curl -i localhost:8080/oleg/aaa/_prev
HTTP/1.1 404 Not Found
Content-Type: text/plain; charset=utf-8
Date: Sun, 28 Sep 2014 07:25:17 GMT
Content-Length: 17

No records found
````
