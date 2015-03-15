Oleg supports a single bulk operation, which is a multi-key variant of the
`unjar` operation. Via the HTTP API, you POST a list of `\n` delimited values to
whatever database you want to retrieve values from, and you'll receieve the keys
back for your trouble.

Response format is a list of size, value pairs where the size is a zero-padded
64-bit integer representing the number of bytes following the size. So for
example if I had two values in the database `test`: `x` and `y`:

```
$ curl -v -d 'x
z
y
y
y
y
' localhost:38080/test/_bulk_unjar
* Connected to localhost (127.0.0.1) port 38080 (#0)
&gt; POST /test/_bulk_unjar HTTP/1.1
&gt; Host: localhost:38080
&gt; Content-Type: application/x-www-form-urlencoded
&gt; 
* upload completely sent off: 12 out of 12 bytes
&lt; HTTP/1.1 200 OK
&lt; Date: Sun, 15 Mar 2015 17:30:40 GMT
&lt; Content-Length: 149
&lt; Content-Type: text/plain; charset=utf-8
&lt; 
* Connection #0 to host localhost left intact
00000009THIS IS X0000000000000021THAT WAS X, THIS IS Y00000021THAT WAS X, THIS
IS Y00000021THAT WAS X, THIS IS Y00000021THAT WAS X, THIS IS Y00000000
```

So a couple of things happened here, first of all we requested multiple copies
of `y`. Thats fine.

Second, we request `z`, which is not a value in the database. This returned the
value `00000000` which fits the format, has no actual value and just keeps on
going to the next key.

Parsing is up to you, sky captain.
