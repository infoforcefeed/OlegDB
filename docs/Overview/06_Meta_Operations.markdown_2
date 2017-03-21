OlegDB supports a handful of meta-operations. These operations are usually
prefixed with a single underscore, and do some additional work compared to the
usual get/set operations. At this point, if you've been following along, you've
already seen a handful including `_all`, `_match`, `_prev` and `_next`.
This document is intended to give you more detailed explanations for all of
the currently supported meta-operators that haven't been previously covered.

## `_info`

`_info` spits out some additional meta data on a key that relates to the
database at large. As an example:

```
$ curl -v localhost:38080/oleg/x/_info
&gt; GET /oleg/x/_info HTTP/1.1
&gt; User-Agent: curl/7.35.0
&gt; Host: localhost:38080
&gt; Accept: */*
&gt;
&lt; HTTP/1.1 200 OK
&lt; Expires: 1423445413
&lt; X-Olegdb-Rcrd-Cnt: 1
&lt; Date: Sun, 08 Feb 2015 01:31:22 GMT
&lt; Content-Length: 2
&lt; Content-Type: text/plain; charset=utf-8

```

So here you can see some useful information. We know that the `oleg` database
has exactly one record in it, and now we know that the key will expire whenever
1423445413 happens. The `X-OlegDB-Rcrd-Cnt` header tells us how many records
exist in this database.

## `_uptime`

`_uptime` simply gives you the amount of time, in seconds, that a given database
has been up. So if we wanted to check the uptime of the previous example's
`oleg` database:

```
$ curl -v localhost:38080/oleg/_uptime
&gt; GET /oleg/_uptime HTTP/1.1
&gt; User-Agent: curl/7.35.0
&gt; Host: localhost:38080
&gt; Accept: */*
&gt;
&lt; HTTP/1.1 200 OK
&lt; Date: Sun, 08 Feb 2015 01:36:57 GMT
&lt; Content-Length: 3
&lt; Content-Type: text/plain; charset=utf-8
&lt;
482
```

It's been up 482 seconds.
