In addition to [cursor iteration](#cursor_iteration) `0.1.2` added the ability
to return the keys that match a given prefix. Use of this feature follows the
same URL layout as it's predeccesors, mainly via the use of the
`_match` qualifier.

For example, say I have three keys in the database, `test_a`, `test_b` and
`test_c`. I can easily find these keys in one operation by using the `_match`
operand. To demonstrate:

````
$ curl -i localhost:8080/oleg/test/_match
HTTP/1.1 200 OK
Date: Sun, 28 Sep 2014 07:26:35 GMT
Content-Length: 20
Content-Type: text/plain; charset=utf-8

test_a
test_b
test_c
````

This returns a list of all the keys separated by `\n`. Also of note is the
`X-Olegdb-Num-Matches` header which specifies the number of keys that matched
the given prefix.

If no matches are present, a 404 is returned.
