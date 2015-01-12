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

Similar to prefix matching, you can also just dump the entire keyspace using
'_all', keep in mind however that this can be an expensive operation.

````
$ curl localhost:38080/waifu/_all | head
HTTP/1.1 200 OK
Content-Length: 27863
X-Olegdb-Num-Matches: 401
Date: Sun, 11 Jan 2015 21:26:08 GMT
Content-Type: text/plain; charset=utf-8

alias50B224D2C7987CE4F51E9258707758841771C82E9A0D3395C849426F6E93B8A85FE94AB42A00845C
alias70170858147E2B26DD5370D9F97113E0D7FDA993A707D5B0304272E93BA9A031372339E4C8F94AA2
alias70170858147E2B26DD5370D9F97113E0D7FDA993A707D5B0304272E93BA9A031383CEF2534DF870A
alias70170858147E2B26DD5370D9F97113E0D7FDA993A707D5B0304272E93BA9A031717594C273021004
...
````
