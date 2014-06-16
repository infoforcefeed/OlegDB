Communicating with OlegDB is done via a pretty simple REST interface.
You POST to create/update records, GET to retrieve them, DELETE to delete,
and HEAD to get back some information about them. Probably.

For example, to store the value Raphael into the named database turtles under
the key red you could use something like the following:

````
$ curl -X POST -d 'Raphael' http://localhost:8080/turtles/red
````

Retrieving data is just as simple:

````
$ curl http://localhost:8080/turtles/red
````

Deleting keys can be done by using DELETE:

````
$ curl -X DELETE http://localhost:8080/turtles/red
````

You can also tell Oleg what the `Content-Type` of that key's value is:

````
$ curl -X POST -H "Content-Type: text/html" -d '<p>Raphael</p>' http://localhost:8080/turtles/red
````

OlegDB supports lazy key expiration. You can specify an expiration date by setting the
`X-OlegDB-use-by` header to a *UTC* POSIX timestamp .

````
$ curl -X POST \
-H "X-OlegDB-use-by: $(date +%s)" \
-H "Content-Type: application/json" \
-d '{turtle: "Johnny", age: 34}' http://localhost:8080/turtles/Johnny
> POST /turtles/Johnny HTTP/1.1
> User-Agent: curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0
> Host: localhost:8080
> Accept: */*
> X-OlegDB-use-by: 1394323192
> Content-Type: application/json
> Content-Length: 27
> 
* upload completely sent off: 27out of 27 bytes
< HTTP/1.1 200 OK
< Server: OlegDB/fresh_cuts_n_jams
< Content-Type: text/plain
< Connection: close
< Content-Length: 7
<
無駄

$ curl -v http://localhost:8080/turtles/Johnny
> GET /turtles/Johnny HTTP/1.1
> User-Agent: curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0
> Host: localhost:8080
> Accept: */*
>
< HTTP/1.1 404 Not Found
< Status: 404 Not Found
< Server: OlegDB/fresh_cuts_n_jams
< Content-Length: 26
< Connection: close
< Content-Type: text/plain
<
These aren't your ghosts.
````

As you can hopefully tell, the POST succeeds and a 200 OK is returned. We
used the bash command `date +%s` which returns a timestamp. Then, immediately
trying to access the key again results in a 404, because the key expired.

If you want to retrieve the expiration date of a key, you can do so by sending HEAD:

````
$ curl -v -X HEAD http://localhost:8080/turtles/Johnny
> HEAD /turtles/Johnny HTTP/1.1
> User-Agent: curl/7.35.0
> Host: localhost:8080
> Accept: */*
>
< HTTP/1.1 200 OK
* Server OlegDB/fresh_cuts_n_jams is not blacklisted
< Server: OlegDB/fresh_cuts_n_jams
< Content-Length: 0
< Content-Type: application/json
< Expires: 1395368972
<
````

