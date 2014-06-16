OlegDB is a concurrent, pretty fast K/V hashtable with an Erlang frontend.
It uses the Murmur3 hashing algorithm to hash and index keys. We chose
Erlang for the server because it is functional, uses the actor model and 
the pattern matching is ridiculous.

In addition to this, liboleg is a C library that powers everything. liboleg exports
a relatively simplistic API for use in other applications. We build the main
database off of this library.
