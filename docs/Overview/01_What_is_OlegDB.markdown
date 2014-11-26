OlegDB is a concurrent, pretty fast K/V hashtable with an Go frontend.
It uses the Murmur3 hashing algorithm to hash and index keys. We chose
Go for the server because it is easy to rapidly create an HTTP frontend that is
performant and has all the tools in core to prevent race conditions.

In addition to this, liboleg is a C library that powers everything. liboleg exports
a relatively simplistic API for use in other applications. We build the main
database off of this library.
