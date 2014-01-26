Project Oleg
============

Alternate title: "How far can we push a mayonnaise metaphor?"

````
$ pgrep olegdb | xargs kill
olegdb: No.
````

OlegDB is a single-threaded, non-concurrent, transactionless NoSQL database
written by bitter SQL-lovers in a futile attempt to hop on the schemaless trend
before everyone realizes it was a bad move.

Dependencies
============

* A healthy fear of the end

Installation
============

OlegDB consists of a server written in Erlang and a C library for all of the
heavy lifting. Binaries are in `build/bin/` and the library is in `build/lib/`.
Beam files are also thrown in `build/bin/`.

```bash
# Building everything:
make
# Just the erlang beam files:
make erlang
# Just the C library:
make liboleg
```

To run tests:

```bash
./run_tests.sh
```

To run the erlang server:

```bash
./run_server.sh
```

curl2sudo® install script coming soon.

Dumps
=====

Olegdb dumps are binary.

There is a header struct in [dump.h](./include/dump.h) that is written to the
beginning of the file.

10 bytes -> OLEGDBDUMP  
3 bytes   -> Version number ascii  
4 bytes   -> Record count  
n bytes   -> data (keys and values)

Keys are always 16 bytes, so you read 16 bytes after reading the number of keys
to get the first bucket key. Following that is a size_t (8 bytes) value that is
the size of the data. We will call this data_size. Read data_size. This is the
key's data. Continue the process until Record count is exhausted.

Roadmap
=======

0.1
---

* Volatile in memory
* Speaks HTTP with a heavy accent
* ACID Compliance
* 16 byte hard keysize limit
* Single-digit concurrency
* Discardable soft mutexes
* Non-guaranteed record expiry
* Silent sharding
* Pre-computed hashes

0.2
---
* RAR storage for minimal storage

1.0
---

* WINRAR and WINE

2.0
---
* BSP Export
