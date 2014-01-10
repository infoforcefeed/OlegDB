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

```bash
make
./olegdb
```

To run tests:

```bash
./olegdb test
```

curl2sudo® install script coming soon.

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
