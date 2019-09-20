OlegDB
============

<img src="http://olegdb.org/static/img/olegdb_stars.png" />

[![Build Status](https://travis-ci.org/infoforcefeed/OlegDB.svg?branch=master)](https://travis-ci.org/infoforcefeed/OlegDB)
![OlegDB MAYO](http://b.repl.ca/v1/OlegDB-MAYO-brightgreen.png)
[![Scan Status](https://scan.coverity.com/projects/1414/badge.svg)](https://scan.coverity.com/projects/1414)

Alternate title: "How far can we push a mayonnaise metaphor?"

````
$ pgrep olegdb | xargs kill
olegdb: No.
````

OlegDB is a ~~single-threaded, non-concurrent, transactionless~~ NoSQL
~~database~~ datastore
written by bitter SQL-lovers in a futile attempt to hop on the schemaless trend
before everyone realizes it was a bad move. It is primarily a C library with a
Go frontend for communication.

Dependencies
============

* A healthy fear of the end
* Go (>= 1.1)

Installation
============

OlegDB consists of a server written in Go and a C library for all of the
heavy lifting. Binaries are in `build/bin/` and the `liboleg` library is in `build/lib/`.

Currently builds are tested against gcc and clang.

```bash
# Building everything:
make
# Just the C library:
make liboleg
# Make and run tests:
make test
# Install
sudo make install
```

Note that BSD users may have to use `gmake` where applicable.

To run tests:

```bash
./run_tests.sh
```

To run the Go server:

```bash
olegdb [-conf olegdb.conf] [-bind localhost:8080] [-dir data]
```
For an explanation of the command line parameters, do `olegdb -h`

curl2sudo&reg; install script coming soon.

Un-Installation
============

`sudo make uninstall`

Documentation
=============

Documentation can be found on the [the website](https://olegdb.org/documentation.html).

Roadmap
=======

Roadmap is full of lies and half-truths, please ignore.

- [ ] Witch hunt
- [ ] Wordard generation
- [ ] Feeding tube integration
- [ ] Being more stable than redis
