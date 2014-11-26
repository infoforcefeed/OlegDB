Installing OlegDB is pretty simple, you only need a POSIX compliant system,
make, gcc/clang (thats all we test) and Go. You'll also need the source
code for OlegDB.

Once you have your fanciful medley of computer science tools, you're ready to
dive into a lengthy and complex process of program compilation. Sound
foreboding? Have no fear, people have been doing this for at least a quarter of
a century.

I'm going to assume you've extracted the source tarball into a folder called
`~/src/olegdb` and that you haven't cd'd into it yet. Lets smash some electrons
together:

```bash
$ cd ~/src/olegdb
$ make
$ sudo make install
```

If you really wanted to, you could specify a different installation directory.
The default is `/usr/local`. You can do this by setting the `PREFIX` variable
before compilation:

```bash
$ sudo make PREFIX=/usr/ install
```

Actually running OlegDB and getting it do stuff after this point is trivial, if
your installation prefix is in your `PATH` variable  you should just be able to run
something like the following:

```bash
$ olegdb -config /path/to/json/config
```
