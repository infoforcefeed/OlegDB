Installing OlegDB is pretty simple, you only need a POSIX compliant system,
make, gcc/clang (thats all we test) and Erlang. You'll also need the source
code for OlegDB.

Once you have your fanciful medley of computer science tools, you're ready to
dive into a lengthy and complex process of program compilation. Sound
foreboding? Have no fear, people have been doing this for at least a quarter of
a century.

I'm going to assume you've extracted the source tarball into a folder called
`~/src/olegdb` and that you haven't `cd 'd into it yet. Lets smash some electrons
together:

````
$ cd ~/src/olegdb
$ make
$ sudo make install
````

If you really wanted to, you could specify a different installation directory.
The default is `/usr/local`. You can do this by setting the `PREFIX` variable
before compilation:

````
$ sudo make PREFIX=/usr/ install
````

Actually running OlegDB and getting it do stuff after this point is trivial, if
your installation prefix is in your `PATH` variable  you should just be able to run
something like the following:

````
$ olegdb <data_directory>
````

...where `<data_directory>` is the place you want OlegDB to store persistent data. 
Make it `/dev/null` if you want, I don't care. You can also specify
IP/port information from the commandline:

````
$ olegdb /tmp 1978 #Starts OlegDB listening on port 1978
$ olegdb /tmp 0.0.0.0 1337 #Starts OlegDB listening on the 0.0.0.0 IP, with port 1337
$ olegdb /tmp data.shithouse.tv 666 #Hostnames work too
````
