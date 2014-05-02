Contributing
============

Contributing is easy. We're on github, obviously, so if you want to contribute
just fork the repository, make your changes and send us
a pull request.

If it passes tests, code review and any other arbitrary constraints it comes up
with, we'll add your name and your code to the repository.

What needs to be done?
======================

We have a rather large amount of [issues](https://github.com/infoforcefeed/OlegDB/issues)
in the github issue-tracking system.
The general rule appears to be if it's not assigned, it's fair game. Some
tickets have a larger scope than others and this isn't really defined anywhere.
If the ticket is just really vague but seems important, it's probably a
discussion thing.

Of course, contributions should not be limited to the ticketing system. If you
want to add support for something or a random feature, go ahead. The worst we
can do is belittle you and your code (we probably won't, though).

General Guidelines
==================

Code Style
----------

* Use C-style comments (`/* ... */`). This is an entirely arbitrary decision. I just don't want
  the github language processing thing thinking we have a C++ project.
* Try to keep lines under 79 columns. This is not a hard rule, it's more like a
  goal. If it breaks the congruency and readability of your code, don't bother
with this one.
* Keep functions small. Theres no hard limit for this one either, you can
  usually just tell when a function is too large.
* Stay POSIX compliant. No weird `#define _GNU_SOURCE` stuff.
* Avoid code duplication. This one is important. If you're copying and pasting
  code, refactor it out to a common function. Code duplication causes bugs.
* Make sure your code is clear. This means comments and clear variable names. If
  we see stuff like:
```C
ol_bucket *c, *v, *u, *zx;
````
We're probably not going to accept that pull request.

Architecture Decisions
----------------------

Communication is key. If you're making a rather big architecture decision,
discuss it with others. A second pair of eyes often does wonders to reveal weird
or bad ideas.

Code Review
-----------

All code should be code-reviewed by at least one other person before being
merged into master. Master is considered stable and should always pass tests.
Ping @qpfiffer for quick commentary.

Documentation
-------------

This one is kind of tough. We autogenerate documentation for the OlegDB website
via a custom templating language (hahaha). Generally, if it's in `oleg.h` it's
going to need to be documented with the `xXx KEY=VALUE xXx` syntax. You can see
many examples of this in that file.

Most other files you don't have to worry about as they're external. That is not
to say that we don't want or need documentation in these files, but a simple
comment will suffice.
