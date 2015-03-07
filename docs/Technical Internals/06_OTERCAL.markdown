OTERCAL
=======

This specification's use of the keywords "must", "must not", "required",
"shall", "shall not", "should", "should not", "recommended", "not recommended",
"may", and "optional" are to be interpreted as described in RFC2119.

Basic Syntax
------------
```OTERCAL
<VERB> [<key> [AS <value>]] :D
```

All statements must be terminated with the "endless happiness" operator `:D`.

Verbs
-----

| Verb | Meaning | Needs Arguments? | == C API call |
| :--- | :--- | :--- | :--- |
| `JAR` | Sets `<key>` to the active table with the contents of `<value>` | Yes and Value | `ol_jar` |
| `UNJAR` | Gets the contents of `<key>` from the active table | Yes | `ol_unjar` |
| `SNIFF` | Gets the expiration of `<key>` from the active table | Yes | `ol_expiration_time` |
| `SCOOP` | Removes `<key>` from the active table | Yes | `ol_scoop` |
| `SPOIL` | Sets `<key>` in the active table to have an expiry date of `<value>` | Yes and Value | `ol_spoil` |
| `SELECT <prefix>` | Matches by prefix in the active table with `<key>` as the prefix to match against | Yes | `ol_prefix_match` |
| `SELECT *` | Gets all keys in the active table. | Yes | `ol_key_dump` |
| `INVENTORY` | Sees if `<key>` exists in the active table. | Yes | `ol_exists` |
| `CRAZY` | Performs the [Malbolge Crazy operation](https://en.wikipedia.org/wiki/Malbolge#Crazy_operation) on `<key>` in `<table>` against `<value>` | Yes and Value | |
| `PLS <attribute>` | Get an attribute from the current database | Yes | |

Attributes
----------

| Verb | Meaning | Needs Arguments? | == C API call |
| :--- | :--- | :--- | :--- |
| `UPTIME` | Returns the uptime of the database | No | `ol_uptime` |
| `SQUISH` | Squishes the AOL file | No | `ol_squish` |
| `PONDER` | Start a transaction | No | |
| `GO AWAY` | Abort a transaction | No | |
| `SHIP IT` | Ends a transaction | No | |
| `PURCHASE <table>` | Switches active database table | Yes | |
| `GTFO` | Closes active database table | Yes | |

Example Usage
-------------

```OTERCAL
PLS PURCHASE Users :D
PLS PONDER :D
JAR 'Xena' AS [[{"password": "foobang"}]] :D
PLS SHIP IT :D
```
