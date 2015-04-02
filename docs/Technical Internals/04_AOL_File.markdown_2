The **A**ppend **O**nly **L**og file is how Oleg keeps track of state outside of
it's values files. Everytime a change occurs to OlegDB, that command is written
to the AOL file. This is what allows OlegDB to be persistent.

Every now and then the AOL file needs to be [squished (compacted)](#ol_squish)
to remove old and expired data. The AOL file is designed to be human readable
(mostly) so you can tell at a glance whats going on with your database.
