At it's core, OlegDB is just a hashtable. On a good day, this means operations
are `O(1)`. Since we use linked lists to handle collisions [see here](http://en.wikipedia.org/wiki/Hash_table#Separate_chaining_with_linked_lists) 
the worst-case scenario for operations is `O(n)`. This usually doesn't happen.

Rehashing happens when we run out of space. To handle this, we currently
allocate a new block of memory, rehash all keys and move everything over. This
is a blocking operation. If you know you're going to have a lot of keys and want
to avoid this, you can tweak the [HASH_MALLOC](#HASH_MALLOC) parameter before compilation.
This controls the default amount of space that OlegDB will allocate.
