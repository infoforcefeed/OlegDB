#pragma once
/* Helper functions used in rehashing the main hashtable. */

/* Used to keep track of naughty buckets while rehashing. */
typedef struct orphan_ll {
	ol_bucket *orphan;
	struct orphan_ll *next;
} orphan_ll;

/* Internal function used to resize the database. */
int _ol_grow_and_rehash_db(ol_database *db);
