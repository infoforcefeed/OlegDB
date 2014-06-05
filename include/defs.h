#pragma once
/* Commonly used #defines. */

/* xXx DEFINE=VERSION xXx
* xXx DESCRIPTION=The current version of the OlegDB. xXx
*/
#define VERSION "0.1.1"

/* xXx DEFINE=KEY_SIZE xXx
* xXx DESCRIPTION=The hardcoded upperbound for key lengths. xXx
*/
#define KEY_SIZE 250

/* xXx DEFINE=HASH_MALLOC xXx
* xXx DESCRIPTION=The size, in bytes, to allocate when initially creating the database. ol_bucket pointers are stored here. xXx
*/
#define HASH_MALLOC 65536

/* xXx DEFINE=PATH_LENGTH xXx
* xXx DESCRIPTION=The maximum length of a database's path. xXx
*/
#define PATH_LENGTH 256

/* xXx DEFINE=DB_NAME_SIZE xXx
* xXx DESCRIPTION=Database maximum name length. xXx
*/
#define DB_NAME_SIZE 64

/* xXx DEFINE=DEVILS_SEED xXx
* xXx DESCRIPTION=The seed to feed into the murmur3 algorithm. xXx
*/
#define DEVILS_SEED 666

/* xXx DEFINE=VALUES_FILENAME xXx
* xXx DESCRIPTION=The file extension used for the values file on disk. xXx
*/
#define VALUES_FILENAME "val"

/* xXx DEFINE=VALUES_DEFAULT_SIZE xXx
* xXx DESCRIPTION=The default size of the values file on disk. 16 Megabytes by default. xXx
*/
#define VALUES_DEFAULT_SIZE 16777216
