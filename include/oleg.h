#pragma once
/* This is the main OlegDB file. In here you will find common data types,
 * functions, enums used everywhere. This is also the main source of the
 * liboleg API. Refer to the documentation on https://olegdb.org/ for more
 * information.
 */

/* CPP compatibility. This is put before the includes so they we can probably
 * avoid putting this ugly wrapper around those other files as well.
 */
#ifdef __cplusplus
extern "C" {
#endif

#include <inttypes.h>
#include <stdio.h>
#include <stdbool.h>
#include <time.h>
#include "defs.h"
#include "tree.h"

/* xXx ENUM=ol_feature_flags xXx
* xXx DESCRIPTION=Feature flags tell the database what it should be doing. xXx
* xXx OL_F_APPENDONLY=Enable the append only log. This is a write-only logfile for simple persistence. xXx
* xXx OL_F_SPLAYTREE=Whether or not to enable to splay tree in the server. This can have a performance impact. xXx
* xXx OL_F_LZ4=Enable LZ4 compression. xXx
* xXx OL_F_AOL_FFLUSH=Make sure AOL data is REAAAALLY written to disk. This will run fflush after every AOL write. Otherwise, fsync only. xXx
* xXx OL_F_DISABLE_TX=Disable transactions on this database. xXx
*/
typedef enum {
    OL_F_APPENDONLY                 = 1 << 0,
    OL_F_SPLAYTREE                  = 1 << 1,
    OL_F_LZ4                        = 1 << 2,
    OL_F_AOL_FFLUSH                 = 1 << 3,
    OL_F_DISABLE_TX                 = 1 << 4
} ol_feature_flags;

/* xXx ENUM=ol_state_flags xXx
* xXx DESCRIPTION=State flags tell the database what it should be doing. xXx
* xXx OL_S_STARTUP=Startup state. The DB is starting, duh. xXx
* xXx OL_S_AOKAY=The normal operating state, the database is a-okay xXx
*/
typedef enum {
    OL_S_STARTUP        = 0,
    OL_S_AOKAY          = 1
} ol_state_flags;

/* xXx TYPEDEF=ol_val_array xXx
* xXx DESCRIPTION=This is shorthand for a pointer to an array of values, the same kind of values stored in an <a href="#ol_bucket">ol_bucket</a>'s data_offset. xXx
*/
typedef char ** ol_val_array;

/* xXx STRUCT=ol_bucket xXx
* xXx DESCRIPTION=This is the object stored in the database's hashtable. Contains references to value, key, etc. xXx
* xXx key[KEY_SIZE]=The key used for this bucket. xXx
* xXx klen=Length of the key. xXx
* xXx data_offset=Location of this key's value (data) in the values file on disk. xXx
* xXx data_size=Length of the value (data) in bytes. This is the size of the data stored in memory. xXx
* xXx original_size=Length of the value (data) in bytes. This is the original length of the data we receieved, non-compressed. xXx
* xXx next=Collisions are resolved via linked list. This contains the pointer to the next object in the chain, or NULL. xXx
* xXx expiration=The POSIX timestamp when this key will expire. xXx
* xXx *node=A pointer to this objects node in the splay tree. xXx
* xXx tx_id=If this record is a part of a transaction, then this will be a non-negative integer.
*/
typedef struct ol_bucket {
    char                key[KEY_SIZE]; /* The key used to reference the data */
    size_t              klen;
    size_t              data_offset;
    size_t              data_size;
    size_t              original_size;
    struct ol_bucket    *next; /* The next ol_bucket in this chain, if any */
    struct tm           *expiration;
    ol_splay_tree_node  *node;
    int                 tx_id;
} ol_bucket;

/* xXx STRUCT=ol_meta xXx
* xXx DESCRIPTION=Structure used to record meta-information about the database. xXx
* xXx created=When the database was created. xXx
* xXx key_collisions=The number of keys that have collided over the lifetime of this database. xXx
*/
typedef struct ol_meta {
    time_t      created;
    int         key_collisions;
} ol_meta;

/* xXx STRUCT=ol_database xXx
* xXx DESCRIPTION=The object representing a database. This is used in almost every ol_* function to store state and your data. xXx
* xXx get_db_file_name=A function pointer that returns the path to the location of the db file to reduce code duplication. Used for writing and reading of dump files. xXx
* xXx enable=Helper function to enable a feature for the database instance passed in. See <a href="#ol_feature_flags">ol_feature_flags</a> xXx
* xXx disable=Helper function to disable a database feature. See <a href="#ol_feature_flags">ol_feature_flags</a> xXx
* xXx is_enabled=Helper function that checks weather or not a feature flag is enabled. xXx
* xXx name=The name of the database. xXx
* xXx path[PATH_LENGTH]=Path to the database's working directory. xXx
* xXx aol_file=Path and filename of the append only log. xXx
* xXx aolfd=Pointer of FILE type to append only log. xXx
* xXx feature_set=Bitmask holding enabled/disabled status of various features. See ol_feature_flags. xXx
* xXx state=Current state of the database. See ol_state_flags. xXx
* xXx rcrd_cnt=Number of records in the database. xXx
* xXx key_collisions=Number of key collisions this database has had since initialization. xXx
* xXx created=Timestamp of when the database was initialized. xXx
* xXx cur_ht_size=The current amount, in bytes, of space allocated for storing <a href="#ol_bucket">ol_bucket</a> objects. xXx
* xXx **hashes=The actual hashtable. Stores <a href="#ol_bucket">ol_bucket</a> instances. xXx
* xXx *values=This is where values for hashes are stored. This is a pointer to an mmap()'d region of memory. xXx
* xXx valuesfd=The file descriptor of the values file. xXx
* xXx val_size=The size of the sum total of records in the db, in bytes. It is <strong>not</strong> the size of the file on disk. xXx
* xXx *tree=A pointer to the splay tree holding the ordered list of keys. xXx
* xXx *cur_transactions=The current open/uncommitted transactions. Represented with a splay tree. xXx
* xXx *meta=A pointer to a struct holding extra meta information. See <a href="#ol_meta">oleg_meta</a> for more information. xXx
*/
typedef struct ol_database {
    void            (*get_db_file_name)(const struct ol_database *db,const char *p,char*);
    void            (*enable)(int, int*);
    void            (*disable)(int, int*);
    bool            (*is_enabled)(const int, const int*);
    char            name[DB_NAME_SIZE];
    char            path[PATH_LENGTH];
    char            aol_file[AOL_FILENAME_ALLOC];
    FILE            *aolfd;
    int             feature_set;
    short int       state;
    int             rcrd_cnt;
    size_t          cur_ht_size;
    ol_bucket       **hashes;
    unsigned char   *values;
    int             valuesfd;
    size_t          val_size;
    ol_splay_tree   *tree;
    ol_splay_tree   *cur_transactions;
    ol_meta         *meta;
} ol_database;

/* xXx FUNCTION=ol_open xXx
 * xXx DESCRIPTION=Opens a database for use. xXx
 * xXx RETURNS=A new database object. NULL on failure. xXx
 * xXx *path=The directory where the database will be stored. xXx
 * xXx *name=The name of the database. This is used to create the dumpfile, and keep track of the database. xXx
 * xXx features=Features to enable when the database is initialized. You can logically OR multiple features together. xXx
 */
ol_database *ol_open(const char *path, const char *name, int features);

/* xXx FUNCTION=ol_close xXx
 * xXx DESCRIPTION=Closes a database cleanly and frees memory. xXx
 * xXx RETURNS=0 on success, 1 if not everything could be freed. xXx
 * xXx *database=The database to close. xXx
 */
int ol_close(ol_database *database);

/* xXx FUNCTION=ol_unjar xXx
 * xXx DESCRIPTION=This is OlegDB's canonical 'get' function. Unjar a value from the mayo (database). <strong>data must be freed after calling this function!</strong> Calls <a href="#ol_unjar_ds">ol_unjar_ds</a> with a dsize of NULL. xXx
 * xXx RETURNS=0 on success, 1 on failure or if the key was not found.
 * xXx *db=Database to retrieve value from. xXx
 * xXx *key=The key to use. xXx
 * xXx klen=The length of the key. xXx
 * xXx data=This parameter will be filled out with the data found in the DB. Passing NULL will check if a key exists. xXx
 */
int ol_unjar(ol_database *db, const char *key, size_t klen, unsigned char **data);

/* xXx FUNCTION=ol_unjar_ds xXx
 * xXx DESCRIPTION=This function retrieves a value from the database. <strong>data must be freed after calling this function!</strong> It also writes the size of the data to <code>dsize</code>. xXx
 * xXx RETURNS=0 on success, 1 on failure or if the key was not found.
 * xXx *db=Database to retrieve value from. xXx
 * xXx *key=The key to use. xXx
 * xXx klen=The length of the key to use. xXx
 * xXx data=This parameter will be filled out with the data found in the DB. Passing NULL will check if a key exists. xXx
 * xXx *dsize=Optional parameter that will be filled out with the size of the data, if NULL is not passed in. xXx
 */
int ol_unjar_ds(ol_database *db, const char *key, size_t klen, unsigned char **data, size_t *dsize);

/* xXx FUNCTION=ol_jar xXx
 * xXx DESCRIPTION=This is OlegDB's canonical 'set' function. Put a value into the mayo (the database). It's easy to piss in a bucket, it's not easy to piss in 19 jars. xXx
 * xXx RETURNS=0 on success. xXx
 * xXx *db=Database to set the value to. xXx
 * xXx *key=The key to use. xXx
 * xXx klen=The length of the key. xXx
 * xXx *value=The value to insert. xXx
 * xXx vsize=The size of the value in bytes. xXx
 */
int ol_jar(ol_database *db, const char *key, size_t klen, unsigned char *value, size_t vsize);

/* xXx FUNCTION=ol_expiration xXx
 * xXx DESCRIPTION=Retrieves the expiration time for a given key from the database. xXx
 * xXx RETURNS=Stored <code>struct tm *</code> representing the time that this key will expire, or NULL if not found. xXx
 * xXx *db=Database to set the value to. xXx
 * xXx *key=The key to use. xXx
 * xXx klen=The length of the key. xXx
 */
struct tm *ol_expiration_time(ol_database *db, const char *key, size_t klen);

/* xXx FUNCTION=ol_scoop xXx
 * xXx DESCRIPTION=Removes an object from the database. Get that crap out of the mayo jar. xXx
 * xXx RETURNS=0 on success, and 1 or 2 if the object could not be deleted. xXx
 * xXx *db=Database to remove the value from. xXx
 * xXx *key=The key to use. xXx
 * xXx klen=The length of the key. xXx
 */
int ol_scoop(ol_database *db, const char *key, size_t klen);

/* xXx FUNCTION=ol_uptime xXx
 * xXx DESCRIPTION=Gets the time, in seconds, that a database has been up. xXx
 * xXx RETURNS=Uptime in seconds since database initialization. xXx
 * xXx *db=Database to retrieve value from. xXx
 */
int ol_uptime(ol_database *db);

/* xXx FUNCTION=ol_spoil xXx
 * xXx DESCRIPTION=Sets the expiration value of a key. Will fail if no <a href="#ol_bucket">ol_bucket</a> under the chosen key exists. xXx
 * xXx RETURNS=0 upon success, 1 if otherwise. xXx
 * xXx *db=Database to set the value to. xXx
 * xXx *key=The key to use. xXx
 * xXx klen=The length of the key. xXx
 * xXx expiration_date=The <b>UTC</b> time to set the expiration to. xXx
 */
int ol_spoil(ol_database *db, const char *key, size_t klen, struct tm *expiration_date);

/* xXx FUNCTION=ol_ht_bucket_max xXx
 * xXx DESCRIPTION=Does some <code>sizeof</code> witchery to return the maximum current size of the database. This is mostly an internal function, exposed to reduce code duplication. xXx
 * xXx RETURNS=The maximum possible bucket slots for db. xXx
 * xXx *ht_size=The size you want to divide by <code>sizeof(ol_bucket)</code>. xXx
 */
int ol_ht_bucket_max(size_t ht_size);

/* xXx FUNCTION=ol_prefix_match xXx
 * xXx DESCRIPTION=Returns values of keys that match a given prefix. xXx
 * xXx RETURNS=-1 on failure and a positive integer representing the number of matched prefices in the database. xXx
 * xXx *db=Database to retrieve values from. xXx
 * xXx *prefix=The prefix to attempt matches on. xXx
 * xXx plen=The length of the prefix. xXx
 * xXx *data=A pointer to an <code>ol_val_array</code> object where the list of values will be stored. <strong>Both the list and it's items must be freed after use.</strong> xXx
 */
int ol_prefix_match(ol_database *db, const char *prefix, size_t plen, ol_val_array *data);

/* xXx FUNCTION=ol_exists xXx
 * xXx DESCRIPTION=Returns whether the given key exists on the database xXx
 * xXx RETURNS=0 if the key exists, 1 otherwise. xXx
 * xXx *db=Database the key should be in. xXx
 * xXx *key=The key to check. xXx
 * xXx klen=The length of the key. xXx
 */
int ol_exists(ol_database *db, const char *key, size_t klen);

/* xXx FUNCTION=ol_get_bucket xXx
 * xXx DESCRIPTION=Utility function to retrieve an <a href="#ol_bucket">ol_bucket</a> object from the database for a given key. xXx
 * xXx RETURNS=The bucket if it exists, otherwise NULL. xXx
 * xXx *db=Database the bucket should be in. xXx
 * xXx *key=The key to check. xXx
 * xXx klen=The length of the key. xXx
 * xXx **_key=The truncated key will be filled in at this address. xXx
 * xXx *_klen=The length of the truncated key. xXx
 */
ol_bucket *ol_get_bucket(const ol_database *db, const char *key, const size_t klen,
                         char (*_key)[KEY_SIZE], size_t *_klen);

/* xXx FUNCTION=ol_squish xXx
 * xXx DESCRIPTION=Compacts both the aol file (if enabled) and the values file. This is a blocking operation. xXx
 * xXx RETURNS=1 if successful, 0 if otherwise. xXx
 * xXx *db=The database to compact. xXx
 */
int ol_squish(ol_database *db);

/* xXx FUNCTION=ol_cas xXx
 * xXx DESCRIPTION=ol_jar operation that atomically compares-and-swaps old data for new data. xXx
 * xXx RETURNS=0 on success. xXx
 * xXx *db=The database to operate on. xXx
 * xXx *key=The key to check. xXx
 * xXx klen=The length of the key. xXx
 * xXx *value=The value to insert. xXx
 * xXx vsize=The size of the value in bytes. xXx
 * xXx *ovalue=The old value to compare against. xXx
 * xXx ovsize=The size of the old value in bytes. xXx
 */
int ol_cas(ol_database *db, const char *key, const size_t klen,
                            unsigned char *value, size_t vsize,
                            const unsigned char *ovalue, const size_t ovsize);

#ifdef __cplusplus
}
#endif
