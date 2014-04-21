#pragma once
/* The MIT License (MIT)
*
* Copyright (c) 2014 Quinlan Pfiffer, Kyle Terry
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
* THE SOFTWARE.
*/

#include <inttypes.h>
#include <stdio.h>
#include <stdbool.h>
#include <time.h>


/* xXx DEFINE=VERSION xXx
* xXx DESCRIPTION=The current version of the OlegDB. xXx
*/
#define VERSION "0.1.0"

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

/* xXx ENUM=ol_feature_flags xXx
* xXx DESCRIPTION=Feature flags tell the database what it should be doing. xXx
* xXx OL_F_APPENDONLY=Enable the append only log. This is a write-only logfile for simple persistence. xXx
* xXx OL_F_SEMIVOL=<strong>Not Implemented</strong> Tell servers that it's okay to fsync every once in a while xXx
* xXx OL_F_REGDUMPS=<strong>Not Implemented</strong> Tell servers to snapshot the data using ol_save() regularly xXx
* xXx OL_F_LZ4=Enable LZ4 compression. xXx
*/
typedef enum {
    OL_F_APPENDONLY     = 1 << 0,
    OL_F_SEMIVOL        = 1 << 1,
    OL_F_REGDUMPS       = 1 << 2,
    OL_F_LZ4            = 1 << 3
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

/* xXx TYPEDEF=ol_val xXx
 * xXx DESCRIPTION=Typedef for the values that can be stored inside the database. xXx
 */
typedef unsigned char *ol_val;

/* xXx STRUCT=ol_bucket xXx
* xXx DESCRIPTION=This is the object stored in the database's hashtable. Contains references to value, key, etc. xXx
* xXx key[KEY_SIZE]=The key used for this bucket. xXx
* xXx klen=Length of the key. xXx
* xXx *content_type=The content-type of this object. If using the server, this defaults to "application/octet-stream". xXx
* xXx ctype_size=Length of the string representing content-type. xXx
* xXx data_ptr=Location of this key's value (data). xXx
* xXx data_size=Length of the value (data) in bytes. xXx
* xXx hash=Hashed value of this key. xXx
* xXx next=Collisions are resolved via linked list. This contains the pointer to the next object in the chain, or NULL. xXx
* xXx expiration=The POSIX timestamp when this key will expire. xXx
*/
typedef struct ol_bucket {
    char              key[KEY_SIZE]; /* The key used to reference the data */
    size_t            klen;
    char              *content_type;
    size_t            ctype_size;
    ol_val            data_ptr;
    size_t            data_size;
    size_t            original_size; /* Only needed for LZ4 */
    uint32_t          hash;
    struct ol_bucket  *next; /* The next ol_bucket in this chain, if any */
    struct tm         *expiration;
} ol_bucket;

/* xXx STRUCT=ol_database xXx
* xXx DESCRIPTION=The object representing a database. This is used in almost every ol_* function to store state and your data. xXx
* xXx get_db_file_name=A function pointer that returns the path to the location of the db file to reduce code duplication. Used for writing and reading of dump files. xXx
* xXx enable=Helper function to enable a feature for the database instance passed in. See <a href="#ol_feature_flags">ol_feature_flags</a> xXx
* xXx disable=Helper function to disable a database feature. See <a href="#ol_feature_flags">ol_feature_flags</a> xXx
* xXx is_enabled=Helper function that checks weather or not a feature flag is enabled. xXx
* xXx name=The name of the database. xXx
* xXx path[PATH_LENGTH]=Path to the database's working directory. xXx
* xXx dump_file=Path and filename of db dump. xXx
* xXx aol_file=Path and filename of the append only log. xXx
* xXx aolfd=Pointer of FILE type to append only log. xXx
* xXx feature_set=Bitmask holding enabled/disabled status of various features. See ol_feature_flags. xXx
* xXx state=Current state of the database. See ol_state_flags. xXx
* xXx rcrd_cnt=Number of records in the database. xXx
* xXx key_collisions=Number of key collisions this database has had since initialization. xXx
* xXx created=Timestamp of when the database was initialized. xXx
* xXx cur_ht_size=The current amount, in bytes, of space allocated for storing <a href="#ol_bucket">ol_bucket</a> objects. xXx
* xXx **hashes=The actual hashtable. Stores <a href="#ol_bucket">ol_bucket</a> instances. xXx
*/
typedef struct ol_database {
    void      (*get_db_file_name)(struct ol_database *db,const char *p,char*);
    void      (*enable)(int, int*);
    void      (*disable)(int, int*);
    bool      (*is_enabled)(int, int*);
    char      name[DB_NAME_SIZE];
    char      path[PATH_LENGTH];
    char      *dump_file;
    char      *aol_file;
    FILE      *aolfd;
    int       feature_set;
    short int state;
    int       rcrd_cnt;
    int       key_collisions;
    time_t    created;
    size_t    cur_ht_size;
    ol_bucket **hashes;
} ol_database;

/* xXx STRUCT=ol_meta xXx
* xXx DESCRIPTION=Structure used to record meta-information about the database. xXx
*/
typedef struct ol_meta {
    time_t uptime;
} ol_meta;

/* xXx FUNCTION=ol_open xXx
 * xXx DESCRIPTION=Opens a database for use. xXx
 * xXx RETURNS=A new database object. xXx
 * xXx *path=The directory where the database will be stored. xXx
 * xXx *name=The name of the database. This is used to create the dumpfile, and keep track of the database. xXx
 * xXx features=Features to enable when the database is initialized. You can logically OR multiple features together. xXx
 */
ol_database *ol_open(char *path, char *name, int features);

/* xXx FUNCTION=ol_close xXx
 * xXx DESCRIPTION=Closes a database cleanly and frees memory. xXx
 * xXx RETURNS=0 on success, 1 if not everything could be freed. xXx
 * xXx *database=The database to close. xXx
 */
int ol_close(ol_database *database);

/* xXx FUNCTION=ol_close_save xXx
 * xXx DESCRIPTION=Dumps and closes a database cleanly, frees memory and makes sure everything is written. xXx
 * xXx RETURNS=0 on success, 1 if not everything could be freed. xXx
 * xXx *database=The database to close. xXx
 */
int ol_close_save(ol_database *database);

/* xXx FUNCTION=ol_unjar xXx
 * xXx DESCRIPTION=This is OlegDB's canonical 'get' function. Unjar a value from the mayo (database). Calls <a href="#ol_unjar_ds">ol_unjar_ds</a> with a dsize of NULL. xXx
 * xXx RETURNS=A pointer to an <a href="#ol_val">ol_val</a> object, or NULL if the object was not found. xXx
 * xXx *db=Database to retrieve value from. xXx
 * xXx *key=The key to use. xXx
 * xXx klen=The length of the key. xXx
 */
ol_val ol_unjar(ol_database *db, const char *key, size_t klen);

/* xXx FUNCTION=ol_unjar_ds xXx
 * xXx DESCRIPTION=This function retrieves a value from the database. It also writes the size of the data to <code>dsize</code>. xXx
 * xXx RETURNS=A pointer to an <a href="#ol_val">ol_val</a> object, or NULL if the object was not found. xXx
 * xXx *db=Database to retrieve value from. xXx
 * xXx *key=The key to use. xXx
 * xXx klen=The length of the key to use. xXx
 * xXx *dsize=Optional parameter that will be filled out with the size of the data, if NULL is not passed in. xXx
 */
ol_val ol_unjar_ds(ol_database *db, const char *key, size_t klen, size_t *dsize);

/* xXx FUNCTION=ol_jar xXx
 * xXx DESCRIPTION=This is OlegDB's canonical 'set' function. Put a value into the mayo (the database). It's easy to piss in a bucket, it's not easy to piss in 19 jars. Uses default content type. xXx
 * xXx RETURNS=0 on sucess. xXx
 * xXx *db=Database to retrieve value from. xXx
 * xXx *key=The key to use. xXx
 * xXx klen=The length of the key. xXx
 * xXx *value=The value to insert. xXx
 * xXx vsize=The size of the value in bytes. xXx
 */
int ol_jar(ol_database *db, const char *key, size_t klen, unsigned char *value, size_t vsize);

/* xXx FUNCTION=ol_jar_ct xXx
 * xXx DESCRIPTION=Wrapped by <a href="#ol_jar">ol_jar</a>, this function will set a value in the database. It differs only in that it allows you to specify a content type to store in addition to the value. xXx
 * xXx RETURNS=0 on sucess. xXx
 * xXx *db=Database to retrieve value from. xXx
 * xXx *key=The key to use. xXx
 * xXx klen=The length of the key. xXx
 * xXx *value=The value to insert. xXx
 * xXx vsize=The size of the value in bytes. xXx
 * xXx *content_type=The content type to store, or really anything. Store your middle name if you want to. xXx
 * xXx content_type_size=The length of the content_type string. xXx
 */
int ol_jar_ct(ol_database *db, const char *key, size_t klen, unsigned char *value, size_t vsize,
        const char *content_type, const size_t content_type_size);

/* xXx FUNCTION=ol_content_type xXx
 * xXx DESCRIPTION=Retrieves the content type for a given key from the database. xXx
 * xXx RETURNS=Stored content type, or NULL if it was not found. xXx
 * xXx *db=Database to retrieve value from. xXx
 * xXx *key=The key to use. xXx
 * xXx klen=The length of the key. xXx
 */
char *ol_content_type(ol_database *db, const char *key, size_t klen);

/* xXx FUNCTION=ol_expiration xXx
 * xXx DESCRIPTION=Retrieves the expiration time for a given key from the database. xXx
 * xXx RETURNS=Stored <code>struct tm *</code> representing the time that this key will expire, or NULL if not found. xXx
 * xXx *db=Database to retrieve value from. xXx
 * xXx *key=The key to use. xXx
 * xXx klen=The length of the key. xXx
 */
struct tm *ol_expiration_time(ol_database *db, const char *key, size_t klen);

/* xXx FUNCTION=ol_scoop xXx
 * xXx DESCRIPTION=Removes an object from the database. Get that crap out of the mayo jar. xXx
 * xXx RETURNS=0 on success, and 1 or 2 if the object could not be deleted. xXx
 * xXx *db=Database to retrieve value from. xXx
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
 * xXx *db=Database to retrieve value from. xXx
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

