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
#include <time.h>


/* xXx DEFINE=VERSION xXx
* xXx DESCRIPTION=The current version of the OlegDB. xXx
*/
#define VERSION "0.1.0"

/* xXx DEFINE=KEY_SIZE xXx
* xXx DESCRIPTION=The hardcoded upperbound for key lengths. xXx
*/
#define KEY_SIZE 32

/* xXx DEFINE=HASH_MALLOC xXx
* xXx DESCRIPTION=The size, in bytes, to allocate when initially creating the database. ol_bucket pointers are stored here. xXx
*/
#define HASH_MALLOC 65536

/* xXx DEFINE=PATH_LENGTH xXx
* xXx DESCRIPTION=The maximum length of a database's path. xXx
*/
#define PATH_LENGTH 256

/* xXx DEFINE=DEVILS_SEED xXx
* xXx DESCRIPTION=The seed to feed into the murmur3 algorithm. xXx
*/
#define DEVILS_SEED 666

/* xXx ENUM=ol_filemode xXx
* xXx DESCRIPTION=The filemodes to open and manipulate a database with. OR them together to use multiple. xXx
* xXx OL_CONSUME_DIR=Open database for reading. xXx
* xXx OL_SLAUGHTER_DIR=Open database in for writing. xXx
* xXx OL_CARESS_DIR=Open database in appendonly mode. xXx
* xXx OL_SLAUGHTER_DIR=Open database and create directories and dumpfiles if they don't exist. xXx
*/
typedef enum {
    OL_CONSUME_DIR      = 1 << 0, /* Read */
    OL_SLAUGHTER_DIR    = 1 << 1, /* Write */
    OL_CARESS_DIR       = 1 << 2, /* Append */
    OL_MANUFACTURE_DIR  = 1 << 3  /* Create */
} ol_filemode;

/* xXx TYPEDEF=ol_val xXx
 * xXx DESCRIPTION=Typedef for the values that can be stored inside the database. xXx
 */
typedef unsigned char *ol_val;

/* xXx STRUCT=ol_bucket xXx
* xXx DESCRIPTION=This is the object stored in the database's hashtable. Contains references to value, key, etc. xXx
* xXx key[KEY_SIZE]=The key used for this bucket. xXx
* xXx *content_type=The content-type of this object. Defaults to "application/octet-stream". xXx
* xXx ctype_size=Length of the string representing content-type. xXx
* xXx data_ptr=Location of this key's value. xXx
* xXx data_size=Length of the value in bytes. xXx
* xXx hash=Hashed value of this key. xXx
* xXx next=Collisions are resolved via linked list. This contains the pointer to the next object in the chain, or NULL. xXx
*/
typedef struct ol_bucket {
    char              key[KEY_SIZE]; /* The key used to reference the data */
    char              *content_type;
    size_t            ctype_size;
    ol_val            data_ptr;
    size_t            data_size;
    uint32_t          hash;
    struct ol_bucket  *next; /* The next ol_bucket in this chain, if any */
} ol_bucket;


typedef struct ol_database {
    void      (*get_db_name)(struct ol_database *db,char*);       /* Function to grab db name */
    char      name[64];           /* Name of the database */
    char      path[PATH_LENGTH]; /* Path to the database directory */
    char      *dump_file;        /* Path and filename of db dump */
    int       rcrd_cnt;          /* Number of records in the database. Eventually consistent. */
    int       key_collisions;    /* How many times have our keys collided. */
    time_t    created;           /* For uptime. */
    size_t    cur_ht_size;       /* Gotta keep track of that table size */
    ol_bucket **hashes;          /* All hashes in the DB */
} ol_database;

typedef struct ol_meta {
    time_t uptime;
} ol_meta;

/* xXx FUNCTION=ol_open xXx
 * xXx path=The directory where the database will be stored. xXx
 * xXx name=The name of the database. This is used to create the dumpfile, and keep track of the database. xXx
 * xXx filemode=The filemode used to interact with the database. See xXx REF=ol_filemode xXx xXx
 */
ol_database *ol_open(char *path, char *name, ol_filemode filemode);
/* Closes a database, makes sure everything is written and frees memory */
int ol_close(ol_database *database);
/* Unjar a value from the mayo */
ol_val ol_unjar(ol_database *db, const char *key);
/* it's easy to piss in a big bucket; it's NOT easy to piss in 19 jars */
int ol_jar(ol_database *db, const char *key, unsigned char *value, size_t vsize);
/* ol_jar with specified content type */
int ol_jar_ct(ol_database *db, const char *key,unsigned char *value, size_t vsize,
        const char *content_type, const size_t content_type_size);
/* Get the stored content type of a jarred value */
char *ol_content_type(ol_database *db, const char *key);
/* Get that crap out of my mayo jar */
int ol_scoop(ol_database *db, const char *key);
/* Helper for meta info */
int ol_uptime(ol_database *db);
int _ol_ht_bucket_max(size_t);
