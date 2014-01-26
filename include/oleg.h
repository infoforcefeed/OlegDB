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


/* ye ol version */
#define VERSION "0.1.0"

/* Hardcoded key size */
#define KEY_SIZE 32
/* The size (in bytes) of a hash block */
#define HASH_MALLOC 65536
#define PATH_LENGTH 256
#define DEVILS_SEED 666

/* Modes of opening and operating on a DB */
typedef enum {
    OL_CONSUME_DIR      = 1 << 0, /* Read */
    OL_SLAUGHTER_DIR    = 1 << 1, /* Write */
    OL_CARESS_DIR       = 1 << 2, /* Append */
    OL_MANUFACTURE_DIR  = 1 << 3  /* Create */
} ol_filemode;

/* Data that the DB stores */
typedef unsigned char *ol_val;
struct ol_bucket {
    char              key[KEY_SIZE]; /* The key used to reference the data */
    char              *content_type;
    size_t            ctype_size;
    ol_val            data_ptr;
    size_t            data_size;
    uint32_t          hash;
    struct ol_bucket  *next; /* The next ol_bucket in this chain, if any */
};
typedef struct ol_bucket ol_bucket; /* To enable self-referential struct */

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

/* Opens a database using the filemode(s) specified */
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
