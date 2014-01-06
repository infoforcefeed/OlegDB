#pragma once
//        DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//                    Version 2, December 2004
//
// Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>
//
// Everyone is permitted to copy and distribute verbatim or modified
// copies of this license document, and changing it is allowed as long
// as the name is changed.
//
//            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
//
//  0. You just DO WHAT THE FUCK YOU WANT TO.

#include <stdio.h>
#include <time.h>


/* Hardcoded key size */
#define KEY_SIZE 16
/* The size (in bytes) of a hash block */
#define HASH_MALLOC 8192
#define PATH_LENGTH 256

/* Modes of opening and operating on a DB */
typedef enum {
    OL_CONSUME_DIR      = 1 << 0, // Read
    OL_SLAUGHTER_DIR    = 1 << 1, // Write
    OL_CARESS_DIR       = 1 << 2, // Append
    OL_MANUFACTURE_DIR  = 1 << 3  // Create
} ol_filemode;

/* Data that the DB stores */
typedef unsigned char *ol_val;
typedef struct hash {
    char key[KEY_SIZE]; // The key used to reference the data
    ol_val data_ptr;
    size_t data_size;
} ol_hash;

typedef struct ol_database {
    char name[8];       // Name of the database
    char path[PATH_LENGTH];     // Path to the database directory
    int  rcrd_cnt;      // Number of records in the database. Eventually consistent.
    time_t created;     // For uptime.
    // huh...
    ol_hash **hashes;   // All hashes in the DB
    ol_val **values;    // All values in the DB
} *ol_database_obj;

typedef struct ol_meta {
    time_t uptime;
} *ol_meta_obj;

/* Opens a database using the filemode(s) specified */
ol_database_obj ol_open(char *path, ol_filemode filemode);
/* Closes a database, makes sure everything is written and frees memory */
int ol_close(ol_database_obj database);
/* Unjar a value from the mayo */
ol_val ol_unjar(ol_database_obj db, char *key);
/* it's easy to piss in a big bucket; it's NOT easy to piss in 19 jars */
int ol_jar(ol_database_obj db, char *key, unsigned char *value, size_t vsize);
/* Get that crap out of my mayo jar */
int ol_scoop(ol_database_obj db, char *key);
/* Helper for meta info */
int ol_uptime(ol_database_obj db);
