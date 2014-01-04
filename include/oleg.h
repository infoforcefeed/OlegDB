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


#define KEY_SIZE 16
#define HASH_MALLOC 8192

typedef enum {
    OL_CONSUME_DIR      = 1 << 0,
    OL_SLAUGHTER_DIR    = 1 << 1,
    OL_CARESS_DIR      = 1 << 2
} ol_filemode;

typedef unsigned char *ol_val;
typedef struct hash {
    char key[KEY_SIZE];
    ol_val data_ptr;
    size_t data_size;
} ol_hash;

typedef struct ol_database {
    char name[8];       // Name of the database
    char path[256];     // Path to the database directory
    int  rcrd_cnt;      // Number of records in the database. Eventually consistent.
    // huh...
    ol_hash **hashes;    // All hashes in the DB
    ol_val *values;     // All values in the DB
} *ol_database_obj;

ol_database_obj ol_open(char *path, ol_filemode filemode);
int ol_close(ol_database_obj database);
ol_val ol_unjar(ol_database_obj db, char *key);
// it's easy to piss in a big bucket; it's NOT easy to piss in 19 jars
int ol_jar(ol_database_obj db, char *key, unsigned char *value);
