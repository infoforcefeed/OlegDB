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


typedef enum {
    OL_CONSUME_DIR      = 1 << 0,
    OL_SLAUGHTER_DIR    = 1 << 1,
    OL_CARRESS_DIR      = 1 << 2
} ol_filemode;

typedef unsigned char *ol_val;
typedef struct hash {
    char key[16];
    ol_val data_ptr;
    size_t data_size;
} ol_hash;

typedef struct ol_database {
    char name[8];                   // Name of the database
    char path[256];                 // Path to the database directory
    int  rcrd_cnt;                  // Number of records in the database. Eventually consistent.
    // huh...
    ol_hash *hashes;
    ol_val *values;
} *ol_database_obj;

ol_database_obj ol_open(char *path, ol_filemode filemode);
int ol_close(ol_database_obj database);
ol_val ol_unjar(char *key);
// it's easy to piss in a big bucket; it's NOT easy to piss in 19 jars
int ol_jar(char *key, unsigned char *value);
