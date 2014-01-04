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

#include <msgpack.h>

typedef enum {
    OL_CONSUME_DIR      = 1 << 0,
    OL_SLAUGHTER_DIR    = 1 << 1,
    OL_CARRESS_DIR      = 1 << 2
} ol_filemode;

typedef msgpack_sbuffer *ol_obj;

struct ol_database {
    char name[8];                   // Name of the database
    char path[256];                 // Path to the database directory
    int  rcrd_cnt;                  // Number of records in the database. Eventually consistent.
    ol_obj records[];     // All of the records in the database
};

struct ol_database *ol_open(char *path, ol_filemode filemode);
ol_obj ol_get(char *path, ol_filemode filemode);
