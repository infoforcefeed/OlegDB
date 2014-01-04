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
#include <stdlib.h>
#include <string.h>
#include "oleg.h"


ol_database_obj ol_open(char *path, ol_filemode filemode){
    ol_database_obj new_db = malloc(sizeof(struct ol_database));
    size_t to_alloc = HASH_MALLOC;
    new_db->hashes = malloc(to_alloc);
    return new_db;
}

int ol_close(ol_database_obj database){
    int i;
    for (i = 0; i < database->rcrd_cnt; i++) {
       ol_val free_me = database->hashes[i]->data_ptr;
       free(free_me);
       free(&database->hashes[i]);
    }

    free(database);
    return 0;
}

ol_val ol_unjar(char *key){
    return NULL;
}

int ol_jar(ol_database_obj db, char *key, unsigned char *value){
    /* Takes a key and a value and inserts it into the db */
    ol_hash *new_hash = malloc(sizeof(ol_hash));
    if (new_hash == NULL) {
        return 1;
    }

    //Silently truncate because #yolo
    if (strncpy(new_hash->key, key, KEY_SIZE) != new_hash->key) {
        return 2;
    }

    size_t data_size = sizeof(value);
    if (data_size != 0) {
        return 3;
    }

    new_hash->data_size = data_size;
    printf("Size of: %zu", data_size);

    unsigned char *data = malloc(data_size);
    if (memcpy(data, value, data_size) != data) {
        return 4;
    }

    // Insert it into our db struct
    db->rcrd_cnt += 1;
    int cnt = db->rcrd_cnt;
    db->hashes[cnt] = new_hash;
    return 0;
}
