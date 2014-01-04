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
#include <time.h>
#include "oleg.h"


ol_database_obj ol_open(char *path, ol_filemode filemode){
    ol_database_obj new_db = malloc(sizeof(struct ol_database));

    size_t to_alloc = HASH_MALLOC;
    new_db->hashes = malloc(to_alloc);

    time_t created;
    time(&created);

    new_db->created = created;
    new_db->rcrd_cnt = 0;
    strncpy(new_db->name, "OLEG", sizeof("OLEG"));
    strncpy(new_db->path, path, PATH_LENGTH);
    return new_db;
}

int ol_close(ol_database_obj database){
    int i;
    for (i = 0; i < database->rcrd_cnt; i++) {
       ol_val free_me = database->hashes[i]->data_ptr;
       free(free_me);
       free(database->hashes[i]);
    }

    free(database);
    return 0;
}

ol_val ol_unjar(ol_database_obj db, char *key){
    int i;
    for(i = 0; i < db->rcrd_cnt; i++) {
        if(strncmp(db->hashes[i]->key, key, KEY_SIZE) == 0) {
            return db->hashes[i]->data_ptr;
        }
    }
    return NULL;
}

int ol_jar(ol_database_obj db, char *key, unsigned char *value, size_t vsize){
    /* Takes a key and a value and inserts it into the db */
    ol_hash *new_hash = malloc(sizeof(ol_hash));
    if (new_hash == NULL) {
        return 1;
    }

    //Silently truncate because #yolo
    if (strncpy(new_hash->key, key, KEY_SIZE) != new_hash->key) {
        return 2;
    }

    new_hash->data_size = vsize;

    unsigned char *data = malloc(vsize);
    if (memcpy(data, value, vsize) != data) {
        return 4;
    }
    new_hash->data_ptr = data;

    // Insert it into our db struct
    int cnt = db->rcrd_cnt;
    db->hashes[cnt] = new_hash;
    db->rcrd_cnt += 1;
    return 0;
}

int ol_scoop(ol_database_obj db, char *key) {
    // you know... like scoop some data from the jar and eat it? All gone.
    int i;
    for(i = 0; i < db->rcrd_cnt; i++) {
        if(strncmp(db->hashes[i]->key, key, KEY_SIZE) == 0) {
            ol_val free_me = db->hashes[i]->data_ptr;

            free(free_me);
            free(db->hashes[i]);

            db->hashes[i] = NULL;
            db->rcrd_cnt -= 1;

            return 0;
        }
    }
    return 1;
}

void ol_info(ol_database_obj db) {
    // Make uptime
    time_t now;
    double diff;
    time(&now);
    diff = difftime(now, db->created);
    printf("Uptime: %.f seconds\n", diff);
}
