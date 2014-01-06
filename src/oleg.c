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

void usage(void) {
    fprintf(stderr, "Usage: ./olegdb test\n");
    fprintf(stderr, "       ./olegdb\n");
    exit(1);
}

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

    free(database->hashes);
    free(database);
    return 0;
}

ol_hash *ol_get_hash(ol_database_obj db, char *key) {
    int i;
    for(i = 0; i < db->rcrd_cnt; i++) {
        if(strncmp(db->hashes[i]->key, key, KEY_SIZE) == 0) {
            return db->hashes[i];
        }
    }
    return NULL;
}

ol_val ol_unjar(ol_database_obj db, char *key){
    ol_hash *hash = ol_get_hash(db, key);

    if (hash != NULL) {
        return hash->data_ptr;
    }

    return NULL;
}

int ol_jar(ol_database_obj db, char *key, unsigned char *value, size_t vsize){
    // Check to see if we have an existing entry with that key
    ol_hash *old_hash = ol_get_hash(db, key);
    if (old_hash != NULL) {
        unsigned char *data = realloc(old_hash->data_ptr, vsize);
        if (memcpy(data, value, vsize) != data) {
            return 4;
        }

        old_hash->data_size = vsize;
        old_hash->data_ptr = data;
        printf("[-] Found old hash key.\n");
        return 0;
    }

    // Looks like we don't have an old hash
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
    ol_hash *old_hash = ol_get_hash(db, key);
    if (old_hash != NULL) {
        ol_val free_me = old_hash->data_ptr;

        free(free_me);
        free(old_hash);

        old_hash = NULL;
        db->rcrd_cnt -= 1;

        return 0;
    }
    return 1;
}

int ol_uptime(ol_database_obj db) {
    // Make uptime
    time_t now;
    double diff;
    time(&now);
    diff = difftime(now, db->created);
    return diff;
}
