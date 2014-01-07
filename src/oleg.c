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
#include <inttypes.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "oleg.h"

ol_database_obj ol_open(char *path, ol_filemode filemode){
    ol_database_obj new_db = malloc(sizeof(struct ol_database));

    size_t to_alloc = HASH_MALLOC;
    new_db->hashes = calloc(1, to_alloc);

    time_t created;
    time(&created);

    new_db->created = created;
    new_db->rcrd_cnt = 0;
    strncpy(new_db->name, "OLEG", sizeof("OLEG"));
    strncpy(new_db->path, path, PATH_LENGTH);
    return new_db;
}

int ol_close(ol_database_obj database){
    int iterations = HASH_MALLOC/sizeof(ol_hash);
    int i;
    int rcrd_cnt = database->rcrd_cnt;
    int freed = 0;
    for (i = 0; i < iterations; i++) {
        if (database->hashes[i] != NULL) {
            ol_val free_me = database->hashes[i]->data_ptr;
            //printf("Freeing data for %s.\n", database->hashes[i]->key);
            free(free_me);
            free(database->hashes[i]);
            freed++;
        }
    }

    free(database->hashes);
    free(database);
    if (freed != rcrd_cnt) {
        printf("[X] Error: Couldn't free all records.\n");
        printf("[X] Records freed: %i\n", freed);
        return 1;
    }
    return 0;
}

int64_t _ol_gen_hash(char *key) {
    const int64_t fnv_offset_bias = 0xcbf29ce484222325;
    const int64_t fnv_prime = 0x100000001b3;

    const int iterations = strlen(key);

    int i;
    int64_t hash = fnv_offset_bias;

    //printf("Key: %s\n", key);
    //printf("Iterations: %i\n", iterations);
    /* Rather insidiously hash the entire key, but truncate to 16 *
     * chars later.                                               */
    for(i = 0; i < iterations; i++) {
        hash ^= key[i];
        hash *= fnv_prime;
    }
    //printf("Hash: 0x%" PRIX64 "\n", hash);

    return hash;
}

ol_hash *_ol_get_hash(ol_database_obj db, char *key) {
    int64_t hash = _ol_gen_hash(key);
    printf("[-] Hash: 0x%" PRIX64 "\n", hash);
    int index = hash % (HASH_MALLOC/sizeof(ol_hash));

    if(db->hashes[index]->key != NULL) {
        if (strncmp(db->hashes[index]->key, key, KEY_SIZE) == 0) {
            printf("[-] Found existing key.\n");
            return db->hashes[index];
        } else {
            printf("[-] Found collision.\n");
            return db->hashes[index];
        }
    }
    return NULL;
}

ol_val ol_unjar(ol_database_obj db, char *key){
    ol_hash *hash = _ol_get_hash(db, key);

    if (hash != NULL) {
        return hash->data_ptr;
    }

    return NULL;
}

int ol_jar(ol_database_obj db, char *key, unsigned char *value, size_t vsize) {
    // Check to see if we have an existing entry with that key
    ol_hash *old_hash = _ol_get_hash(db, key);
    if (old_hash != NULL) {
        unsigned char *data = realloc(old_hash->data_ptr, vsize);
        if (memcpy(data, value, vsize) != data) {
            return 4;
        }

        old_hash->data_size = vsize;
        old_hash->data_ptr = data;
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
    int64_t hash = _ol_gen_hash(key);
    int index = hash % (HASH_MALLOC/sizeof(ol_hash));
    //printf("[-] Index: %i\n", index);

    db->hashes[index] = new_hash;
    db->rcrd_cnt += 1;
    return 0;
}

int ol_scoop(ol_database_obj db, char *key) {
    // you know... like scoop some data from the jar and eat it? All gone.
    ol_hash *old_hash = _ol_get_hash(db, key);
    if (old_hash != NULL) {
        ol_val free_me = old_hash->data_ptr;

        free(free_me);
        free(old_hash);

        old_hash = NULL;
        db->rcrd_cnt -= 1;

        int64_t hash = _ol_gen_hash(key);
        int index = hash % (HASH_MALLOC/sizeof(ol_hash));
        db->hashes[index] = NULL;

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
