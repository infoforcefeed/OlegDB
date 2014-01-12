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
#include <math.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "oleg.h"
#include "murmur3.h"

ol_database *ol_open(char *path, ol_filemode filemode){
    ol_database *new_db = malloc(sizeof(struct ol_database));

    size_t to_alloc = HASH_MALLOC;
    new_db->hashes = calloc(1, to_alloc);
    new_db->cur_ht_size = to_alloc;

    time_t created;
    time(&created);

    new_db->created = created;
    new_db->rcrd_cnt = 0;
    new_db->key_collisions = 0;
    strncpy(new_db->name, "OLEG", sizeof("OLEG"));
    strncpy(new_db->path, path, PATH_LENGTH);
    return new_db;
}

int ol_close(ol_database *db){
    int iterations = _ol_ht_bucket_max(db->cur_ht_size);
    int i;
    int rcrd_cnt = db->rcrd_cnt;
    int freed = 0;
    printf("[-] Freeing %d records.\n", rcrd_cnt);
    printf("[-] Iterations: %d.\n", iterations);
    for (i = 0; i <= iterations; i++) { // 8=======D
        if (db->hashes[i] != NULL) {
            ol_val free_me = db->hashes[i]->data_ptr;
            //printf("%s is free now.\n", db->hashes[i]->key);
            free(free_me);
            free(db->hashes[i]);
            freed++;
        }
    }

    free(db->hashes);
    free(db);
    if (freed != rcrd_cnt) {
        printf("[X] Error: Couldn't free all records.\n");
        printf("[X] Records freed: %i\n", freed);
        return 1;
    }
    return 0;
}

/*
int64_t _ol_gen_hash(const char *key) {
    // https://en.wikipedia.org/wiki/Fowler_Noll_Vo_hash
    const int64_t fnv_offset_bias = 0xcbf29ce484222325;
    const int64_t fnv_prime = 0x100000001b3;

    const int iterations = strlen(key);

    uint8_t i;
    int64_t hash = fnv_offset_bias;

    //printf("Key: %s\n", key);
    //printf("Iterations: %i\n", iterations);
    for(i = 0; i < iterations; i++) { // 8========D
        hash ^= key[i]; // XOR key octet
        hash *= fnv_prime; // Multiply by prime
    }
    //printf("Hash: 0x%" PRIX64 "\n", hash);

    return hash;
}
*/

inline int _ol_ht_bucket_max(size_t ht_size) {
    return (ht_size/sizeof(ol_hash *));
}

int _ol_calc_idx(size_t ht_size, int64_t hash) {
    int index;
    index = hash % _ol_ht_bucket_max(ht_size);
    //printf("max bucket size: %i\n", _ol_ht_bucket_max(ht_size));
    //printf("[X] Index: %u\n", index);
    return index;
}

/*
int _ol_get_index_insert(ol_hash **ht, size_t ht_size, int64_t hash, const char *key) {
    int index = _ol_calc_idx(ht_size, hash);

    if(ht[index]->key != NULL) {
        //db->key_collisions++;
        int i;
        int quad = 1;
        int hash_table_size = (ht_size/sizeof(hash));
        for (i = 0; i < hash_table_size; i++){ // 8===============D
            int tmp_idx = _ol_calc_idx(ht_size, (int64_t)(index + quad));
            if (ht[tmp_idx] == NULL || strncmp(ht[tmp_idx]->key, key, KEY_SIZE) == 0) {
                return tmp_idx;
            }
            quad += pow((double)i, (double)2);
        }
        return -1; // Everything is fucked and we are out of keyspace
    }
    return index;
}

int _ol_get_index_search(ol_database *db, int64_t hash, const char *key) {
    int index = _ol_calc_idx(db->cur_ht_size, hash);
    //printf("Calculated index: %ui\n", index);

    if(db->hashes[index]->key != NULL) {
        if (strncmp(db->hashes[index]->key, key, KEY_SIZE) == 0) {
            return index;
        } else {
            int i;
            int quad = 1;
            for (i = 0; i < (db->cur_ht_size/sizeof(hash)); i++) { // 8==========D
                size_t ht_size = db->cur_ht_size;
                int tmp_idx = _ol_calc_idx(ht_size, (int64_t)(index + quad));
                if (db->hashes[tmp_idx]->key != NULL) {
                    if (strncmp(db->hashes[index]->key, key, KEY_SIZE) == 0) {
                        return index;
                    }
                }
                quad += pow((double)i, (double)2);
            }
        }
    }
    return -1;
}
*/

int _ol_grow_and_rehash_db(ol_database *db) {
    int i;
    int new_index;
    ol_hash *bucket;
    ol_hash **tmp_hashes = NULL;

    size_t to_alloc = db->cur_ht_size * 2;
    printf("[-] Growing DB to %zu bytes\n", to_alloc);
    tmp_hashes = calloc(1, to_alloc);
    for (i = 0; i < _ol_ht_bucket_max(db->cur_ht_size); i++) {
        bucket = db->hashes[i];
        if (bucket != NULL) {
            new_index = _ol_get_index_insert(tmp_hashes, to_alloc,
                                             bucket->hash, bucket->key);
            tmp_hashes[new_index] = bucket;
        }
    }
    free(db->hashes);
    db->hashes = tmp_hashes;
    db->cur_ht_size = to_alloc;
    printf("[-] Current hash table size is now: %zu\n", to_alloc);
    return 0;
}

ol_val ol_unjar(ol_database *db, const char *key) {
    unsigned char hash[32];
    MurmurHash3_x64_128(key, strlen(key), DEVILS_SEED, &hash);
    int index = _ol_get_index_search(db, hash, key);

    if (index >= 0) {
        return db->hashes[index]->data_ptr;
    }

    return NULL;
}

int ol_jar(ol_database *db, const char *key, unsigned char *value, size_t vsize) {
    // Check to see if we have an existing entry with that key
    unsigned char hash[32];
    MurmurHash3_x64_128(key, strlen(key), DEVILS_SEED, &hash);
    int index = _ol_get_index_search(db, hash, key);
    // TODO check for errors (-1) from the search function
    ol_hash *old_hash = db->hashes[index];

    if (old_hash != NULL && index >= 0) {
        printf("[-] realloc\n");
        unsigned char *data = realloc(old_hash->data_ptr, vsize);
        if (memcpy(data, value, vsize) != data) {
            return 4;
        }

        old_hash->data_size = vsize;
        old_hash->data_ptr = data;
        return 0;
    }
    index = 0;

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
        return 3;
    }
    new_hash->data_ptr = data;
    new_hash->hash = hash;

    int bucket_max = _ol_ht_bucket_max(db->cur_ht_size);
    if (db->rcrd_cnt > 0 && db->rcrd_cnt == bucket_max) {
        printf("[-] Record count is now %i so regrowing hash table.\n", db->rcrd_cnt);
        int ret;
        ret = _ol_grow_and_rehash_db(db);
        if (ret > 0) {
            printf("Error: Problem rehashing DB. Error code: %i\n", ret);
            return 4;
        }
    }

    index = _ol_get_index_insert(db->hashes, db->cur_ht_size, hash, key);

    // Insert it into our db struct
    db->hashes[index] = new_hash;
    db->rcrd_cnt += 1;

    return 0;
}

int ol_scoop(ol_database *db, const char *key) {
    // you know... like scoop some data from the jar and eat it? All gone.
    unsigned char hash[32];
    MurmurHash3_x64_128(key, strlen(key), DEVILS_SEED, &hash);
    int index = _ol_get_index_search(db, hash, key);

    if (index < 0) {
        return 1;
    }

    ol_hash *old_hash = db->hashes[index];

    if (old_hash != NULL) {
        ol_val free_me = old_hash->data_ptr;

        free(free_me);
        free(old_hash);

        old_hash = NULL;

        db->hashes[index] = NULL;

        // Decrement our record count
        db->rcrd_cnt -= 1;

        return 0;
    }
    return 2;
}

int ol_uptime(ol_database *db) {
    // Make uptime
    time_t now;
    double diff;
    time(&now);
    diff = difftime(now, db->created);
    return diff;
}
