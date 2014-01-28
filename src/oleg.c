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

#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include "oleg.h"
#include "logging.h"
#include "dump.h"
#include "murmur3.h"
#include "errhandle.h"


inline int ol_ht_bucket_max(size_t ht_size) {
    return (ht_size/sizeof(ol_bucket *));
}

void _ol_get_dump_name(ol_database *db, char *dump_file) {
    sprintf(dump_file, "%s/%s.dump", db->path, db->name);
}

ol_database *ol_open(char *path, char *name, ol_filemode filemode){
    debug("Opening \"%s\" database", name);
    ol_database *new_db = malloc(sizeof(struct ol_database));

    size_t to_alloc = HASH_MALLOC;
    new_db->hashes = calloc(1, to_alloc);
    new_db->cur_ht_size = to_alloc;
    /* NULL everything */
    int i;
    for (i = 0; i < ol_ht_bucket_max(to_alloc); i++){
        new_db->hashes[i] = NULL;
    }

    time_t created;
    time(&created);

    new_db->created = created;
    new_db->rcrd_cnt = 0;
    new_db->key_collisions = 0;

    strncpy(new_db->name, name, strlen(name));
    strncpy(new_db->path, path, PATH_LENGTH);

    struct stat st = {0};
    if (stat(path, &st) == -1)
        mkdir(path, 0755);

    char dump_file[512];
    new_db->get_db_name = &_ol_get_dump_name;
    new_db->get_db_name(new_db, dump_file);

    new_db->dump_file = calloc(1, strlen(dump_file));
    memcpy(new_db->dump_file, dump_file, strlen(dump_file));

    return new_db;
}

int _ol_close(ol_database *db){
    int iterations = ol_ht_bucket_max(db->cur_ht_size);
    int i;
    int rcrd_cnt = db->rcrd_cnt;
    int freed = 0;
    debug("Freeing %d records.", rcrd_cnt);
    debug("Hash table iterations: %d.", iterations);
    for (i = 0; i <= iterations; i++) { /* 8=======D */
        if (db->hashes[i] != NULL) {
            ol_bucket *ptr;
            ol_bucket *next;
            for (ptr = db->hashes[i]; NULL != ptr; ptr = next) {
                next = ptr->next;
                free(ptr->content_type);
                free(ptr->data_ptr);
                free(ptr);
                freed++;
            }
        }
    }

    free(db->hashes);
    free(db->dump_file);
    free(db);
    if (freed != rcrd_cnt) {
        ol_log_msg(LOG_INFO, "Error: Couldn't free all records.");
        ol_log_msg(LOG_INFO, "Records freed: %i\n", freed);
        return 1;
    }
    return 0;
}

int ol_close_save(ol_database *db) {
    debug("Saving and closing \"%s\" database.", db->name);
    check(ol_save_db(db) == 0, "Could not save DB.");
    check(_ol_close(db) == 0, "Could not close DB.");

    return 0;

error:
    return 1;
}

int ol_close(ol_database *db) {
    debug("Closing \"%s\" database.", db->name);
    check(_ol_close(db) == 0, "Could not close DB.");

    return 0;

error:
    return 1;
}

int _ol_calc_idx(const size_t ht_size, const uint32_t hash) {
    int index;
    index = hash % ol_ht_bucket_max(ht_size);
    return index;
}

ol_bucket *_ol_get_last_bucket_in_slot(ol_bucket *bucket) {
    ol_bucket *tmp_bucket = bucket;
    while (tmp_bucket->next != NULL)
        tmp_bucket = tmp_bucket->next;
    return tmp_bucket;
}

ol_bucket *_ol_get_bucket(const ol_database *db, const uint32_t hash, const char *key) {
    int index = _ol_calc_idx(db->cur_ht_size, hash);
    if (db->hashes[index] != NULL) {
        ol_bucket *tmp_bucket = db->hashes[index];
        if (strncmp(tmp_bucket->key, key, KEY_SIZE) == 0) {
            return tmp_bucket;
        } else if (tmp_bucket->next != NULL) {
            do {
                tmp_bucket = tmp_bucket->next;
                if (strncmp(tmp_bucket->key, key, KEY_SIZE) == 0)
                    return tmp_bucket;
            } while (tmp_bucket->next != NULL);
        }
    }
    return NULL;
}

int _ol_set_bucket(ol_database *db, ol_bucket *bucket) {
    /* TODO: error codes? */
    int index = _ol_calc_idx(db->cur_ht_size, bucket->hash);
    if (db->hashes[index] != NULL) {
        db->key_collisions++;
        ol_bucket *tmp_bucket = db->hashes[index];
        tmp_bucket = _ol_get_last_bucket_in_slot(tmp_bucket);
        tmp_bucket->next = bucket;
    } else {
        db->hashes[index] = bucket;
    }
    db->rcrd_cnt++;
    return 0;
}

int _ol_grow_and_rehash_db(ol_database *db) {
    int i;
    int new_index;
    ol_bucket *bucket;
    ol_bucket **tmp_hashes = NULL;

    size_t to_alloc = db->cur_ht_size * 2;
    debug("Growing DB to %zu bytes.", to_alloc);
    tmp_hashes = calloc(1, to_alloc);
    check_mem(tmp_hashes);
    for (i = 0; i < ol_ht_bucket_max(db->cur_ht_size); i++) {
        bucket = db->hashes[i];
        if (bucket != NULL) {
            new_index = _ol_calc_idx(db->cur_ht_size, bucket->hash);
            if (tmp_hashes[new_index] != NULL) {
                ol_bucket *last_bucket = _ol_get_last_bucket_in_slot(
                        tmp_hashes[new_index]);
                last_bucket->next = bucket;
            } else {
                tmp_hashes[new_index] = bucket;
            }
        }
    }
    free(db->hashes);
    db->hashes = tmp_hashes;
    db->cur_ht_size = to_alloc;
    debug("Current hash table size is now: %zu bytes.", to_alloc);
    return 0;

error:
    return -1;
}

ol_val ol_unjar(ol_database *db, const char *key) {
    uint32_t hash;
    MurmurHash3_x86_32(key, strlen(key), DEVILS_SEED, &hash);
    ol_bucket *bucket = _ol_get_bucket(db, hash, key);

    if (bucket != NULL)
        return bucket->data_ptr;

    return NULL;
}

int _ol_jar(ol_database *db, const char *key,unsigned char *value, size_t vsize,
        const char *ct, const size_t ctsize) {
    int ret;
    uint32_t hash;
    MurmurHash3_x86_32(key, strlen(key), DEVILS_SEED, &hash);
    ol_bucket *bucket = _ol_get_bucket(db, hash, key);

    /* Check to see if we have an existing entry with that key */
    if (bucket != NULL && strncmp(bucket->key, key, KEY_SIZE) == 0) {
        unsigned char *data = realloc(bucket->data_ptr, vsize);
        if (memcpy(data, value, vsize) != data)
            return 4;

        char *ct_real = realloc(bucket->content_type, ctsize);
        if (memcpy(ct_real, ct, ctsize) != ct_real)
            return 5;

        bucket->ctype_size = ctsize;
        bucket->content_type = ct_real;
        bucket->data_size = vsize;
        bucket->data_ptr = data;
        return 0;
    }

    /* Looks like we don't have an old hash */
    ol_bucket *new_bucket = malloc(sizeof(ol_bucket));
    if (new_bucket == NULL)
        return 1;

    new_bucket->next = NULL;

    /* Silently truncate because #yolo */
    if (strncpy(new_bucket->key, key, KEY_SIZE) != new_bucket->key)
        return 2;

    new_bucket->data_size = vsize;
    unsigned char *data = calloc(1, vsize);
    if (memcpy(data, value, vsize) != data)
        return 3;
    new_bucket->data_ptr = data;
    new_bucket->hash = hash;

    new_bucket->ctype_size = ctsize;
    char *ct_real = calloc(1, ctsize);
    if (memcpy(ct_real, ct, ctsize) != ct_real)
        return 7;
    new_bucket->content_type = ct_real;

    int bucket_max = ol_ht_bucket_max(db->cur_ht_size);
    /* TODO: rehash this shit at 80% */
    if (db->rcrd_cnt > 0 && db->rcrd_cnt == bucket_max) {
        debug("Record count is now %i; growing hash table.", db->rcrd_cnt);
        ret = _ol_grow_and_rehash_db(db);
        if (ret > 0) {
            ol_log_msg(LOG_ERR, "Problem rehashing DB. Error code: %i", ret);
            return 4;
        }
    }

    ret = _ol_set_bucket(db, new_bucket);

    if(ret > 0)
        ol_log_msg(LOG_ERR, "Problem inserting item: Error code: %i", ret);

    return 0;
}

int ol_jar(ol_database *db,const char *key,unsigned char *value,size_t vsize) {
    return _ol_jar(db, key, value, vsize, "application/octet-stream", 24);
}

int ol_jar_ct(ol_database *db, const char *key,unsigned char *value, size_t vsize,
        const char *content_type, const size_t content_type_size) {
    return _ol_jar(db, key, value, vsize, content_type, content_type_size);
}

int ol_scoop(ol_database *db, const char *key) {
    /* you know... like scoop some data from the jar and eat it? All gone. */
    uint32_t hash;
    MurmurHash3_x86_32(key, strlen(key), DEVILS_SEED, &hash);
    int index = _ol_calc_idx(db->cur_ht_size, hash);

    if (index < 0)
        return 1;

    if (db->hashes[index] != NULL) {
        ol_bucket *bucket = db->hashes[index];

        if (strncmp(bucket->key, key, KEY_SIZE) == 0){
            if (bucket->next != NULL) {
                db->hashes[index] = bucket->next;
            } else {
                db->hashes[index] = NULL;
            }
            free(bucket->data_ptr);
            free(bucket);
            db->rcrd_cnt -= 1;
            return 0;
        } else if (bucket->next != NULL) {
            ol_bucket *last;
            do {
                last = bucket;
                bucket = bucket->next;
                if (strncmp(bucket->key, key, KEY_SIZE) == 0) {
                    if (bucket->next != NULL)
                        last->next = bucket->next;
                    free(bucket->content_type);
                    free(bucket->data_ptr);
                    free(bucket);
                    db->rcrd_cnt -= 1;
                    return 0;
                }
            } while (bucket->next != NULL);
        }
    }
    return 2;
}

char *ol_content_type(ol_database *db, const char *key) {
    uint32_t hash;
    MurmurHash3_x86_32(key, strlen(key), DEVILS_SEED, &hash);
    ol_bucket *bucket = _ol_get_bucket(db, hash, key);

    if (bucket != NULL)
        return bucket->content_type;

    return NULL;
}

int ol_uptime(ol_database *db) {
    /* Make uptime */
    time_t now;
    double diff;
    time(&now);
    diff = difftime(now, db->created);
    return diff;
}
