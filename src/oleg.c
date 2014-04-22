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

#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include "oleg.h"
#include "aol.h"
#include "logging.h"
#include "dump.h"
#include "murmur3.h"
#include "errhandle.h"
#include "rehash.h"
#include "utils.h"
#include "lz4.h"

/* Fix for the GNU extension strnlen not being available on some platforms */
#if defined(__MINGW32_VERSION) || (defined(__APPLE__) && \
    __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ < 1070)
size_t strnlen(char *text, size_t maxlen) {
    const char *last = memchr(text, '\0', maxlen);
    return last ? (size_t) (last - text) : maxlen;
}
#endif


inline int ol_ht_bucket_max(size_t ht_size) {
    return (ht_size/sizeof(ol_bucket *));
}

void _ol_get_file_name(ol_database *db, const char *p, char *o_file) {
    sprintf(o_file, "%s/%s.%s", db->path, db->name, p);
}

void _ol_enable(int feature, int *feature_set) {
    *feature_set |= feature;
}

void _ol_disable(int feature, int *feature_set) {
    *feature_set &= ~feature;
}

bool _ol_is_enabled(int feature, int *feature_set) {
    return (*feature_set & feature);
}

ol_database *ol_open(char *path, char *name, int features){
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
    new_db->aolfd = 0;

    /* Function pointers for feature flags */
    new_db->enable = &_ol_enable;
    new_db->disable = &_ol_disable;
    new_db->is_enabled = &_ol_is_enabled;

    /* Function pointer building file paths based on db name */
    new_db->get_db_file_name = &_ol_get_file_name;

    memset(new_db->name, '\0', DB_NAME_SIZE);
    strncpy(new_db->name, name, DB_NAME_SIZE);
    memset(new_db->path, '\0', PATH_LENGTH);
    strncpy(new_db->path, path, PATH_LENGTH);

    /* Make sure that the directory the database is in exists */
    struct stat st = {0};
    if (stat(path, &st) == -1) /* Check to see if the DB exists */
        mkdir(path, 0755);

    new_db->dump_file = calloc(1, 512);
    check_mem(new_db->dump_file);
    new_db->get_db_file_name(new_db, "dump", new_db->dump_file);

    new_db->aol_file = calloc(1, 512);
    check_mem(new_db->aol_file);
    new_db->get_db_file_name(new_db, "aol", new_db->aol_file);
    new_db->feature_set = features;
    new_db->state = OL_S_STARTUP;
    if (new_db->is_enabled(OL_F_APPENDONLY, &new_db->feature_set)) {
        ol_aol_init(new_db);
        check(ol_aol_restore(new_db) == 0, "Error restoring from AOL file");
    }
    new_db->state = OL_S_AOKAY;

    return new_db;

error:
    return NULL;
}

int _ol_close(ol_database *db){
    int iterations = ol_ht_bucket_max(db->cur_ht_size);
    int i;
    int rcrd_cnt = db->rcrd_cnt;
    int freed = 0;
    debug("Freeing %d records.", rcrd_cnt);
    debug("Hash table iterations: %d.", iterations);
    for (i = 0; i < iterations; i++) { /* 8=======D */
        if (db->hashes[i] != NULL) {
            ol_bucket *ptr;
            ol_bucket *next;
            for (ptr = db->hashes[i]; NULL != ptr; ptr = next) {
                next = ptr->next;
                _ol_free_bucket(ptr);
                freed++;
            }
        }
    }

    debug("Force flushing files");

    if (db->aolfd) {
        fflush(db->aolfd);
        fclose(db->aolfd);
    }

    debug("Files flushed to disk");

    free(db->hashes);
    free(db->dump_file);
    free(db->aol_file);
    db->feature_set = 0;
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

/* TODO: Refactor this to not allocate a new str, but to fill out a passed in
 * str. This keeps memory management to the parent function. */
static inline char *_ol_trunc(const char *key, size_t klen) {
    /* Silently truncate because #yolo */
    size_t real_key_len = klen > KEY_SIZE ? KEY_SIZE : klen;
    char *_key = malloc(real_key_len+1);
    strncpy(_key, key, real_key_len);
    _key[real_key_len] = '\0';
    return _key;
}

ol_bucket *_ol_get_bucket(const ol_database *db, const uint32_t hash, const char *key, size_t klen) {
    int index = _ol_calc_idx(db->cur_ht_size, hash);
    size_t larger_key = 0;
    if (db->hashes[index] != NULL) {
        ol_bucket *tmp_bucket;
        tmp_bucket = db->hashes[index];
        larger_key = tmp_bucket->klen > klen ? tmp_bucket->klen : klen;
        if (strncmp(tmp_bucket->key, key, larger_key) == 0) {
            return tmp_bucket;
        } else if (tmp_bucket->next != NULL) {
            do {
                tmp_bucket = tmp_bucket->next;
                larger_key = tmp_bucket->klen > klen ? tmp_bucket->klen : klen;
                if (strncmp(tmp_bucket->key, key, larger_key) == 0)
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

int ol_unjar(ol_database *db, const char *key, size_t klen, unsigned char **data) {
    return ol_unjar_ds(db, key, klen, data, NULL);
}

static inline int _has_bucket_expired(const ol_bucket *bucket) {
    struct tm utctime;
    time_t current;
    time_t made;

    /* So dumb */
    time(&current);
    gmtime_r(&current, &utctime);
    current = timegm(&utctime);
    if (bucket->expiration != NULL) {
        made = timelocal(bucket->expiration);
        debug("Made Expiration: %lu", (long)made);
    }

    /* For some reason you can't compare 0 to a time_t. */
    if (bucket->expiration == NULL || current < made) {
        return 0;
    }
    return 1;
}
int ol_unjar_ds(ol_database *db, const char *key, size_t klen, unsigned char **data, size_t *dsize) {
    uint32_t hash;

    char *_key = _ol_trunc(key, klen);
    size_t _klen = strnlen(_key, KEY_SIZE);
    MurmurHash3_x86_32(_key, _klen, DEVILS_SEED, &hash);
    ol_bucket *bucket = _ol_get_bucket(db, hash, _key, _klen);
    free(_key);

    if (bucket != NULL) {
        if (!_has_bucket_expired(bucket)) {
            if (data == NULL)
                return 0;

            /* Allocate memory to store memcpy'd data into. */
            *data = malloc(bucket->original_size);
            check(*data != NULL, "Could not allocate memory for compressed data.");

            /* Decomperss with LZ4 if enabled */
            if (db->is_enabled(OL_F_LZ4, &db->feature_set)) {
                int processed = 0;
                processed = LZ4_decompress_fast((const char*)bucket->data_ptr,
                                                (char *)*data,
                                                bucket->original_size);
                check(processed != bucket->data_size, "Could not decompress data.");

                /* "memcpy never fails!" */
                void *ret = memcpy(dsize, &bucket->original_size, sizeof(size_t));
                check(ret != dsize, "Could not copy data size into input data_size param.");
            } else {
                /* Avoid two memcpy operations: */
                if (dsize != NULL) {
                    void *ret = memcpy(dsize, &bucket->data_size, sizeof(size_t));
                    check(ret != dsize, "Could not copy data size into input data_size param.");
                }
                /* We know data isn't NULL by this point. */
                void *ret = memcpy(data, &bucket->data_ptr, bucket->data_size);
                check(ret != data, "Could not copy data into output data param.");
            }

            /* Key found, tell somebody. */
            return 0;
        } else {
            /* It's dead, get rid of it. */
            ol_scoop(db, key, klen);
        }
    }

    return 1;

error:
    return 2;
}

static inline int _ol_reallocate_bucket(ol_database *db, ol_bucket *bucket,
        unsigned char *value, size_t vsize, const char *ct, const size_t ctsize) {
    debug("Reallocating bucket.");

    /* Compress using LZ4 if enabled */
    size_t cmsize = 0;
    unsigned char* compressed = NULL;
    if (db->is_enabled(OL_F_LZ4, &db->feature_set)) {
        compressed = malloc(vsize);
        cmsize = (size_t)LZ4_compress((const char*)value, (char*)compressed,
                                      (int)vsize);
    }

    unsigned char *data = realloc(bucket->data_ptr, vsize);
    if (memcpy(data, value, vsize) != data)
        return 4;

    char *ct_real = realloc(bucket->content_type, ctsize+1);
    if (strncpy(ct_real, ct, ctsize) != ct_real)
        return 5;
    ct_real[ctsize] = '\0';

    /* bucket->klen = _klen; */
    bucket->ctype_size = ctsize;
    bucket->content_type = ct_real;
    if (bucket->expiration != NULL)
        free(bucket->expiration);
    bucket->expiration = NULL;

    /* Set original_size regardless of lz4 compression. This ensures we always
     * have something to write to the AOL. */
    bucket->original_size = vsize;
    if(db->is_enabled(OL_F_LZ4, &db->feature_set)) {
        bucket->data_size = cmsize;
        bucket->data_ptr = compressed;
    } else {
        bucket->data_size = vsize;
        bucket->data_ptr = data;
    }

    if(db->is_enabled(OL_F_APPENDONLY, &db->feature_set) &&
            db->state != OL_S_STARTUP) {
        ol_aol_write_cmd(db, "JAR", bucket);
    }

    return 0;
}

int _ol_jar(ol_database *db, const char *key, size_t klen, unsigned char *value,
        size_t vsize, const char *ct, const size_t ctsize) {
    int ret;
    uint32_t hash;

    /* Free the _key as soon as possible */
    char *_key = _ol_trunc(key, klen);
    size_t _klen = strnlen(_key, KEY_SIZE);
    MurmurHash3_x86_32(_key, _klen, DEVILS_SEED, &hash);
    ol_bucket *bucket = _ol_get_bucket(db, hash, _key, _klen);

    /* Check to see if we have an existing entry with that key */
    if (bucket) {
        free(_key);
        return _ol_reallocate_bucket(db, bucket, value, vsize, ct, ctsize);
    }

    /* Looks like we don't have an old hash */
    ol_bucket *new_bucket = malloc(sizeof(ol_bucket));
    if (new_bucket == NULL)
        return 1;

    if (strncpy(new_bucket->key, _key, KEY_SIZE) != new_bucket->key) {
        free(_key);
        return 2;
    }
    free(_key);
    new_bucket->klen = _klen;
    new_bucket->expiration = NULL;

    new_bucket->next = NULL;

    new_bucket->hash = hash;

    new_bucket->ctype_size = ctsize;
    char *ct_real = calloc(1, ctsize+1);
    if (strncpy(ct_real, ct, ctsize) != ct_real)
        return 7;
    ct_real[ctsize] = '\0';
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

    new_bucket->original_size = vsize;
    if(db->is_enabled(OL_F_LZ4, &db->feature_set)) {
        /* Compress using LZ4 if enabled */
        size_t cmsize = 0;
        unsigned char* compressed = NULL;
        compressed = malloc(vsize);
        cmsize = (size_t)LZ4_compress((const char*)value, (char*)compressed,
                                      (int)vsize);

        new_bucket->data_size = cmsize;
        new_bucket->data_ptr = compressed;
    } else {
        new_bucket->data_size = vsize;
        unsigned char *data = calloc(1, vsize);
        if (memcpy(data, value, vsize) != data)
            return 3;
        new_bucket->data_ptr = data;
    }

    ret = _ol_set_bucket(db, new_bucket);

    if(ret > 0)
        ol_log_msg(LOG_ERR, "Problem inserting item: Error code: %i", ret);

    if(db->is_enabled(OL_F_APPENDONLY, &db->feature_set) &&
            db->state != OL_S_STARTUP) {
        ol_aol_write_cmd(db, "JAR", new_bucket);
    }

    return 0;
}

int ol_jar(ol_database *db, const char *key, size_t klen, unsigned char *value,
        size_t vsize) {
    return _ol_jar(db, key, klen, value, vsize, "application/octet-stream", 24);
}

int ol_jar_ct(ol_database *db, const char *key, size_t klen, unsigned char *value,
        size_t vsize, const char *content_type, const size_t content_type_size) {
    return _ol_jar(db, key, klen, value, vsize, content_type, content_type_size);
}

int ol_spoil(ol_database *db, const char *key, size_t klen, struct tm *expiration_date) {
    uint32_t hash;
    char *_key = _ol_trunc(key, klen);
    size_t _klen = strnlen(_key, KEY_SIZE);

    MurmurHash3_x86_32(_key, _klen, DEVILS_SEED, &hash);
    ol_bucket *bucket = _ol_get_bucket(db, hash, _key, _klen);
    free(_key);

    if (bucket != NULL) {
        if (bucket->expiration == NULL)
            bucket->expiration = malloc(sizeof(struct tm));
        else
            debug("Hmmm, bucket->expiration wasn't null.");
        memcpy(bucket->expiration, expiration_date, sizeof(struct tm));
        debug("New expiration time: %lu", (long)mktime(bucket->expiration));

#ifdef DEBUG
        struct tm utctime;
        time_t current;

        /* So dumb */
        time(&current);
        gmtime_r(&current, &utctime);
        current = timegm(&utctime);
        debug("Current time: %lu", (long)current);
#endif
        if(db->is_enabled(OL_F_APPENDONLY, &db->feature_set) &&
                db->state != OL_S_STARTUP) {
            ol_aol_write_cmd(db, "SPOIL", bucket);
        }
        return 0;
    }

    return 1;
}

int ol_scoop(ol_database *db, const char *key, size_t klen) {
    /* you know... like scoop some data from the jar and eat it? All gone. */
    uint32_t hash;
    char *_key = _ol_trunc(key, klen);
    size_t _klen = strnlen(_key, KEY_SIZE);
    size_t larger_key = 0;

    MurmurHash3_x86_32(_key, _klen, DEVILS_SEED, &hash);
    int index = _ol_calc_idx(db->cur_ht_size, hash);

    if (index < 0) {
        free(_key);
        return 1;
    }

    if (db->hashes[index] != NULL) {
        ol_bucket *bucket = db->hashes[index];
        larger_key = bucket->klen > _klen ? bucket->klen : _klen;
        if (strncmp(bucket->key, _key, larger_key) == 0) {
            if (bucket->next != NULL) {
                db->hashes[index] = bucket->next;
            } else {
                db->hashes[index] = NULL;
            }
            if(db->is_enabled(OL_F_APPENDONLY, &db->feature_set) &&
                    db->state != OL_S_STARTUP) {
                ol_aol_write_cmd(db, "SCOOP", bucket);
            }
            _ol_free_bucket(bucket);
            free(_key);
            db->rcrd_cnt -= 1;
            return 0;
        } else { /* Keys weren't the same, traverse the bucket LL */
            ol_bucket *last;
            do {
                last = bucket;
                bucket = bucket->next;
                larger_key = bucket->klen > klen ? bucket->klen : klen;
                if (strncmp(bucket->key, _key, larger_key) == 0) {
                    if (bucket->next != NULL)
                        last->next = bucket->next;
                    _ol_free_bucket(bucket);
                    db->rcrd_cnt -= 1;
                    free(_key);
                    return 0;
                }
            } while (bucket->next != NULL);
        }
    }
    free(_key);
    return 2;
}

char *ol_content_type(ol_database *db, const char *key, size_t klen) {
    uint32_t hash;
    char *_key = _ol_trunc(key, klen);
    size_t _klen = strnlen(_key, KEY_SIZE);
    MurmurHash3_x86_32(_key, _klen, DEVILS_SEED, &hash);
    ol_bucket *bucket = _ol_get_bucket(db, hash, _key, _klen);
    free(_key);

    if (bucket != NULL) {
        if (!_has_bucket_expired(bucket)) {
            return bucket->content_type;
        } else {
            /* It's dead, get rid of it. */
            int res = ol_scoop(db, key, klen);
            if (res > 0)
                debug("Could not delete a bucket!");
        }
    }

    return NULL;
}

struct tm *ol_expiration_time(ol_database *db, const char *key, size_t klen) {
    uint32_t hash;
    char *_key = _ol_trunc(key, klen);
    size_t _klen = strnlen(_key, KEY_SIZE);
    MurmurHash3_x86_32(_key, _klen, DEVILS_SEED, &hash);
    ol_bucket *bucket = _ol_get_bucket(db, hash, _key, _klen);
    free(_key);

    if (bucket != NULL && bucket->expiration != NULL) {
        if (!_has_bucket_expired(bucket)) {
            return bucket->expiration;
        } else {
            /* It's dead, get rid of it. */
            int res = ol_scoop(db, key, klen);
            if (res > 0)
                debug("Could not delete a bucket!");
        }
    }

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
