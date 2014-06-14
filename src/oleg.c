/* Main oleg source code. See corresponding header file for more info. */
#include <assert.h>
#include <fcntl.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#include "oleg.h"
#include "file.h"
#include "aol.h"
#include "logging.h"
#include "murmur3.h"
#include "errhandle.h"
#include "rehash.h"
#include "utils.h"
#include "lz4.h"

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
    return (*feature_set & feature) == feature;
}

ol_database *ol_open(char *path, char *name, int features){
    debug("Opening \"%s\" database", name);
    ol_database *new_db = calloc(1, sizeof(ol_database));

    size_t to_alloc = HASH_MALLOC;
    new_db->hashes = calloc(1, to_alloc);
    new_db->cur_ht_size = to_alloc;

    /* Make sure that the directory the database is in exists */
    struct stat st = {0};
    /* Check to see if the DB exists */
    if (!_ol_get_stat(path, &st) || !S_ISDIR(st.st_mode)) {
        int ret = mkdir(path, 0755);
        check(ret == 0, "Can't mkdir for database");
    }

    time_t created;
    time(&created);
    new_db->meta = malloc(sizeof(ol_meta));
    new_db->meta->created = created;
    new_db->meta->key_collisions = 0;
    new_db->rcrd_cnt = 0;
    new_db->val_size = 0;

    /* Null every pointer before initialization in case something goes wrong */
    new_db->aol_file = NULL;
    new_db->aolfd = 0;

    /* Function pointers for feature flags */
    new_db->enable = &_ol_enable;
    new_db->disable = &_ol_disable;
    new_db->is_enabled = &_ol_is_enabled;

    /* Function pointer building file paths based on db name */
    new_db->get_db_file_name = &_ol_get_file_name;

    /* Zero out the db name and path strings */
    memset(new_db->name, '\0', DB_NAME_SIZE);
    strncpy(new_db->name, name, DB_NAME_SIZE);
    memset(new_db->path, '\0', PATH_LENGTH);
    strncpy(new_db->path, path, PATH_LENGTH);

    /* mmap() the values into memory. */
    new_db->values = NULL;

    _ol_open_values(new_db);

    new_db->feature_set = features;
    new_db->state = OL_S_STARTUP;
    /* Allocate a splay tree if we're into that */
    if (new_db->is_enabled(OL_F_SPLAYTREE, &new_db->feature_set)) {
        new_db->tree = malloc(sizeof(ol_splay_tree));
        new_db->tree->root = NULL;
        new_db->tree->rcrd_cnt = 0;
    }

    /* We figure out the filename now incase someone flips the aol_init bit
     * later.
     */
    new_db->aol_file = calloc(1, 512);
    check_mem(new_db->aol_file);
    new_db->get_db_file_name(new_db, "aol", new_db->aol_file);

    /* Lets use an append-only log file */
    if (new_db->is_enabled(OL_F_APPENDONLY, &new_db->feature_set)) {
        ol_aol_init(new_db);
        check(ol_aol_restore(new_db) == 0, "Error restoring from AOL file");
    }
    new_db->state = OL_S_AOKAY;

    return new_db;

error:
    /* Make sure we free the database first */
    free(new_db->aol_file);
    free(new_db);
    return NULL;
}

int ol_close(ol_database *db){
    debug("Closing \"%s\" database.", db->name);

    int iterations = ol_ht_bucket_max(db->cur_ht_size);
    int rcrd_cnt = db->rcrd_cnt;
    int freed = 0;
    debug("Freeing %d records.", rcrd_cnt);
    debug("Hash table iterations: %d.", iterations);

    int i = 0;
    for (; i < iterations; i++) { /* 8=======D */
        if (db->hashes[i] != NULL) {
            ol_bucket *ptr, *next;
            for (ptr = db->hashes[i]; NULL != ptr; ptr = next) {
                next = ptr->next;
                _ol_free_bucket(&ptr);
                freed++;
            }
        }
    }

    if (db->is_enabled(OL_F_SPLAYTREE, &db->feature_set) && db->tree != NULL) {
        debug("Destroying tree.");
        ols_close(db->tree);
        free(db->tree);
        db->tree = NULL;
    }

    if (db->aolfd) {
        debug("Force flushing files");
        fflush(db->aolfd);
        fclose(db->aolfd);
        debug("Files flushed to disk");
    }

    /* Sync and close values file. */
    if (db->val_size > 0) {
        msync(db->values, db->val_size, MS_SYNC);
        munmap(db->values, db->val_size);
    }
    free(db->aol_file);
    free(db->meta);
    free(db->hashes);
    free(db);

    check(freed == rcrd_cnt, "Error: Couldn't free all records.\nRecords freed: %d", freed);
    ol_log_msg(LOG_INFO, "Database closed. Remember to drink your coffee.");
    return 0;

error:
    return 1;
}

static inline void _ol_trunc(const char *key, size_t klen, char *out) {
    /* Silently truncate because #yolo */
    size_t real_key_len = klen > KEY_SIZE ? KEY_SIZE : klen;
    strncpy(out, key, real_key_len);
    out[real_key_len] = '\0';
}

int _ol_set_bucket(ol_database *db, ol_bucket *bucket, uint32_t hash) {
    /* TODO: error codes? */
    int index = _ol_calc_idx(db->cur_ht_size, hash);
    if (db->hashes[index] != NULL) {
        db->meta->key_collisions++;
        ol_bucket *tmp_bucket = db->hashes[index];
        tmp_bucket = _ol_get_last_bucket_in_slot(tmp_bucket);
        tmp_bucket->next = bucket;
    } else {
        db->hashes[index] = bucket;
    }
    db->rcrd_cnt++;

    if (db->is_enabled(OL_F_SPLAYTREE, &db->feature_set)) {
        /* Put the bucket into the tree */
        ol_splay_tree_node *node = NULL;
        node = ols_insert(db->tree, bucket->key, bucket->klen, bucket);
        /* Make sure the bucket can reference the node. */
        bucket->node = node;
    }

    return 0;
}

int ol_unjar(ol_database *db, const char *key, size_t klen, unsigned char **data) {
    return ol_unjar_ds(db, key, klen, data, NULL);
}

int ol_exists(ol_database *db, const char *key, size_t klen) {
    return ol_unjar_ds(db, key, klen, NULL, NULL);
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
    } else {
        return 0;
    }

    /* For some reason you can't compare 0 to a time_t. */
    if (current < made) {
        return 0;
    }
    return 1;
}

ol_bucket *ol_get_bucket(const ol_database *db, const char *key, const size_t klen, char (*_key)[KEY_SIZE], size_t *_klen) {
    uint32_t hash;
    _ol_trunc(key, klen, *_key);
    *_klen = strnlen(*_key, KEY_SIZE);

    if (_klen == 0)
        return NULL;

    MurmurHash3_x86_32(*_key, *_klen, DEVILS_SEED, &hash);

    int index = _ol_calc_idx(db->cur_ht_size, hash);
    if (db->hashes[index] != NULL) {
        size_t larger_key = 0;
        ol_bucket *tmp_bucket;
        tmp_bucket = db->hashes[index];
        larger_key = tmp_bucket->klen > klen ? tmp_bucket->klen : klen;
        if (strncmp(tmp_bucket->key, key, larger_key) == 0) {
            return tmp_bucket;
        } else if (tmp_bucket->next != NULL) {
            /* Keys were not the same, traverse the linked list to see if it's
             * farther down. */
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

int ol_unjar_ds(ol_database *db, const char *key, size_t klen, unsigned char **data, size_t *dsize) {
    char _key[KEY_SIZE] = {'\0'};
    size_t _klen = 0;
    ol_bucket *bucket = ol_get_bucket(db, key, klen, &_key, &_klen);
    check_warn(_klen > 0, "Key length of zero not allowed.");

    if (bucket != NULL) {
        if (!_has_bucket_expired(bucket)) {
            /* We don't need to fill out the data so just return 'we found the key'. */
            if (data == NULL)
                return 0;

            /* Allocate memory to store memcpy'd data into. */
            *data = malloc(bucket->original_size);
            check(*data != NULL, "Could not allocate memory for compressed data.");

            if (dsize != NULL) {
                /* "strncpy never fails!" */
                size_t *ret = memcpy(dsize, &bucket->original_size, sizeof(size_t));
                check(ret == dsize, "Could not copy data size into input data_size param.");
            }

            unsigned char *data_ptr = db->values + bucket->data_offset;
            /* Decomperss with LZ4 if enabled */
            if (db->is_enabled(OL_F_LZ4, &db->feature_set)) {
                int processed = 0;
                processed = LZ4_decompress_fast((char *)data_ptr,
                                                (char *)*data,
                                                bucket->original_size);
                check(processed == bucket->data_size, "Could not decompress data.");
            } else {
                /* We know data isn't NULL by this point. */
                char *ret = strncpy((char *)*data, (char *)data_ptr, bucket->original_size);
                check(ret == (char *)*data, "Could not copy data into output data param.");
            }

            /* Key found, tell somebody. */
            return 0;
        } else {
            /* It's dead, get rid of it. */
            check(ol_scoop(db, key, klen) == 0, "Scoop failed");
        }
    }

    return 1;

error:
    return 2;
}

static inline int _ol_reallocate_bucket(ol_database *db, ol_bucket *bucket,
        unsigned char *value, size_t vsize, const char *ct, const size_t ctsize) {
    debug("Reallocating bucket.");

    unsigned char *old_data_ptr = db->values + bucket->data_offset;
    /* Clear out the old data in the file. */
    if (bucket->data_size > 0)
        memset(old_data_ptr, '\0', bucket->data_size);
    /* Compute the new position of the data in the values file: */
    const size_t new_offset = db->val_size;
    unsigned char *new_data_ptr = NULL;

    /* Compress using LZ4 if enabled */
    size_t cmsize = 0;
    if (db->is_enabled(OL_F_LZ4, &db->feature_set)) {
        int maxoutsize = LZ4_compressBound(vsize);
        _ol_ensure_values_file_size(db, maxoutsize);
        new_data_ptr = db->values + db->val_size;

        cmsize = (size_t)LZ4_compress((char*)value, (char*)new_data_ptr,
                                      (int)vsize);
    } else {
        _ol_ensure_values_file_size(db, vsize);
        new_data_ptr = db->values + db->val_size;
        if (memcpy(new_data_ptr, value, vsize) != new_data_ptr)
            return 4;
    }

    char *ct_real = realloc(bucket->content_type, ctsize+1);
    if (strncpy(ct_real, ct, ctsize) != ct_real) {
        free(ct_real);
        return 5;
    }
    ct_real[ctsize] = '\0';

    /* bucket->klen = _klen; */
    bucket->ctype_size = ctsize;
    bucket->content_type = ct_real;
    if (bucket->expiration != NULL) {
        free(bucket->expiration);
        bucket->expiration = NULL;
    }

    /* Set original_size regardless of lz4 compression. This ensures we always
     * have something to write to the AOL. */
    bucket->original_size = vsize;
    if(db->is_enabled(OL_F_LZ4, &db->feature_set)) {
        bucket->data_size = cmsize;
    } else {
        bucket->data_size = vsize;
    }
    bucket->data_offset = new_offset;

    /* Remember to increment the tracked data size of the DB. */
    db->val_size += bucket->data_size;

    if(db->is_enabled(OL_F_APPENDONLY, &db->feature_set) && db->state != OL_S_STARTUP) {
        ol_aol_write_cmd(db, "JAR", bucket);
    }

    return 0;
}

int _ol_jar(ol_database *db, const char *key, size_t klen, unsigned char *value,
        size_t vsize, const char *ct, const size_t ctsize) {
    int ret;
    char _key[KEY_SIZE] = {'\0'};
    size_t _klen = 0;
    ol_bucket *bucket = ol_get_bucket(db, key, klen, &_key, &_klen);
    check_warn(_klen > 0, "Key length of zero not allowed.");

    /* Check to see if we have an existing entry with that key */
    if (bucket != NULL) {
        return _ol_reallocate_bucket(db, bucket, value, vsize, ct, ctsize);
    }

    /* Looks like we don't have an old hash */
    ol_bucket *new_bucket = calloc(1, sizeof(ol_bucket));
    if (new_bucket == NULL)
        return 1;

    /* copy _key into new bucket */
    if (strncpy(new_bucket->key, _key, _klen) != new_bucket->key) {
        free(new_bucket);
        return 2;
    }

    new_bucket->klen = _klen;
    new_bucket->ctype_size = ctsize;

    char *ct_real = calloc(1, ctsize+1);
    if (strncpy(ct_real, ct, ctsize) != ct_real) {
        /* Free allocated memory since we're not going to use them */
        free(ct_real);
        free(new_bucket);
        return 7;
    }
    ct_real[ctsize] = '\0';
    new_bucket->content_type = ct_real;

    int bucket_max = ol_ht_bucket_max(db->cur_ht_size);
    /* TODO: rehash this shit at 80% */
    if (db->rcrd_cnt > 0 && db->rcrd_cnt == bucket_max) {
        debug("Record count is now %i; growing hash table.", db->rcrd_cnt);
        ret = _ol_grow_and_rehash_db(db);
        if (ret > 0) {
            ol_log_msg(LOG_ERR, "Problem rehashing DB. Error code: %i", ret);
            free(ct_real);
            free(new_bucket);
            return 4;
        }
    }

    new_bucket->original_size = vsize;

    /* Compute the new position of the data in the values file: */
    const size_t new_offset = db->val_size;

    if (db->state != OL_S_STARTUP) {
        unsigned char *new_data_ptr = NULL;

        if (db->is_enabled(OL_F_LZ4, &db->feature_set)) {
            /* Compress using LZ4 if enabled */
            int maxoutsize = LZ4_compressBound(vsize);
            _ol_ensure_values_file_size(db, maxoutsize);
            new_data_ptr = db->values + db->val_size;
            memset(new_data_ptr, '\0', maxoutsize);

            /* All these fucking casts */
            size_t cmsize = (size_t)LZ4_compress((char*)value, (char*)new_data_ptr,
                                                 (int)vsize);
            if (cmsize == 0) {
                /* Free allocated data */
                free(new_bucket->content_type);
                free(new_bucket);
                return 1;
            }

            new_bucket->data_size = cmsize;
            new_bucket->data_offset = new_offset;
        } else {
            new_bucket->data_size = vsize;
            _ol_ensure_values_file_size(db, new_bucket->data_size);
            new_data_ptr = db->values + db->val_size;
            memset(new_data_ptr, '\0', new_bucket->data_size);

            if (memcpy(new_data_ptr, value, vsize) != new_data_ptr) {
                /* Free allocated memory since we're not going to use them */
                free(new_bucket->content_type);
                free(new_bucket);
                return 3;
            }
            new_bucket->data_offset = new_offset;
        }
    } else {
        /* We still need to set the data size, but not the actual data. */
        if (db->is_enabled(OL_F_LZ4, &db->feature_set)) {
            /* Since LZ4_compressBound only provides the worst case scenario
             * and not what the data actually compressed to (we're replaying
             * the AOL file, remember?) we have to compress it again and grab
             * the amount of bytes processed.
             * TODO: This is dumb. Make a function that just sets the bucket size.
             * This new mythical function should also handle setting the data_offset
             * of the bucket.
             */
            int maxoutsize = LZ4_compressBound(vsize);
            char tmp_data[maxoutsize];
            /* Don't need to memset tmp_data because I don't care about it. */

            size_t cmsize = (size_t)LZ4_compress((char *)value, (char *)tmp_data,
                                                 (int)vsize);
            new_bucket->data_size = cmsize;
        } else {
            new_bucket->data_size = vsize;
        }
        new_bucket->data_offset = db->val_size;
    }

    /* Remember to increment the tracked data size of the DB. */
    db->val_size += new_bucket->data_size;

    uint32_t hash;
    MurmurHash3_x86_32(_key, _klen, DEVILS_SEED, &hash);
    ret = _ol_set_bucket(db, new_bucket, hash);

    if(ret > 0)
        ol_log_msg(LOG_ERR, "Problem inserting item: Error code: %i", ret);

    if(db->is_enabled(OL_F_APPENDONLY, &db->feature_set) &&
            db->state != OL_S_STARTUP) {
        ol_aol_write_cmd(db, "JAR", new_bucket);
    }

    return 0;

error:
    return 1;
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
    char _key[KEY_SIZE] = {'\0'};
    size_t _klen = 0;
    ol_bucket *bucket = ol_get_bucket(db, key, klen, &_key, &_klen);
    check_warn(_klen > 0, "Key length of zero not allowed.");

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

error:
    return 1;
}

int ol_scoop(ol_database *db, const char *key, size_t klen) {
    /* you know... like scoop some data from the jar and eat it? All gone. */
    uint32_t hash;
    char _key[KEY_SIZE] = {'\0'};
    _ol_trunc(key, klen, _key);
    size_t _klen = strnlen(_key, KEY_SIZE);
    check_warn(_klen > 0, "Key length cannot be zero.");


    MurmurHash3_x86_32(_key, _klen, DEVILS_SEED, &hash);
    int index = _ol_calc_idx(db->cur_ht_size, hash);

    if (index < 0) {
        return 1;
    }

    ol_bucket *to_free = NULL;
    int return_level = 2;
    if (db->hashes[index] != NULL) {
        size_t larger_key = 0;
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

            to_free = bucket;
            return_level = 0;
        } else { /* Keys weren't the same, traverse the bucket LL */
            do {
                ol_bucket *last = bucket;
                bucket = bucket->next;
                larger_key = bucket->klen > klen ? bucket->klen : klen;
                if (strncmp(bucket->key, _key, larger_key) == 0) {
                    if (bucket->next != NULL)
                        last->next = bucket->next;
                    else
                        last->next = NULL;
                    to_free = bucket;
                    return_level = 0;
                    break;
                }
            } while (bucket->next != NULL);
        }
    }

    if (to_free != NULL) {
        if (db->is_enabled(OL_F_SPLAYTREE, &db->feature_set)) {
            ols_delete(db->tree, to_free->node);
            to_free->node = NULL;
        }
        unsigned char *data_ptr = db->values + to_free->data_offset;
        const size_t data_size = to_free->data_size;
        if (data_size != 0)
            memset(data_ptr, '\0', data_size);
        _ol_free_bucket(&to_free);
        db->rcrd_cnt -= 1;
    }
    return return_level;
error:
    return 1;
}

char *ol_content_type(ol_database *db, const char *key, size_t klen) {
    char _key[KEY_SIZE] = {'\0'};
    size_t _klen = 0;
    ol_bucket *bucket = ol_get_bucket(db, key, klen, &_key, &_klen);
    check_warn(_klen > 0, "Key length of zero not allowed.");

    if (bucket != NULL) {
        if (!_has_bucket_expired(bucket)) {
            return bucket->content_type;
        } else {
            /* It's dead, get rid of it. */
            check(ol_scoop(db, key, klen) == 0, "Could not delete a bucket!")
        }
    }

error:
    return NULL;
}

struct tm *ol_expiration_time(ol_database *db, const char *key, size_t klen) {
    char _key[KEY_SIZE] = {'\0'};
    size_t _klen = 0;
    ol_bucket *bucket = ol_get_bucket(db, key, klen, &_key, &_klen);
    check_warn(_klen > 0, "Key length of zero not allowed.");

    if (bucket != NULL && bucket->expiration != NULL) {
        if (!_has_bucket_expired(bucket)) {
            return bucket->expiration;
        } else {
            /* It's dead, get rid of it. */
            check(ol_scoop(db, key, klen) == 0, "Could not delete a bucket!")
        }
    }

error:
    return NULL;
}

int ol_uptime(ol_database *db) {
    /* Make uptime */
    time_t now;
    double diff;
    time(&now);
    diff = difftime(now, db->meta->created);
    return diff;
}
