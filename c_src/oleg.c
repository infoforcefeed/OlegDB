/* Main oleg source code. See corresponding header file for more info. */
#include <assert.h>
#include <fcntl.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
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
#include "transaction.h"

inline int ol_ht_bucket_max(size_t ht_size) {
    return (ht_size/sizeof(ol_bucket *));
}

void _ol_get_file_name(const ol_database *db, const char *p, char *o_file) {
    sprintf(o_file, "%s/%s.%s", db->path, db->name, p);
}

void _ol_enable(int feature, int *feature_set) {
    *feature_set |= feature;
}

void _ol_disable(int feature, int *feature_set) {
    *feature_set &= ~feature;
}

bool _ol_is_enabled(const int feature, const int *feature_set) {
    return (*feature_set & feature) == feature;
}

ol_database *ol_open(const char *path, const char *name, int features){
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
    new_db->cur_transactions = NULL;

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

    check(_ol_open_values(new_db), "Could not open values file.");

    new_db->feature_set = features;
    new_db->state = OL_S_STARTUP;
    /* Allocate a splay tree if we're into that */
    if (new_db->is_enabled(OL_F_SPLAYTREE, &new_db->feature_set)) {
        ols_init(&new_db->tree);
    }

    /* We figure out the filename now incase someone flips the aol_init bit
     * later.
     */
    new_db->aol_file = calloc(1, AOL_FILENAME_ALLOC);
    check_mem(new_db->aol_file);
    new_db->get_db_file_name(new_db, AOL_FILENAME, new_db->aol_file);

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
        flock(fileno(db->aolfd), LOCK_UN);
        fclose(db->aolfd);
        debug("Files flushed to disk");
    }

    /* Sync and close values file. */
    if (db->val_size > 0) {
        msync(db->values, db->val_size, MS_SYNC);
        _ol_close_values(db);
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
        made = mktime(bucket->expiration);
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

    if (*_klen == 0)
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
            *data = calloc(1, bucket->original_size);
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
                unsigned char *ret = memcpy(*data, data_ptr, bucket->original_size);
                check(ret == *data, "Could not copy data into output data param.");
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

int ol_jar(ol_database *db, const char *key, size_t klen,
           unsigned char *value, size_t vsize) {

    /* Is disabled_tx enabled? lksjdlkfpfpfllfplflpf */
    if(db->is_enabled(OL_F_DISABLE_TX, &db->feature_set)) {
        /* Fake a transaction: */
        ol_transaction stack_tx = {
            .tx_id = 0,
            .parent_db = NULL,
            .transaction_db = db
        };
        return olt_jar(&stack_tx, key, klen, value, vsize);
    }

    ol_transaction *tx = olt_begin(db);
    int jar_ret = 10;
    check(tx != NULL, "Could not begin implicit transaction.");

    jar_ret = olt_jar(tx, key, klen, value, vsize);
    check(jar_ret == 0, "Could not jar value. Aborting.");

    check(olt_commit(tx) == 0, "Could not commit transaction.");

    return jar_ret;

error:
    if (tx != NULL && jar_ret != 10)
        olt_abort(tx);

    return jar_ret;
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

int ol_cas(ol_database *db, const char *key, const size_t klen,
                            unsigned char *value, size_t vsize,
                            const unsigned char *ovalue, const size_t ovsize) {
    char _key[KEY_SIZE] = {'\0'};
    size_t _klen = 0;

    ol_bucket *bucket = ol_get_bucket(db, key, klen, &_key, &_klen);
    check_warn(_klen > 0, "Key length of zero not allowed.");

    if (bucket == NULL)
        return 1;

    /* Quick fail if the two sizes don't match */
    if (bucket->original_size != ovsize)
        return 1;

    /* ATOMIC, GOOOO! */
    const unsigned char *data_ptr = db->values + bucket->data_offset;
    if (db->is_enabled(OL_F_LZ4, &db->feature_set)) {
        char decompressed[bucket->original_size];
        memset(decompressed, '\0', bucket->original_size);

        int processed = 0;
        processed = LZ4_decompress_fast((char *)data_ptr,
                                        (char *)decompressed,
                                        bucket->original_size);
        check(processed == bucket->data_size, "Could not decompress data.");

        if (memcmp(decompressed, ovalue, ovsize) == 0)
            return ol_jar(db, key, klen, value, vsize);
    } else {
        if (memcmp(data_ptr, ovalue, ovsize) == 0)
            return ol_jar(db, key, klen, value, vsize);
    }

    return 1;

error:
    return 1;
}

int ol_squish(ol_database *db) {
    int fflush_turned_off = 0;
    if(db->is_enabled(OL_F_APPENDONLY, &db->feature_set)) {
        /* Turn off fflush for the time being. We'll do it once at the end. */
        if (db->is_enabled(OL_F_AOL_FFLUSH, &db->feature_set)) {
            db->disable(OL_F_AOL_FFLUSH, &db->feature_set);
            fflush_turned_off = 1;
        }

        /* AOL is enabled. Create a new aol file that we'll be using. */
        fflush(db->aolfd);
        fclose(db->aolfd);

        /* Create a new file which we'll move into the old ones place later */
        db->get_db_file_name(db, "aol.new", db->aol_file);

        /* Get a new file descriptor */
        db->aolfd = fopen(db->aol_file, AOL_FILEMODE);
    }

    /* Iterate through the hash table instead of using the tree just
     * so you can use this in case the tree isn't enabled. */
    const int iterations = ol_ht_bucket_max(db->cur_ht_size);

    int i = 0;
    for (; i < iterations; i++) {
        if (db->hashes[i] != NULL) {
            /* Found a bucket. */
            ol_bucket *ptr, *next;
            /* Start traversing the linked list of collisions, starting with
             * the bucket we found. */
            for (ptr = db->hashes[i]; NULL != ptr; ptr = next) {
                if (!_has_bucket_expired(ptr)) {
                    /* Bucket hasn't been deleted or expired. */
                    if(db->is_enabled(OL_F_APPENDONLY, &db->feature_set)) {
                        /* AOL is enabled. Write it to the new AOL file. */
                        ol_aol_write_cmd(db, "JAR", ptr);

                        /* See if theres an expiration date we care about: */
                        if (ptr->expiration != NULL) {
                            ol_aol_write_cmd(db, "SPOIL", ptr);
                        }
                    }
                }
                /* Get the next bucket in the collision chain. */
                next = ptr->next;
            }
        }
    }

    if(db->is_enabled(OL_F_APPENDONLY, &db->feature_set)) {
        /* Turn off fflush for the time being. We'll do it once at the end. */
        if (fflush_turned_off) {
            db->enable(OL_F_AOL_FFLUSH, &db->feature_set);
        }
        /* Make sure all of the new stuff is written */
        fflush(db->aolfd);
        fclose(db->aolfd);

        char new_filename[AOL_FILENAME_ALLOC] = {0};
        /* Set the old filename. */
        db->get_db_file_name(db, "aol.new", new_filename);
        db->get_db_file_name(db, AOL_FILENAME, db->aol_file);
        /* Rename the .aol.new file to just be .aol */
        check(rename(new_filename, db->aol_file) == 0, "Could not rename new AOL to old AOL.");

        /* Get a new file descriptor */
        db->aolfd = fopen(db->aol_file, AOL_FILEMODE);
    }

    return 1;

error:
    return 0;
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
