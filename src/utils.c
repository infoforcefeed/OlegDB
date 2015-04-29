/* Common utility functions. */
#include <stdlib.h>
#include "oleg.h"
#include "utils.h"
#include "errhandle.h"
#include "logging.h"
#include "errhandle.h"
#include "lz4.h"
#include "file.h"
#include "aol.h"

ol_bucket *_ol_get_last_bucket_in_slot(ol_bucket *bucket) {
    ol_bucket *tmp_bucket = bucket;
    int depth = 0;
    while (tmp_bucket->next != NULL) {
        tmp_bucket = tmp_bucket->next;
        depth++;
        if (depth > 1000)
            ol_log_msg(LOG_WARN, "Depth of bucket stack is crazy, help! It's at %i", depth);
    }
    return tmp_bucket;
}

int _has_bucket_expired(const ol_bucket *bucket) {
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
        {   /* Double-wrap the braces in case the first ones are optimized out. */
            return 0;
        }
    }
    return 1;
}

void _ol_free_bucket(ol_bucket **ptr) {
    free((*ptr)->expiration);
    free((*ptr)->key);
    free((*ptr));
    *ptr = NULL;
}

inline unsigned int _ol_calc_idx(const size_t ht_size, const uint32_t hash) {
    unsigned int index;
    /* Powers of two, baby! */
    index = hash & (ol_ht_bucket_max(ht_size) - 1);
    return index;
}

const int _ol_compute_padded_size(const int size) {
    return (size + 4095) & ~4095;
}

int _ol_reallocate_bucket(ol_database *db, ol_bucket *bucket,
                          const unsigned char *value, size_t vsize) {
    debug("Reallocating bucket.");

    unsigned char *old_data_ptr = db->values + bucket->data_offset;
    /* Clear out the old data in the file. */
    if (db->state != OL_S_STARTUP && bucket->data_size > 0)
        memset(old_data_ptr, '\0', bucket->data_size);
    /* Compute the new position of the data in the values file: */
    size_t new_offset = db->val_size;
    unsigned char *new_data_ptr = NULL;

    /* Compress using LZ4 if enabled */
    size_t cmsize = 0;
    int extended_value_area = 0;
    if (db->is_enabled(OL_F_LZ4, &db->feature_set)) {
        int maxoutsize = LZ4_compressBound(vsize);
        if (maxoutsize <= bucket->data_size) {
            /* We don't need to put this value at the end of the file if the
             * new value is small enough. */
            new_data_ptr = old_data_ptr;
            new_offset = bucket->data_offset;
        } else {
            _ol_ensure_values_file_size(db, maxoutsize);
            extended_value_area = 1;
            new_data_ptr = db->values + db->val_size;
        }

        if (db->state != OL_S_STARTUP) {
            cmsize = (size_t)LZ4_compress((char*)value, (char*)new_data_ptr,
                                          (int)vsize);
        } else {
            /* We're starting up, so we don't want to actually write to the
             * values file. We just want the size and stuff.
             */
            int maxoutsize = LZ4_compressBound(vsize);
            char tmp_data[maxoutsize];
            cmsize = (size_t)LZ4_compress((char *)value, (char *)tmp_data,
                                                 (int)vsize);
        }
    } else {
        if (vsize <= bucket->data_size) {
            /* We don't need to put this value at the end of the file if the
             * new value is small enough. */
            new_data_ptr = old_data_ptr;
            new_offset = bucket->data_offset;
        } else {
            _ol_ensure_values_file_size(db, vsize);
            extended_value_area = 1;
            new_data_ptr = db->values + db->val_size;
        }
        if (db->state != OL_S_STARTUP) {
            /* Like above, avoid writing to the values file on startup. */
            if (memcpy(new_data_ptr, value, vsize) != new_data_ptr)
                return 4;
        }
    }

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
    if (extended_value_area)
        db->val_size += bucket->data_size;

    if(db->is_enabled(OL_F_APPENDONLY, &db->feature_set) && db->state != OL_S_STARTUP) {
        ol_aol_write_cmd(db, "JAR", bucket);
    }

    return 0;
}

int _ol_set_bucket_no_incr(ol_database *db, ol_bucket *bucket, uint32_t hash) {
    /* TODO: error codes? */
    unsigned int index = _ol_calc_idx(db->cur_ht_size, hash);
    if (db->hashes[index] != NULL) {
        db->meta->key_collisions++;
        ol_bucket *tmp_bucket = db->hashes[index];
        tmp_bucket = _ol_get_last_bucket_in_slot(tmp_bucket);
        tmp_bucket->next = bucket;
    } else {
        db->hashes[index] = bucket;
    }

    if (db->is_enabled(OL_F_SPLAYTREE, &db->feature_set)) {
        /* Put the bucket into the tree */
        ol_splay_tree_node *node = NULL;
        node = ols_insert(db->tree, bucket->key, bucket->klen, bucket);
        /* Make sure the bucket can reference the node. */
        bucket->node = node;
    }

    return 0;
}

int _ol_set_bucket(ol_database *db, ol_bucket *bucket, uint32_t hash) {
    _ol_set_bucket_no_incr(db, bucket, hash);
    db->rcrd_cnt++;
    return 0;
}

inline void _ol_trunc(const char *key, size_t klen, char *out) {
    /* Silently truncate because #yolo */
    size_t real_key_len = klen > KEY_SIZE ? KEY_SIZE : klen;
    strncpy(out, key, real_key_len);
    out[real_key_len] = '\0';
}

int _ol_get_value_from_bucket(const ol_database *db, const ol_bucket *bucket,
        unsigned char **data, size_t *dsize) {
    check(bucket != NULL, "Cannot retrieve value from NULL bucket.");
    /* Allocate memory to store memcpy'd data into. */
    *data = malloc(bucket->original_size + 1);
    check_mem(*data);
    (*data)[bucket->original_size] = '\0';

    if (dsize != NULL) {
        /* "memcpy never fails!" */
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
        check(processed == bucket->data_size, "Could not decompress data. Key: %s", bucket->key);
    } else {
        /* We know data isn't NULL by this point. */
        unsigned char *ret = memcpy(*data, data_ptr, bucket->original_size);
        check(ret == *data, "Could not copy data into output data param.");
    }

    return 0;

error:
    return 1;
}

static const char lookup[] = "0123456789";
inline void sizet_to_a(const size_t src, const size_t dest_len, char dest[static MAX_SIZE_T_STR_SIZE]) {
    size_t copy = src;
    dest[dest_len] = '\0';
    uint i;
    for (i = dest_len; i > 0; i--) {
        dest[(i - 1)] = lookup[copy % 10];
        copy /= 10;
    }
}
