/* Common utility functions. */
#include <stdlib.h>
#include "oleg.h"
#include "utils.h"
#include "errhandle.h"
#include "logging.h"
#include "lz4.h"

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

void _ol_free_bucket(ol_bucket **ptr) {
    free((*ptr)->expiration);
    free((*ptr));
    *ptr = NULL;
}

int _ol_calc_idx(const size_t ht_size, const uint32_t hash) {
    int index;
    /* Powers of two, baby! */
    index = hash & (ol_ht_bucket_max(ht_size) - 1);
    return index;
}

const int _ol_compute_padded_size(const int size) {
    return (size + 4095) & ~4095;
}

int _ol_get_value_from_bucket(const ol_database *db, const ol_bucket *bucket,
        unsigned char **data, size_t *dsize) {
    check(bucket != NULL, "Cannot retrieve value from NULL bucket.");
    /* Allocate memory to store memcpy'd data into. */
    *data = calloc(1, bucket->original_size);
    check(*data != NULL, "Could not allocate memory for compressed data.");

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
        check(processed == bucket->data_size, "Could not decompress data.");
    } else {
        /* We know data isn't NULL by this point. */
        unsigned char *ret = memcpy(*data, data_ptr, bucket->original_size);
        check(ret == *data, "Could not copy data into output data param.");
    }

    return 0;

error:
    return 1;
}
