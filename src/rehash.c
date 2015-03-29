/* Functions needed to rehash the main hash table. */
#include <stdlib.h>
#define F_MEMORY_DEBUG
#include "forge.h"
#include "oleg.h"
#include "rehash.h"
#include "utils.h"
#include "errhandle.h"
#include "logging.h"
#include "vector.h"
#include "murmur3.h"

static inline void _ol_rehash_insert_bucket(
        ol_bucket **tmp_hashes,
        const size_t to_alloc,
        ol_bucket *bucket) {
    unsigned int new_index;

    uint32_t hash;
    MurmurHash3_x86_32(bucket->key, bucket->klen, DEVILS_SEED, &hash);

    new_index = _ol_calc_idx(to_alloc, hash);
    if (tmp_hashes[new_index] != NULL) {
        /* Enforce that this is the last bucket, KILL THE ORPHANS */
        ol_bucket *last_bucket = _ol_get_last_bucket_in_slot(tmp_hashes[new_index]);
        last_bucket->next = bucket;
    } else {
        tmp_hashes[new_index] = bucket;
    }
}

int _ol_grow_and_rehash_db(ol_database *db) {
    int i;
    ol_bucket *bucket;
    ol_bucket **tmp_hashes = NULL;

    size_t to_alloc = db->cur_ht_size * 2;
    debug("Growing DB to %zu bytes.", to_alloc);
    tmp_hashes = calloc(1, to_alloc);
    check_mem(tmp_hashes);

    vector *orphans = NULL;
    orphans = vector_new(sizeof(ol_bucket *), 256);
    check_mem(orphans);

    int iterations = ol_ht_bucket_max(db->cur_ht_size);
    for (i = 0; i < iterations; i++) {
        bucket = db->hashes[i];
        if (bucket != NULL) {
            if (bucket->next != NULL) {
                ol_bucket *tmp_bucket = bucket;
                do {
                    vector_append_ptr(orphans, tmp_bucket->next);
                    ol_bucket *next = tmp_bucket->next;
                    tmp_bucket->next = NULL;
                    tmp_bucket = next;
                } while (tmp_bucket->next != NULL);
            }
            /* Rehash the bucket itself. */
            _ol_rehash_insert_bucket(tmp_hashes, to_alloc, bucket);
        }
    }

    /* Take care of our orphans */
    ol_log_msg(LOG_INFO, "Have %i orphans to take care of.", orphans->count);
    unsigned int j;
    for (j = 0; j < orphans->count; j++) {
        ol_bucket **rebucket = vector_get_danger(orphans, j);
        _ol_rehash_insert_bucket(tmp_hashes, to_alloc, (*rebucket));
    }

    vector_free(orphans);
    free(db->hashes);

    db->hashes = tmp_hashes;
    db->cur_ht_size = to_alloc;
    debug("Current hash table size is now: %zu bytes.", to_alloc);
    return 0;

error:
    if (tmp_hashes != NULL)
        free(tmp_hashes);
    return -1;
}
