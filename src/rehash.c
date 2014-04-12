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
#include <stdlib.h>
#include "oleg.h"
#include "utils.h"
#include "errhandle.h"
#include "logging.h"

static inline void _ol_rehash_insert_bucket(
        ol_bucket **tmp_hashes, const size_t to_alloc, ol_bucket *bucket) {
    int new_index;
    new_index = _ol_calc_idx(to_alloc, bucket->hash);
    if (tmp_hashes[new_index] != NULL) {
        /* Enforce that this is the last bucket, KILL THE ORPHANS */
        ol_bucket *last_bucket = _ol_get_last_bucket_in_slot(
                tmp_hashes[new_index]);
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

    ol_bucket *orphan_chain = NULL;
    int iterations = ol_ht_bucket_max(db->cur_ht_size);
    for (i = 0; i < iterations; i++) {
        bucket = db->hashes[i];
        if (bucket != NULL) {
            if (bucket->next != NULL) {
                if (orphan_chain == NULL) {
                    /* Initialize the orphan collection */
                    orphan_chain = bucket->next;
                } else {
                    ol_bucket *last_orphan =
                        _ol_get_last_bucket_in_slot(orphan_chain);
                    last_orphan->next = bucket->next;
                }
                bucket->next = NULL;
            }
            /* Rehash the bucket itself. */
            _ol_rehash_insert_bucket(tmp_hashes, to_alloc, bucket);
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
