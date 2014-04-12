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
#include "logging.h"

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

void _ol_free_bucket(ol_bucket *ptr) {
    if (ptr->expiration != NULL)
        free(ptr->expiration);
    free(ptr->content_type);
    free(ptr->data_ptr);
    free(ptr);
    ptr = NULL;
}

int _ol_calc_idx(const size_t ht_size, const uint32_t hash) {
    int index;
    /* Powers of two, baby! */
    index = hash & (ol_ht_bucket_max(ht_size) - 1);
    return index;
}

