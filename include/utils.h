#pragma once
#include <math.h>
#include <sys/stat.h>
#include <sys/types.h>
/* Utility functions that don't have a home. */

#define intlen(value) (value == 0 ? 1 : (int)(floor(log10(value)))+1)

/* Internal function used to get the last bucket on the
 * linked list of ol_bucket objects in a db slot. */
ol_bucket *_ol_get_last_bucket_in_slot(ol_bucket *bucket);

/* Internal function used to free a bucket's memory. */
void _ol_free_bucket(ol_bucket **ptr);

/* Internal function used to find a buckets position in the DB */
int _ol_calc_idx(const size_t ht_size, const uint32_t hash);

/* Computes the padded size of a value on disk. */
const int _ol_compute_padded_size(const int size);

/* Reallocates a bucket. I think. I forget. Don't use this for anything. */
int _ol_reallocate_bucket(ol_database *db, ol_bucket *bucket,
                          const unsigned char *value, size_t vsize);

/* Sets a bucket and does some other useful things. I don't remember what they */
/* are at the moment. */
int _ol_set_bucket(ol_database *db, ol_bucket *bucket, uint32_t hash);
