/* Functions needed to rehash the main hash table. */
#include <fcntl.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include "file.h"
#include "oleg.h"
#include "rehash.h"
#include "utils.h"
#include "errhandle.h"
#include "logging.h"
#include "stack.h"

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

    /* Flush any changes we have to disk */
    msync(db->hashes, db->cur_ht_size, MS_SYNC);
    /* Reallocate new memory size */
    int new_hashes_fd = { 0 };
    char new_hashes_filename[DB_NAME_SIZE] = { 0 };

    /* Create the temporary hash table */
    db->get_db_file_name(db, "ht.tmp", new_hashes_filename);
    new_hashes_fd = open(new_hashes_filename, O_RDWR | O_CREAT, S_IWUSR | S_IRUSR);
    check(new_hashes_fd > 0, "Could not open file.");

    /* Make sure it's big enough */
    if (_ol_get_file_size(new_hashes_filename) == 0)
         check(ftruncate(new_hashes_fd, to_alloc) != -1, "Could not truncate file for new hashes.");

    tmp_hashes = _ol_mmap(to_alloc, new_hashes_fd);
    check(tmp_hashes != NULL, "Could not create not temporary rehash table.");
    close(new_hashes_fd);

    /* Track orphans as we go through */
    ol_stack *orphans = NULL;
    orphans = malloc(sizeof(ol_stack));
    check_mem(orphans);
    orphans->next = NULL;
    orphans->data = NULL;
    int orphans_found = 0;

    int iterations = ol_ht_bucket_max(db->cur_ht_size);
    for (i = 0; i < iterations; i++) {
        bucket = db->hashes[i];
        if (bucket != NULL) {
            if (bucket->next != NULL) {
                ol_bucket *tmp_bucket = bucket;
                do {
                    spush(&orphans, tmp_bucket->next);

                    ol_bucket *next = tmp_bucket->next;
                    tmp_bucket->next = NULL;
                    tmp_bucket = next;

                    orphans_found++;
                } while (tmp_bucket->next != NULL);
            }
            /* Rehash the bucket itself. */
            _ol_rehash_insert_bucket(tmp_hashes, to_alloc, bucket);
        }
    }

    /* Take care of our orphans */
    ol_log_msg(LOG_INFO, "Have %i orphans to take care of.", orphans_found);
    do {
        ol_bucket *rebucket = spop(&orphans);
        _ol_rehash_insert_bucket(tmp_hashes, to_alloc, rebucket);

        orphans_found--;
    } while (orphans->next != NULL);
    ol_log_msg(LOG_INFO, "We now have %i orphans not accounted for.", orphans_found);

    free(orphans);
    munmap(db->hashes, db->cur_ht_size);

    char hashes_filename[DB_NAME_SIZE] = { 0 };
    db->get_db_file_name(db, HASHES_FILENAME, hashes_filename);
    /* Delete old hash table */
    unlink(hashes_filename);
    /* Rename temp table to new table. */
    rename(new_hashes_filename, hashes_filename);

    /* Make the pointers point */
    db->hashes = tmp_hashes;
    db->cur_ht_size = to_alloc;
    debug("Current hash table size is now: %zu bytes.", to_alloc);
    return 0;

error:
    if (tmp_hashes != NULL)
        free(tmp_hashes);
    return -1;
}
