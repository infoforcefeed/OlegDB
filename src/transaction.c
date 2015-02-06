#define __STDC_FORMAT_MACROS
#include <fcntl.h>
#include <inttypes.h>
#include <stdlib.h>
#include <unistd.h>

#include "oleg.h"
#include "logging.h"
#include "errhandle.h"
#include "transaction.h"
#include "utils.h"
#include "tree.h"
#include "file.h"
#include "lz4.h"
#include "murmur3.h"
#include "rehash.h"
#include "aol.h"

/* Where we start our transactions from. */
transaction_id global_transaction_id = 1;

static inline char *tx_to_str(const transaction_id tx_id) {
    char *_key = NULL;
    _key = malloc(sizeof(char) * KEY_SIZE);
    check_mem(_key);
    snprintf(_key, KEY_SIZE, "%" PRIu64, tx_id);

    return _key;

error:
    return NULL;
}

ol_splay_tree_node *ols_find_tx_id(ol_splay_tree *tree, const transaction_id key) {
    const size_t klen = intlen(key);
    char *_key = tx_to_str(key);

    ol_splay_tree_node *node = ols_find(tree, _key, klen);
    free(_key);

    return node;
}

static void _tx_directory(char *new_path, ol_database *db) {
    snprintf(new_path, PATH_LENGTH, "%s/%s", db->path, "tx");
}

ol_transaction *olt_begin(ol_database *db) {
    char *name = NULL;
    ol_transaction *new_transaction = NULL;
    check(db != NULL, "No database specified in transaction begin.");
    check(db->cur_transactions != NULL, "No transaction tree.");

    /* We initialize on the stack because tx_id is a const parameter. */
    ol_transaction stack_tx = {
        .tx_id = global_transaction_id,
        .dirty = 0,
        .parent_db = db,
        .transaction_db = NULL
    };

    /* Setup a ".../tx/" directory for our transaction databases. */
    char new_path[PATH_LENGTH] = {0};
    _tx_directory(new_path, db);

    /* Convert our integer tx_id into a string */
    name = tx_to_str(stack_tx.tx_id);
    check(name != NULL, "Could not convert tx_id to str.");

    /* Make sure implicit transactions is turned OFF, because otherwise we'll
     * get endless recursion. Wooo! */
    ol_feature_flags flags = OL_F_APPENDONLY | OL_F_SPLAYTREE | OL_F_LZ4 | OL_F_DISABLE_TX;
    stack_tx.transaction_db = ol_open(new_path, name, flags);
    stack_tx.transaction_db->rcrd_cnt = stack_tx.parent_db->rcrd_cnt;
    check(stack_tx.transaction_db != NULL, "Could not open transaction database.");

    /* Copy the stack thing into the heap thing. */
    new_transaction = malloc(sizeof(ol_transaction));
    check_mem(new_transaction);
    memcpy(new_transaction, &stack_tx, sizeof(ol_transaction));
    free(name);

    ols_insert(db->cur_transactions,
        stack_tx.transaction_db->name,
        strnlen(stack_tx.transaction_db->name, DB_NAME_SIZE),
        new_transaction
    );

    global_transaction_id++;

    return new_transaction;

error:
    free(new_transaction);
    free(name);
    return NULL;
}

static int _olt_cleanup(ol_transaction *tx, char *values_filename, char *tx_aol_filename) {
    /* Yeah, come at me. */
    const int ret = ol_close(tx->transaction_db);
    debug("Unlinking values file for transaction, %s", values_filename);
    unlink(values_filename);

    debug(LOG_WARN, "Unlinking aol file for transaction, %s", tx_aol_filename);
    unlink(tx_aol_filename);

    free(tx);

    return ret;
}

int olt_commit(ol_transaction *tx) {
    /* So at this point we should have a series of operations stored up and
     * successful in our tx->transaction_db. At this point we need to replay
     * them all back onto the parent_db.
     */
    check(tx->parent_db != NULL, "No parent database.");
    check(tx->parent_db->cur_transactions != NULL, "No transaction tree.");

    if (!tx->dirty)
        return olt_abort(tx);

    char tx_aol_filename[AOL_FILENAME_ALLOC] = {0};
    tx->transaction_db->get_db_file_name(tx->transaction_db, AOL_FILENAME, tx_aol_filename);

    char values_filename[DB_NAME_SIZE] = {0};
    tx->transaction_db->get_db_file_name(tx->transaction_db, VALUES_FILENAME, values_filename);

    /* Don't squish or compact or anything here because it'll remove stuff
     * that we don't want removed from the log file, like SCOOP commands.
     */

    /* Make sure everything is written: */
    ol_sync(tx->transaction_db);
    /* Throw fflush in here because fuck Linux */
    fflush(tx->transaction_db->aolfd);

    tx->parent_db->state = OL_S_COMMITTING;
    ol_aol_restore_from_file(tx->parent_db, tx_aol_filename, tx->transaction_db->values);
    tx->parent_db->state = OL_S_AOKAY;

    return _olt_cleanup(tx, values_filename, tx_aol_filename);

error:
    return 1;
}

int olt_abort(ol_transaction *tx) {
    check(tx->parent_db != NULL, "No parent database.");
    check(tx->parent_db->cur_transactions != NULL, "No transaction tree.");

    char tx_aol_filename[AOL_FILENAME_ALLOC] = {0};
    tx->transaction_db->get_db_file_name(tx->transaction_db, AOL_FILENAME, tx_aol_filename);

    char values_filename[DB_NAME_SIZE] = {0};
    tx->transaction_db->get_db_file_name(tx->transaction_db, VALUES_FILENAME, values_filename);

    return _olt_cleanup(tx, values_filename, tx_aol_filename);

error:
    return 1;
}

int olt_lock_bucket(const ol_transaction *tx, ol_bucket *bucket) {
    check(tx, "Cannot lock a bucket with a NULL transaction");
    check(bucket, "Cannot lock a NULL bucket");


    /* If we don't have a parent DB there are no transactions we are
     * competing against. Just succeed.
     */
    if (!tx->parent_db)
        return 0;

    check(tx->parent_db->cur_transactions, "No transaction tree.");
    if (bucket->tx_id != 0 && bucket->tx_id != tx->tx_id) {
        ol_splay_tree_node *node = ols_find_tx_id(
                tx->parent_db->cur_transactions,
                tx->tx_id
        );

        if (node) {
            return 2;
        }
    }

    bucket->tx_id = tx->tx_id;

    return 0;

error:
    return 1;
}

int olt_unjar(ol_transaction *tx, const char *key, size_t klen, unsigned char **data, size_t *dsize) {
    char _key[KEY_SIZE] = {'\0'};
    size_t _klen = 0;
    ol_database *operating_db = NULL;
    ol_bucket *bucket = ol_get_bucket(tx->transaction_db, key, klen, &_key, &_klen);
    check_warn(_klen > 0, "Key length of zero not allowed.");

    /* Fall through to the parent db: */
    if (bucket == NULL) {
        bucket = ol_get_bucket(tx->parent_db, key, klen, &_key, &_klen);
        /* This is getting messy... */
        if (bucket != NULL) {
            operating_db = tx->parent_db;
            check_warn(olt_lock_bucket(tx, bucket) == 0, "Could not lock bucket.");
        }
    } else {
        operating_db = tx->transaction_db;
    }

    if (bucket != NULL) {
        if (!_has_bucket_expired(bucket)) {
            /* We don't need to fill out the data so just return 'we found the key'. */
            if (data == NULL)
                return 0;

            const int ret = _ol_get_value_from_bucket(operating_db, bucket, data, dsize);
            check(ret == 0, "Could not retrieve value from bucket.");

            /* Key found, tell somebody. */
            return 0;
        } else {
            /* It's dead, get rid of it. */
            /* NOTE: We explicitly say the transaction_db here because ITS A
             * FUCKING TRANSACTION. ACID, bro. */
            check(olt_scoop(tx, key, klen) == 0, "Scoop failed.");
        }
    }

    return 1;

error:
    return 2;
}

int olt_exists(ol_transaction *tx, const char *key, size_t klen) {
    return olt_unjar(tx, key, klen, NULL, NULL);
}

int olt_jar(ol_transaction *tx, const char *key, size_t klen, const unsigned char *value, size_t vsize) {
    int ret;
    char _key[KEY_SIZE] = {'\0'};
    size_t _klen = 0;
    ol_database *db = tx->transaction_db;

    ol_bucket *bucket = ol_get_bucket(db, key, klen, &_key, &_klen);
    check_warn(_klen > 0, "Key length of zero not allowed.");

    /* Check to see if we have an existing entry with that key */
    if (bucket != NULL) {
        check_warn(olt_lock_bucket(tx, bucket) == 0, "Could not lock bucket.");
        return _ol_reallocate_bucket(db, bucket, value, vsize);
    }

    /* Looks like we don't have an old hash */
    ol_bucket *new_bucket = calloc(1, sizeof(ol_bucket));
    if (new_bucket == NULL)
        return 1;

    /* Lock the new bucket right off the bat. */
    check_warn(olt_lock_bucket(tx, new_bucket) == 0, "Could not lock bucket.");

    /* copy _key into new bucket */
    if (strncpy(new_bucket->key, _key, _klen) != new_bucket->key) {
        free(new_bucket);
        return 2;
    }

    new_bucket->klen = _klen;
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
                free(new_bucket);
                return 1;
            }

            new_bucket->data_size = cmsize;
        } else {
            new_bucket->data_size = vsize;
            _ol_ensure_values_file_size(db, new_bucket->data_size);
            new_data_ptr = db->values + db->val_size;
            memset(new_data_ptr, '\0', new_bucket->data_size);

            if (memcpy(new_data_ptr, value, vsize) != new_data_ptr) {
                /* Free allocated memory since we're not going to use them */
                free(new_bucket);
                return 3;
            }
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
    }

    /* Set the offset of the bucket before we increment it offset globally. */
    new_bucket->data_offset = new_offset;
    /* Remember to increment the tracked data size of the DB. */
    db->val_size += new_bucket->data_size;

    int bucket_max = ol_ht_bucket_max(db->cur_ht_size);
    /* TODO: rehash this shit at 80% */
    if (db->rcrd_cnt > 0 && db->rcrd_cnt == bucket_max) {
        debug("Record count is now %i; growing hash table.", db->rcrd_cnt);
        ret = _ol_grow_and_rehash_db(db);
        if (ret > 0) {
            ol_log_msg(LOG_ERR, "Problem rehashing DB. Error code: %i", ret);
            free(new_bucket);
            return 4;
        }
    }


    uint32_t hash;
    MurmurHash3_x86_32(_key, _klen, DEVILS_SEED, &hash);
    ret = _ol_set_bucket(db, new_bucket, hash);

    if(ret > 0)
        ol_log_msg(LOG_ERR, "Problem inserting item: Error code: %i", ret);

    if(db->is_enabled(OL_F_APPENDONLY, &db->feature_set) &&
            db->state != OL_S_STARTUP) {
        ol_aol_write_cmd(db, "JAR", new_bucket);
    }

    /* Flag the transaction as dirty. */
    tx->dirty = 1;

    return 0;

error:
    return 1;
}

int olt_scoop(ol_transaction *tx, const char *key, size_t klen) {
    /* you know... like scoop some data from the jar and eat it? All gone. */
    uint32_t hash;
    char _key[KEY_SIZE] = {'\0'};
    _ol_trunc(key, klen, _key);
    size_t _klen = strnlen(_key, KEY_SIZE);
    check_warn(_klen > 0, "Key length cannot be zero.");

    MurmurHash3_x86_32(_key, _klen, DEVILS_SEED, &hash);
    ol_database *operating_db = tx->transaction_db;
    /* First attempt to calculate the index in the transaction_db */
    int index = _ol_calc_idx(tx->transaction_db->cur_ht_size, hash);
    /* If we couldn't find it in the transaction_db, look for the value in the
     * parent_db (the one we forked from) */
    if (tx->transaction_db->hashes[index] == NULL &&
            tx->parent_db != NULL) {
        index = _ol_calc_idx(tx->parent_db->cur_ht_size, hash);
        operating_db = tx->parent_db;
    }

    if (index < 0 || operating_db->hashes[index] == NULL)
        return 1;

    /* Now that we know what database we're operating on, continue
     * as usual. */
    ol_bucket *to_free = NULL;
    int return_level = 2;

    size_t larger_key = 0;
    ol_bucket *bucket = operating_db->hashes[index];
    larger_key = bucket->klen > _klen ? bucket->klen : _klen;
    if (strncmp(bucket->key, _key, larger_key) == 0) {
        /* We only ACTUALLY want to delete something if we're operating on the transaction_db */
        if (operating_db == tx->transaction_db)
            operating_db->hashes[index] = bucket->next;

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

    if (to_free != NULL) {
        /* Only delete the node from the transaction_db. */
        if (operating_db == tx->transaction_db &&
            tx->transaction_db->is_enabled(OL_F_SPLAYTREE, &tx->transaction_db->feature_set)) {
            ols_delete(tx->transaction_db->tree, to_free->node);
            to_free->node = NULL;
        }

        /* Write the SCOOP command to the log, so we can replay it later. */
        if (tx->transaction_db->state != OL_S_STARTUP) {
            ol_aol_write_cmd(tx->transaction_db, "SCOOP", bucket);
        }

        /* Again, only delete the key from the transaction_db, not the parent. */
        if (operating_db == tx->transaction_db) {
            unsigned char *data_ptr = tx->transaction_db->values + to_free->data_offset;
            const size_t data_size = to_free->data_size;
            if (data_size != 0)
                memset(data_ptr, '\0', data_size);
            _ol_free_bucket(&to_free);
            tx->transaction_db->rcrd_cnt -= 1;
        }
    }

    /* Flag the transaction as dirty. */
    tx->dirty = 1;

    return return_level;
error:
    return 1;
}

int olt_spoil(ol_transaction *tx, const char *key, size_t klen, struct tm *expiration_date) {
    char _key[KEY_SIZE] = {'\0'};
    size_t _klen = 0;

    ol_database *operating_db = tx->transaction_db;

    ol_bucket *bucket = ol_get_bucket(operating_db, key, klen, &_key, &_klen);
    check_warn(_klen > 0, "Key length of zero not allowed.");

    if (bucket == NULL) {
        /* Transaction DB doesn't have this key, but the parent does. */
        operating_db = tx->parent_db;
        bucket = ol_get_bucket(operating_db, key, klen, &_key, &_klen);
        if (bucket != NULL) {
            /* Copy that value into our current transaction db,
             * and then spoil it.
             */
            uint32_t hash;
            MurmurHash3_x86_32(_key, _klen, DEVILS_SEED, &hash);
            ol_bucket *copied = calloc(1, sizeof(ol_bucket));
            check_mem(copied);

            _ol_set_bucket_no_incr(operating_db, copied, hash);
        }
    }

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
        if (operating_db->is_enabled(OL_F_APPENDONLY, &operating_db->feature_set) &&
                operating_db->state != OL_S_STARTUP) {
            ol_aol_write_cmd(operating_db, "SPOIL", bucket);
        }

        /* Flag the transaction as dirty. */
        tx->dirty = 1;

        return 0;
    }

    return 1;

error:
    return 1;
}
