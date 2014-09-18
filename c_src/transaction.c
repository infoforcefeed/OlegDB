#define __STDC_FORMAT_MACROS
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

char *tx_to_str(const transaction_id tx_id) {
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

ol_transaction *olt_begin(ol_database *db) {
    ol_transaction *new_transaction = NULL;
    check(db->cur_transactions != NULL, "No transaction tree.");
    check(db != NULL, "No database specified in transaction begin.");

    /* We initialize on the stack because tx_id is a const parameter. */
    ol_transaction stack_tx = {
        .tx_id = global_transaction_id,
        .parent_db = db,
        .transaction_db = NULL
    };

    /* Setup a ".../tx/" directory for our transaction databases. */
    char new_path[PATH_LENGTH] = {0};
    snprintf(new_path, PATH_LENGTH, "%s/%s", db->path, "tx");

    /* Convert our integer tx_id into a string */
    char *name = tx_to_str(stack_tx.tx_id);
    check(name != NULL, "Could not convert tx_id to str.");

    /* Make sure implicit transactions is turned OFF, because otherwise we'll
     * get endless recursion. Wooo! */
    ol_feature_flags flags = OL_F_APPENDONLY | OL_F_SPLAYTREE |
                             OL_F_AOL_FFLUSH | OL_F_LZ4 | OL_F_DISABLE_TX;
    stack_tx.transaction_db = ol_open(new_path, name, flags);
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

    return new_transaction;

error:
    if (new_transaction != NULL)
        free(new_transaction);
    return NULL;
}

static int _olt_cleanup(ol_database *db, char *values_filename, char *tx_aol_filename) {
    /* Yeah, come at me. */
    debug("Unlinking values file for transaction, %s", values_filename);
    unlink(values_filename);

    debug(LOG_WARN, "Unlinking aol file for transaction, %s", tx_aol_filename);
    unlink(tx_aol_filename);

    return ol_close(db);
}

int olt_commit(ol_transaction *tx) {
    /* So at this point we should have a series of operations stored up and
     * successful in our tx->transaction_db. At this point we need to replay
     * them all back onto the parent_db.
     */
    check(tx->parent_db != NULL, "No parent database.");
    check(tx->parent_db->cur_transactions != NULL, "No transaction tree.");

    char tx_aol_filename[AOL_FILENAME_ALLOC] = {0};
    tx->transaction_db->get_db_file_name(tx->transaction_db, AOL_FILENAME, tx_aol_filename);

    char values_filename[DB_NAME_SIZE] = {0};
    tx->transaction_db->get_db_file_name(tx->transaction_db, VALUES_FILENAME, values_filename);

    /* Don't squish or compact or anything here because it'll remove stuff
     * that we don't want removed from the log file, like SCOOP commands.
     */

    /* Make sure everything is written: */
    ol_sync(tx->transaction_db);
    tx->parent_db->state = OL_S_COMMITTING;

    ol_aol_restore_from_file(tx->parent_db, tx_aol_filename, tx->transaction_db->values);

    tx->parent_db->state = OL_S_AOKAY;

    return _olt_cleanup(tx->transaction_db, values_filename, tx_aol_filename);

error:
    return 1;
}

int olt_abort(ol_transaction *tx) {
    check(tx->parent_db != NULL, "No parent database.");
    check(tx->parent_db->cur_transactions != NULL, "No transaction tree.");

    return 1;

error:
    return 1;
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
        return _ol_reallocate_bucket(db, bucket, value, vsize);
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

    return 0;

error:
    return 1;
}
