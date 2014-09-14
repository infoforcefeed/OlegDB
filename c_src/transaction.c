#define __STDC_FORMAT_MACROS
#include <inttypes.h>
#include <stdlib.h>

#include "oleg.h"
#include "logging.h"
#include "errhandle.h"
#include "transaction.h"
#include "utils.h"
#include "tree.h"

/* Where we start our transactions from. */
transaction_id global_transaction_id = 1;

char *tx_to_str(const transaction_id tx_id) {
    char *_key = NULL;
    _key = malloc(sizeof(char) * KEY_SIZE);
    check_mem(_key);
    snprintf(_key, KEY_SIZE, "%" PRIu64, tx_id);

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

transaction_id olt_begin(ol_database *db) {
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

    /* Make sure implciti transactions is turned OFF, because otherwise we'll
     * get endless recursion. Wooo! */
    ol_feature_flags flags = OL_F_APPENDONLY | OL_F_SPLAYTREE | OL_F_LZ4 | OL_F_DISABLE_TX;
    stack_tx.transaction_db = ol_open(new_path, name, flags);
    check(stack_tx.transaction_db != NULL, "Could not open transaction database.");
    free(name);

    /* Copy the stack thing into the heap thing. */
    new_transaction = malloc(sizeof(ol_transaction));
    check_mem(new_transaction);
    memcpy(new_transaction, &stack_tx, sizeof(ol_transaction));
    free(name);

    return 0;

error:
    if (new_transaction != NULL)
        free(new_transaction);
    return -1;
}

int olt_commit(ol_database *db, transaction_id tx_id) {
    check(db->cur_transactions != NULL, "No transaction tree.");

    /* XXX: How do I turn a tx_id into a string? */
    ol_splay_tree_node *found_tx = ols_find_tx_id(db->cur_transactions, tx_id);
    check(found_tx != NULL, "Could not find transaction in tree.");

    return 1;

error:
    return 1;
}

int olt_abort(ol_database *db, transaction_id tx_id) {
    check(db->cur_transactions != NULL, "No transaction tree.");

    return 1;

error:
    return 1;
}
