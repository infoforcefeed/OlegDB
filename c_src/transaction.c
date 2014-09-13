#include "logging.h"
#include "errhandle.h"
#include "transaction.h"

transaction_id olt_begin(ol_database *db) {
    check(db->cur_transactions != NULL, "No transaction tree.");
    return NULL;

error:
    return NULL;
}

int olt_commit(ol_database *db, transaction_id tx_id) {
    check(db->cur_transactions != NULL, "No transaction tree.");

    /* XXX: How do I turn a tx_id into a string? */
    //ol_splay_tree_node *found_tx = ols_find(db->cur_transactions,
    //                                        obj->database_name, dbname_len);
    //check(found_tx != NULL, "Could not find transaction in tree.");

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
