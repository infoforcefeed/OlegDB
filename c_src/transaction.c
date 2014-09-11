#include "logging.h"
#include "errhandle.h"
#include "transaction.h"

int olt_begin(ol_database *db) {
    check(db->cur_transactions != NULL, "No transaction tree.");
    return -1;

error:
    return -1;
}

int olt_commit(ol_database *db, const int tx_id) {
    check(db->cur_transactions != NULL, "No transaction tree.");
    return 1;

error:
    return 1;
}

int olt_abort(ol_database *db, const int tx_id) {
    check(db->cur_transactions != NULL, "No transaction tree.");
    return 1;

error:
    return 1;
}
