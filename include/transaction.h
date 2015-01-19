#pragma once
#include "oleg.h"

typedef struct ol_transaction {
    const transaction_id tx_id;
    int dirty;
    ol_database *parent_db;
    ol_database *transaction_db;
} ol_transaction;

/* Incrementing counter of transaction IDs. Restarts with the program. */
extern transaction_id global_transaction_id;

/* Begins a new transaction. Fails if one is in progress. Returns the new transaction on success, NULL on failure. */
ol_transaction *olt_begin(ol_database *db);

/* Commits and finishes the transaction that matches the tx_id given. Returns 0 on success. */
int olt_commit(ol_transaction *tx);

/* Aborts the current database transaction. Returns 0 on success. */
int olt_abort(ol_transaction *tx);

/* Internal function used to find a transaction in a splay tree. */
ol_splay_tree_node *ols_find_tx_id(ol_splay_tree *tree, const transaction_id key);

/* ACTUAL COMMANDS */
/* --------------- */

/* All of these commands are mirrors of their counterparts in oleg.h, except
 * they operate on a transaction level instead of a database level.
 */
int olt_unjar(ol_transaction *tx, const char *key, size_t klen, unsigned char **data, size_t *dsize);

int olt_jar(ol_transaction *tx, const char *key, size_t klen, const unsigned char *value, size_t vsize);

struct tm *olt_expiration_time(ol_transaction *tx, const char *key, size_t klen);
int olt_scoop(ol_transaction *tx, const char *key, size_t klen);
int olt_spoil(ol_transaction *tx, const char *key, size_t klen, struct tm *expiration_date);

int olt_exists(ol_transaction *tx, const char *key, size_t klen);
