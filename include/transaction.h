#pragma once
#include "oleg.h"

#define transaction_id uint64_t

typedef struct ol_transaction {
    const transaction_id tx_id;
    ol_database *parent_db;
    ol_database *transaction_db;
} ol_transaction;

/* Incrementing counter of transaction IDs. Restarts with the program. */
extern transaction_id global_transaction_id;

/* Begins a new transaction. Fails if one is in progress. Returns the new transaction ID on success, NULL on failure. */
transaction_id olt_begin(ol_database *db);

/* Commits and finishes the transaction that matches the tx_id given. Returns 0 on success. */
int olt_commit(ol_database *db, const transaction_id tx_id);

/* Aborts the current database transaction. Returns 0 on success. */
int olt_abort(ol_database *db, const transaction_id tx_id);

/* Internal function used to find a transaction in a splay tree. */
ol_splay_tree_node *ols_find_tx_id(ol_splay_tree *tree, const transaction_id key);

/* ACTUAL COMMANDS */
/* --------------- */

/* All of these commands are mirrors of their counterparts in oleg.h, except
 * they operate on a transaction level instead of a database level.
 */
int olt_unjar(const int tx_id, const char *key, size_t klen, unsigned char **data);
int olt_unjar_ds(const int tx_id, const char *key, size_t klen, unsigned char **data, size_t *dsize);

int olt_jar(const int tx_id, const char *key, size_t klen, unsigned char *value, size_t vsize);

struct tm *olt_expiration_time(const int tx_id, const char *key, size_t klen);
int olt_scoop(const int tx_id, const char *key, size_t klen);
int olt_spoil(const int tx_id, const char *key, size_t klen, struct tm *expiration_date);

int olt_exists(const int tx_id, const char *key, size_t klen);
