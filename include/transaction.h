#pragma once
#include "oleg.h"

typedef const char* transaction_id;
typedef struct ol_transaction {
    transaction_id tx_id;
    const ol_database *db;
} ol_transaction;

/* Begins a new transaction. Fails if one is in progress. Returns the new transaction ID on success, NULL on failure. */
transaction_id olt_begin(ol_database *db);

/* Commits and finishes the transaction that matches the tx_id given. Returns 0 on success. */
int olt_commit(ol_database *db, transaction_id tx_id);

/* Aborts the current database transaction. Returns 0 on success. */
int olt_abort(ol_database *db, transaction_id tx_id);

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
