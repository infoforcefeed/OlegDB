#pragma once
#include "oleg.h"

typedef struct ol_transaction {
    const int tx_id;
} ol_transaction;

/* Begins a new transaction. Fails if one is in progress. Returns the new transaction ID on success, -1 on failure. */
int olt_begin(ol_database *db);

/* Commits and finishes the transaction that matches the tx_id given. Returns 0 on success. */
int olt_commit(ol_database *db, const int tx_id);

/* Aborts the current database transaction. Returns 0 on success. */
int olt_abort(ol_database *db, const int tx_id);
