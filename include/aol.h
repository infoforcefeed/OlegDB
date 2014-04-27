#pragma once
/* Prototype functions for the append-only log. */
#include "oleg.h"

int ol_aol_init(ol_database *db);
int ol_aol_write_cmd(ol_database *db, const char *cmd, ol_bucket *bucket);
int ol_aol_restore(ol_database *db);
int ol_aol_fsync(FILE *fd);
int ol_aol_rewrite(ol_database *db);
