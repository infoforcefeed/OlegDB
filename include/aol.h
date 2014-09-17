#pragma once
/* Prototype functions for the append-only log. */
#include "oleg.h"

/* Filemode used for writing AOL commands. Restore just uses "r" */
#define AOL_FILEMODE "ab+"

int ol_aol_init(ol_database *db);
int ol_aol_write_cmd(ol_database *db, const char *cmd, ol_bucket *bucket);
/* Restore this database's AOL file. */
int ol_aol_restore(ol_database *db);
/* fsync the AOL file. */
int ol_aol_sync(const ol_database *db);

/* Restore from a specific aol file to a specific database. */
int ol_aol_restore_from_file(ol_database *target_db,
        const char aol_fname[AOL_FILENAME_ALLOC],
        const unsigned char *values_data);
