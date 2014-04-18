#pragma once
/* Stuff used for manipulating dump files. */

#include "oleg.h"

#define DUMP_SIG "OLEG"
#define DUMP_SIG_LEN 4
#define DUMP_VERSION 1
#define DUMP_VERSION_LEN 4

struct dump_header {
    char sig[DUMP_SIG_LEN];
    char version[DUMP_VERSION_LEN];
    int  rcrd_cnt;
};

int ol_save_db(ol_database *db);
int ol_background_save(ol_database *db);
int ol_load_db(ol_database *db, char *filename);
