#pragma once

#include "oleg.h"

#define DUMP_SIG "OLEGDBDUMP"
#define DUMP_VERSION 1

struct dump_header {
    char sig[11];
    char version[4];
    int  rcrd_cnt;
};

int ol_save_db(ol_database *db);
int ol_background_save(ol_database *db);
int ol_load_db(ol_database *db, char *filename);
