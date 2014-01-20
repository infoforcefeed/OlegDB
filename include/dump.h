#pragma once

#include "oleg.h"

#define DUMP_SIG "OLEGDBDUMP"
#define DUMP_VERSION 1

int ol_save_db(ol_database *db);
int ol_load_db(char *filename);
