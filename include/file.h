#pragma once
#include "oleg.h"

/* Opens the hashtable for a database. */
int _ol_open_hashtable(ol_database *db);

/* Opens the values file for a database. */
int _ol_open_values(ol_database *db);

/* Get the stat object of a file. */
int _ol_get_stat(const char *filepath, struct stat *sb);

/* Gets the size of a file. Duh. */
int _ol_get_file_size(const char *filepath);

/* Wrapps mmap so that we mmap() consistently. */
void *_ol_mmap(size_t to_mmap, int fd);
