#pragma once
#include <sys/stat.h>
#include "oleg.h"

/* Get the stat object of a file. */
int _ol_get_stat(const char *filepath, struct stat *sb);

/* Gets the size of a file. Duh. */
size_t _ol_get_file_size(const char *filepath);

/* Closes the values file, munmaps/flocks etc. */
void _ol_close_values(ol_database *db);

/* Wraps mmap so that we mmap() consistently. */
void *_ol_mmap(size_t to_mmap, int fd);

/* Called whenever a value is inserted into the database. Truncates the value
 * file to the correct size, aligned by VALUES_DEFAULT_SIZE chunks. */
int _ol_ensure_values_file_size(ol_database *db, const size_t desired_size);

/* Opens the values file for a database. */
int _ol_open_values(ol_database *db);

/* Makes sure the AOL and values file are sync'd to the
 * disk. */
int ol_sync(const ol_database *db);
