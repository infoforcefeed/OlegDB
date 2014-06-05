#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include "errhandle.h"
#include "oleg.h"
#include "file.h"

int _ol_get_stat(const char *filepath, struct stat *sb) {
    int fd;
    fd = open(filepath, O_RDONLY);
    if (fd == -1)
        return 0;

    if (fstat(fd, sb) == -1)
        return 0;
    close(fd);
    return 1;
}

int _ol_get_file_size(const char *filepath) {
    struct stat sb = {0};
    int ret = _ol_get_stat(filepath, &sb);
    if (ret) /* Maybe the file doesn't exist. */
        return sb.st_size;
    return -1;
}

void *_ol_mmap(size_t to_mmap, int fd) {
    /* TODO: Investigate usage of madvise here. */
    void *to_return = NULL;

    to_return = mmap(NULL, to_mmap, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    check(to_return != MAP_FAILED, "Could not mmap hashes file.");
    return to_return;

error:
    return NULL;
}

int _ol_open_hashtable(ol_database *db) {
    check(db != NULL, "DB is NULL.");
    int hashes_fd = { 0 };
    char hashes_filename[DB_NAME_SIZE] = { 0 };

    /* Figure out the filename */
    db->get_db_file_name(db, HASHES_FILENAME, hashes_filename);
    int filesize = _ol_get_file_size(hashes_filename);
    int to_mmap = filesize <= 0 ? HASH_MALLOC : filesize;
    db->cur_ht_size = to_mmap;

    debug("Opening %s for hashes", hashes_filename);
    hashes_fd = open(hashes_filename, O_RDWR | O_CREAT, S_IWUSR | S_IRUSR);

    check(hashes_fd > 0, "Could not open hashtable file.");
    db->hashes = _ol_mmap(to_mmap, hashes_fd);
    check(db->hashes != NULL, "Could not mmap hashtable file.");

    /* Make sure the file is at least as big as HASH_MALLOC */
    if (_ol_get_file_size(hashes_filename) == 0) {
         check(ftruncate(hashes_fd, HASH_MALLOC) != -1, "Could not truncate file for hashes.");
         int i;
         /* Null out the stragglers */
         for (i = 0; i < ol_ht_bucket_max(db->cur_ht_size); i++)
             db->hashes[i] = NULL;
    }
    close(hashes_fd);
    return 1;

error:
    if (hashes_fd)
        close(hashes_fd);
    return 0;
}

int _ol_open_values(ol_database *db) {
    check(db != NULL, "DB is NULL.");
    int values_fd = { 0 };
    char values_filename[DB_NAME_SIZE] = { 0 };

    /* Figure out the filename */
    db->get_db_file_name(db, VALUES_FILENAME, values_filename);
    int filesize = _ol_get_file_size(values_filename);

    debug("Opening %s for values", values_filename);
    values_fd = open(values_filename, O_RDWR | O_CREAT, S_IWUSR | S_IRUSR);

    check(values_fd > 0, "Could not open file.");
    /* TODO: Do we need madivse(MADV_HUGEPAGE); here? */
    db->values = _ol_mmap(filesize, values_fd);
    check(db->values != NULL, "Could not mmap values file.");

    close(values_fd);
    return 1;

error:
    if (values_fd)
        close(values_fd);
    return 0;
}

