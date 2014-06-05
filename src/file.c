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

size_t _ol_get_file_size(const char *filepath) {
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

int _ol_ensure_values_file_size(ol_database *db, const size_t desired_size) {
    int values_fd = { 0 };
    char values_filename[DB_NAME_SIZE] = { 0 };

    /* Figure out the filename */
    db->get_db_file_name(db, VALUES_FILENAME, values_filename);
    size_t filesize = _ol_get_file_size(values_filename);
    const size_t size_needed = db->val_size + desired_size;

    if (filesize > size_needed)
        return 1;

    /* Sync everything to disk before we fuck with it */
    msync(db->values, filesize, MS_SYNC);

    size_t to_add = VALUES_DEFAULT_SIZE;
    while (filesize + to_add < size_needed) {
        to_add += VALUES_DEFAULT_SIZE;
    }

    const size_t truncate_total = filesize + to_add;
    if (_ol_get_file_size(values_filename) == 0) {
        check(ftruncate(values_fd, to_mmap) != -1, "Could not truncate file to new size for values.");
        /* TODO: Can we use realloc here instead of munmapping and then remapping? */
        munmap(db->values, db->val_size);
    }

    return _ol_open_values_with_fd(db, values_fd, truncate_total);
}


int _ol_open_values(ol_database *db) {
    check(db != NULL, "DB is NULL.");
    int values_fd = { 0 };
    char values_filename[DB_NAME_SIZE] = { 0 };

    /* Figure out the filename */
    db->get_db_file_name(db, VALUES_FILENAME, values_filename);
    size_t filesize = _ol_get_file_size(values_filename);

    debug("Opening %s for values", values_filename);
    values_fd = open(values_filename, O_RDWR | O_CREAT, S_IWUSR | S_IRUSR);
    check(fd > 0, "Could not open file.");

    return _ol_open_values_with_fd(db, values_fd, filesize);
error:
    if (values_fd > 0)
        close(values_fd);

    return 0;
}

int _ol_open_values_with_fd(ol_database *db, const int fd, const size_t filesize) {
    const size_t to_mmap = filesize >= 0 ? filsize : VALUES_DEFAULT_SIZE;

    /* TODO: Do we need madivse(MADV_HUGEPAGE); here? */
    db->values = _ol_mmap(to_mmap, fd);
    check(db->values != NULL, "Could not mmap values file.");

    /* Make sure the file is at least as big as HASH_MALLOC */
    if (_ol_get_file_size(values_filename) == 0) {
         check(ftruncate(fd, to_mmap) != -1, "Could not truncate file for values.");
         int i;
         /* Null out the stragglers */
         for (i = 0; i < to_mmap; i++)
             db->values[i] = NULL;
    }

    close(fd);
    return 1;

error:
    if (fd)
        close(fd);
    return 0;
}

