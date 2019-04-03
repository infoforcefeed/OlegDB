#include <sys/file.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include "aol.h"
#include "errhandle.h"
#include "oleg.h"
#include "file.h"

int _ol_get_stat(const char *filepath, struct stat *sb) {
    int fd;
    fd = open(filepath, O_RDONLY);
    /* Check if path exists */
    if (fd < 0) {
        /* File does not exist. */
        /* Error should not linger. This was intended, clear it out. */
        errno = 0;
        return 0;
    }

    int ret = 1;
    if (fstat(fd, sb) == -1)
        ret = 0;
    close(fd);
    return ret;
}

size_t _ol_get_file_size(const char *filepath) {
    struct stat sb = {0};
    int ret = _ol_get_stat(filepath, &sb);
    if (ret) /* Maybe the file doesn't exist. */
        return sb.st_size;
    return 0;
}

void *_ol_mmap(size_t to_mmap, int fd) {
    /* TODO: Investigate usage of madvise here. */
    void *to_return = NULL;

    to_return = mmap(NULL, to_mmap, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    check(to_return != MAP_FAILED, "Could not mmap file.");

    const int check = madvise(to_return, to_mmap, MADV_RANDOM);
    check_warn(check == 0, "Could not madvise file.");
    return to_return;

error:
    if (to_return)
        munmap(to_return, to_mmap);
    return NULL;
}

int _ol_open_values_with_fd(ol_database *db, const int fd, const size_t filesize) {
    const size_t to_mmap = filesize == 0 ? VALUES_DEFAULT_SIZE : filesize;

    int flock_ret = flock(fd, LOCK_EX | LOCK_NB);
    check(flock_ret == 0, "Could not lock values file.");

    /* TODO: Do we need madivse(MADV_HUGEPAGE); here? */
    db->values = _ol_mmap(to_mmap, fd);
    check(db->values != NULL, "Could not mmap values file.");

    /* Make sure the file is at least as big as VALUES_DEFAULT_SIZE. This
     * should only happen on init. */
    if (filesize == 0) {
        check(ftruncate(fd, to_mmap) != -1, "Could not truncate file for values.");
        const int max = to_mmap / sizeof(unsigned char *);
        /* Null out the stragglers */
        int i = 0;
        for (; i < max; i++) {
            ((unsigned char **)db->values)[i] = NULL;
        }
    }

    db->valuesfd = fd;
    return 1;

error:
    if (fd >= 0) {
        close(fd);
    }
    return 0;
}

void _ol_close_values(ol_database *db) {
    char values_filename[DB_NAME_SIZE] = {0};
    db->get_db_file_name(db, VALUES_FILENAME, values_filename);
    const size_t siz = _ol_get_file_size(values_filename);

    munmap(db->values, siz);
    flock(db->valuesfd, LOCK_UN);
    close(db->valuesfd);
}

int _ol_ensure_values_file_size(ol_database *db, const size_t desired_size) {
    int values_fd = 0;
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

    /* New total size */
    const size_t truncate_total = filesize + to_add;

    values_fd = open(values_filename, O_RDWR | O_CREAT, S_IWUSR | S_IRUSR);
    check(values_fd >= 0, "Could not open values file.");

    /* Set new size */
    check(ftruncate(values_fd, truncate_total) != -1, "Could not truncate file to new size for values.");
    /* TODO: Can we use realloc here instead of munmapping and then remapping? */
    _ol_close_values(db);

    return _ol_open_values_with_fd(db, values_fd, truncate_total);

error:
    /* Close file if opened */
    if (values_fd >= 0) {
        close(values_fd);
    }
    return 0;
}


int _ol_open_values(ol_database *db) {
    check(db != NULL, "DB is NULL.");

    int values_fd = 0;
    char values_filename[DB_NAME_SIZE] = { 0 };

    /* Figure out the filename */
    db->get_db_file_name(db, VALUES_FILENAME, values_filename);
    const size_t filesize = _ol_get_file_size(values_filename);

    debug("Opening %s for values", values_filename);
    values_fd = open(values_filename, O_RDWR | O_CREAT, S_IWUSR | S_IRUSR);
    check(values_fd >= 0, "Could not open file.");

    return _ol_open_values_with_fd(db, values_fd, filesize);
error:
    return 0;
}

int ol_sync(const ol_database *db) {
    check(db != NULL, "DB is NULL.");
    char values_filename[DB_NAME_SIZE] = { 0 };

    /* Figure out the filename */
    db->get_db_file_name(db, VALUES_FILENAME, values_filename);
    size_t filesize = _ol_get_file_size(values_filename);

    if (db->values != NULL) {
        msync(db->values, filesize, MS_SYNC);
    }

    if (db->is_enabled(OL_F_APPENDONLY, &db->feature_set)) {
        /* ol_log_msg(LOG_INFO, "Syncing AOL file."); */
        ol_aol_sync(db);
    }

    return 0;
error:
    return -1;
}
