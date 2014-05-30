/* Common utility functions. */
#include <fcntl.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include "oleg.h"
#include "utils.h"
#include "logging.h"

ol_bucket *_ol_get_last_bucket_in_slot(ol_bucket *bucket) {
    ol_bucket *tmp_bucket = bucket;
    int depth = 0;
    while (tmp_bucket->next != NULL) {
        tmp_bucket = tmp_bucket->next;
        depth++;
        if (depth > 1000)
            ol_log_msg(LOG_WARN, "Depth of bucket stack is crazy, help! It's at %i", depth);
    }
    return tmp_bucket;
}

void _ol_free_bucket(ol_bucket **ptr) {
    free((*ptr)->expiration);
    free((*ptr)->content_type);
    free((*ptr)->data_ptr);
    free((*ptr));
    *ptr = NULL;
}

int _ol_calc_idx(const size_t ht_size, const uint32_t hash) {
    int index;
    /* Powers of two, baby! */
    index = hash & (ol_ht_bucket_max(ht_size) - 1);
    return index;
}

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
    struct stat sb;
    _ol_get_stat(filepath, &sb);

    return (int)sb.st_size;
}
