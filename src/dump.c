#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include "oleg.h"
#include "dump.h"


static inline int ol_serialize_bucket(const ol_bucket *bucket) {
    return 0;
}

int ol_save_db(ol_database *db) {
    int ret;
    FILE *fd;
    char header[14];
    char tmpfile[256] = "/tmp/tmp-olegdb.dump";

    fd = fopen(tmpfile, "w");
    if (!fd) {
        printf("Error: Can't opening file: %s\n", strerror(errno));
        return -1;
    }

    /* do the saving here */
    printf("size: %zu\n", sizeof(VERSION));
    snprintf(header, sizeof(header), "%s%03d", DUMP_SIG, DUMP_VERSION);
    fwrite(header, sizeof(header), 1, fd);
    int *num_records = &(db->rcrd_cnt);
    fwrite(num_records, sizeof(db->rcrd_cnt), 1, fd);

    int i;
    int bucket_max = _ol_ht_bucket_max(db->cur_ht_size);
    ol_bucket *bucket;
    for (i = 0; i < bucket_max; i++) {
        bucket = db->hashes[i];
        if (bucket != NULL) {
            do {
                size_t key_size = sizeof(bucket->key);
                fwrite(&key_size, sizeof(int), 1, fd);
                fwrite(bucket->key, sizeof(char), key_size, fd);
                fwrite(&bucket->data_size, sizeof(int), 1, fd);
                fwrite(bucket->data_ptr, sizeof(char), bucket->data_size, fd);
                bucket = bucket->next;
            } while (bucket != NULL);
        }
    }

    fflush(fd);
    fclose(fd);

    sleep(100);

    ret = rename(tmpfile, db->dump_file);
    if (ret == -1) {
        printf("Error: Can't rename file: %s\n", strerror(errno));
        unlink(tmpfile);
        return -1;
    }
    return 0;
}
