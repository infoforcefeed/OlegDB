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
    //int ret;
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
    printf("header is: %s", header);
    fwrite(header, sizeof(header), 1, fd);
    int *num_records = &db->rcrd_cnt;
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

    //sleep(100);

    // ret = rename(tmpfile, db->dump_file);
    // if (ret == -1) {
    //     printf("Error: Can't rename file: %s\n", strerror(errno));
    //     unlink(tmpfile);
    //     return -1;
    // }
    return 0;
}

int ol_load_db(ol_database *db, char *filename) {
    FILE *fd;
    int i, dump_version, rcrd_cnt;

    fd = fopen(filename, "r");
    if (!fd) {
        printf("Error: Can't opening file: %s\n", strerror(errno));
        return -1;
    }

    char buf[14];
    fread(&buf, sizeof(DUMP_SIG) + 3, 1, fd);
    printf("buf is: %s\n", buf);
    if (memcmp(buf, DUMP_SIG, 10) != 0) {
        fclose(fd);
        printf("Error: Not a valid oleg dump\n");
        return -1;
    }
    dump_version = atoi(buf + 10);
    printf("version is: %i\n", dump_version);
    if (dump_version != DUMP_VERSION) {
        fclose(fd);
        printf("Error: Cannot parse this version\n");
        return -1;
    }
    buf[0] = '\0';
    fread(&buf, 1, 1, fd);
    rcrd_cnt = *buf;
    printf("rcrd_cnt is: %i\n", rcrd_cnt);

    char *tmp_key, *tmp_value;
    int key_size, value_size;
    for (i = 0; i < rcrd_cnt; i++) {
        fread(&key_size, 1, 1, fd);
        printf("Key size is: %i\n", key_size);
        tmp_key = malloc(key_size);
        fread(tmp_key, key_size, 1, fd);
        printf("tmp_key is: %s\n", tmp_key);
        fread(&value_size, 1, 1, fd);
        printf("value size is: %i\n", value_size);
        tmp_value = malloc(value_size);
        fread(tmp_value, value_size, 1, fd);
        printf("tmp_value is: %s\n", tmp_value);
    }

    fclose(fd);

    return 0;
}
