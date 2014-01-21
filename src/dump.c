#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include "oleg.h"
#include "dump.h"


static inline int _ol_serialize_bucket(const ol_bucket *bucket, FILE *fd) {
    do {
        fwrite(&bucket->key, sizeof(char), KEY_SIZE, fd);
        fwrite(&bucket->data_size, sizeof(size_t), 1, fd);
        fwrite(bucket->data_ptr, sizeof(char), bucket->data_size, fd);
        bucket = bucket->next;
    } while (bucket != NULL);
    return 0;
}
static inline int _ol_deserialize_bucket(const ol_bucket *bucket, FILE *fd) {
    return 0;
}

int ol_save_db(ol_database *db) {
    //int ret;
    FILE *fd;
    struct dump_header header;
    char tmpfile[256] = "/tmp/tmp-olegdb.dump";

    fd = fopen(tmpfile, "w");
    if (!fd) {
        printf("Error: Can't opening file: %s\n", strerror(errno));
        return -1;
    }

    /* do the saving here */
    snprintf(header.sig, sizeof(DUMP_SIG), "%s", DUMP_SIG);
    snprintf(header.version, sizeof(DUMP_VERSION), "%03d", DUMP_VERSION);
    header.rcrd_cnt = db->rcrd_cnt;
    if (fwrite(&header, sizeof(header), 1, fd) != 1) {
        printf("Error: Can't write header to file. %s\n", strerror(errno));
        return -1;
    }

    int i;
    int bucket_max = _ol_ht_bucket_max(db->cur_ht_size);
    ol_bucket *bucket;
    for (i = 0; i < bucket_max; i++) {
        bucket = db->hashes[i];
        if (bucket != NULL) {
            if (_ol_serialize_bucket(bucket, fd) != 0) {
                printf("Error: Could not serialize bucket\n");
                return -1;
            }
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
    int i, dump_version;
    struct dump_header header;

    fd = fopen(filename, "r");
    if (!fd) {
        printf("Error: Can't opening file: %s\n", strerror(errno));
        return -1;
    }

    fread(&header, sizeof(header), 1, fd);
    if (memcmp(header.sig, DUMP_SIG, 10) != 0) {
        fclose(fd);
        printf("Error: Not a valid oleg dump\n");
        return -1;
    }
    dump_version = atoi(header.version);
    if (dump_version != DUMP_VERSION) {
        fclose(fd);
        printf("Error: Cannot parse this version\n");
        return -1;
    }
    char *tmp_key;
    unsigned char *tmp_value;
    size_t value_size;
    for (i = 0; i < header.rcrd_cnt; i++) {
        tmp_key = malloc(KEY_SIZE);
        fread(tmp_key, sizeof(char), KEY_SIZE, fd);
        fread(&value_size, sizeof(size_t), 1, fd);
        tmp_value = calloc(1, value_size);
        fread(tmp_value, sizeof(char), value_size, fd);
        ol_jar(db, tmp_key, tmp_value, value_size);
        free(tmp_key);
        free(tmp_value);
    }

    fclose(fd);

    return 0;
}
