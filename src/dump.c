#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include "oleg.h"
#include "logging.h"
#include "dump.h"
#include "errhandle.h"


static inline int _ol_write_bucket(const ol_bucket *bucket, FILE *fd) {
    do {
        fwrite(&bucket->klen, sizeof(size_t), 1, fd);
        fwrite(&bucket->key, sizeof(char), bucket->klen, fd);
        fwrite(&bucket->data_size, sizeof(size_t), 1, fd);
        fwrite(bucket->data_ptr, sizeof(char), bucket->data_size, fd);
        bucket = bucket->next;
    } while (bucket != NULL);
    return 0;
}

static inline int _ol_store_bin_object(ol_database *db, FILE *fd) {
    char *tmp_key;
    unsigned char *tmp_value;
    size_t value_size;
    size_t fread_ret;
    size_t klen;

    fread_ret = fread(&klen, sizeof(size_t), 1, fd);
    check(fread_ret > 0, "Error reading");

    if (klen > KEY_SIZE) {
        ol_log_msg(LOG_ERR, "Key size too damn big! Size: %zu", klen);
    }

    tmp_key = malloc(klen + 1);
    check_mem(tmp_key);

    fread_ret = fread(tmp_key, sizeof(char), klen, fd);
    check(fread_ret > 0, "Error reading");
    fread_ret = fread(&value_size, sizeof(size_t), 1, fd);
    check(fread_ret > 0, "Error reading");

    tmp_value = calloc(1, value_size);
    check_mem(tmp_value);

    fread_ret = fread(tmp_value, sizeof(char), value_size, fd);
    check(fread_ret > 0, "Error reading");
    ol_jar(db, tmp_key, klen, tmp_value, value_size);

    free(tmp_key);
    free(tmp_value);
    return 0;

error:
    return -1;
}

int ol_background_save(ol_database *db) {
    int pid;

    pid = fork();
    if (pid == 0) {
        int ret;
        ret = ol_save_db(db);
        if(ret != 0)
            log_err("Could not save DB to disk."); exit(ret);
    } else {
        check(pid > 0, "Could not background dump.");
        ol_log_msg(LOG_INFO, "Backgrounding ol_dump. PID: %d", pid);
        return 0;
    }
    return 0;

error:
    return -1;
}

int ol_save_db(ol_database *db) {
    FILE *fd;
    struct dump_header header;
    char tmpfile[512];
    sprintf(tmpfile, "%s-tmp", db->dump_file);

    debug("Opening file %s", tmpfile);
    fd = fopen(tmpfile, "w+");
    check(fd, "Failed to open file: %s", tmpfile);

    /* Write the header to the file 
     * Why do we use a temporary buffer here? Because snprintf ALWAYS appends
     * a null string, so you'll end up with the wrong output. Since theres no
     * way to use snprintf and not get warnings (Fuck warnings!), we print to
     * a temporary buffer and then copy that into our struct. -QP
     */
    char tmp_sig_buf[DUMP_SIG_LEN + 1];
    snprintf(tmp_sig_buf, DUMP_SIG_LEN + 1, "%s", DUMP_SIG);
    strncpy(header.sig, tmp_sig_buf, DUMP_SIG_LEN);

    char tmp_ver_buf[DUMP_VERSION_LEN + 1];
    snprintf(tmp_ver_buf, DUMP_VERSION_LEN + 1, "%04i", DUMP_VERSION);
    strncpy(header.version, tmp_ver_buf, DUMP_SIG_LEN);

    header.rcrd_cnt = db->rcrd_cnt;
    check(fwrite(&header, sizeof(header), 1, fd) == 1, "Write failed.");

    /* Start serializing the struct and write to the file */
    int i;
    int bucket_max = ol_ht_bucket_max(db->cur_ht_size);
    ol_bucket *item;
    for (i = 0; i < bucket_max; i++) {
        item = db->hashes[i];
        if (item != NULL)
            check(_ol_write_bucket(item, fd) == 0, "Recording bucket failed.");
    }

    fflush(fd);
    fclose(fd);

    debug("Renaming file: %s -> %s.", tmpfile, db->dump_file);
    check(rename(tmpfile, db->dump_file) == 0, "Could not rename.")

    return 0;

error:
    unlink(tmpfile);
    return -1;
}

int ol_load_db(ol_database *db, char *filename) {
    /* Pass in a filename so we can load in dumpfiles other than what
     * the db thinks we should have. For instance, a new or alternate
     * dataset.
     */
    FILE *fd;
    int i, dump_version;
    struct dump_header header;

    debug("Opening file %s", filename);
    fd = fopen(filename, "r");
    check(fd, "Failed to open file: %s", filename);

    check(fread(&header, sizeof(header), 1, fd) > 0, "Error reading.");
    if (memcmp(header.sig, DUMP_SIG, 4) != 0) {
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

    /* Load up the database */
    for (i = 0; i < header.rcrd_cnt; i++) {
        check(_ol_store_bin_object(db, fd) == 0, "Could not read item.");
    }

    fclose(fd);

    return 0;

error:
    return -1;
}
