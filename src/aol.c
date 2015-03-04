#include "aol.h"
#include "data.h"
#include "file.h"
#include "oleg.h"
#include "logging.h"
#include "errhandle.h"
#include "lz4.h"
#include "utils.h"

#include <sys/file.h>
#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#define APPEND_UINT(X) {\
    strncat(write_buf, ":", write_buf_size);\
    char x_buf[MAX_SIZE_T_STR_SIZE];\
    sizet_to_a(X, uintlen(X), x_buf);\
    strncat(write_buf, x_buf, write_buf_size);\
};

int ol_aol_init(ol_database *db) {
    if (db->is_enabled(OL_F_APPENDONLY, &db->feature_set)) {
        if (db->aolfd == 0) {
            debug("Opening append only log");
            debug("Append only log: %s", db->aol_file);
            db->aolfd = open(db->aol_file, O_RDWR, AOL_FILEMODE);
            check(db->aolfd > 0, "Error opening append only file");

            int flock_ret = flock(db->aolfd, LOCK_NB | LOCK_EX);
            check(flock_ret == 0, "Could not lock AOL file.");
        } else {
            ol_log_msg(LOG_WARN, "AOL already initialized.");
        }
    }

    return 0;
error:
    return -1;
}

static inline void _serialize_time(struct tm *time, char *buf) {
    strftime(buf, 21, "%FT%TZ", time);
}

void _deserialize_time(struct tm *fillout, char *buf) {
    /* Example 8601 datestamp: 2014-03-08T11:17:39Z */

    sscanf(buf, "%4d-%2d-%2dT%2d:%2d:%2dZ",
        &fillout->tm_year,
        &fillout->tm_mon,
        &fillout->tm_mday,
        &fillout->tm_hour,
        &fillout->tm_min,
        &fillout->tm_sec
    );

    fillout->tm_year -= 1900;
    fillout->tm_mon -= 1;
}

int ol_aol_sync(const ol_database *db) {
    /* Force the OS to flush write to hardware */
    if (db->is_enabled(OL_F_AOL_FFLUSH, &db->feature_set))
        check(fsync(db->aolfd) == 0, "Could not fflush.");
    /* AOL should always fsync at least. */
    check(fsync(db->aolfd) == 0, "Could not fsync");
    return 0;

error:
    return -1;
}
int ol_aol_write_cmd(ol_database *db, const char *cmd, ol_bucket *bct) {
    int ret;

    if (strncmp(cmd, "JAR", 3) == 0) {
        /* I'LL RIGOR YER MORTIS */
        const size_t write_buf_size =
            strlen(":3:JAR:") + uintlen(bct->klen) + strlen(":") + strlen(bct->key) +
            strlen(":1: ") +
            strlen(":") + uintlen(uintlen(bct->original_size)) + strlen(":") + uintlen(bct->original_size) +
            strlen(":") + uintlen(uintlen(bct->data_size)) + strlen(":") + uintlen(bct->data_size) +
            strlen(":") + uintlen(uintlen(bct->data_offset)) + strlen(":") + uintlen(bct->data_offset) +
            strlen("\n");

        char write_buf[write_buf_size + 1];
        memset(write_buf, '\0', write_buf_size);

        /* CMD */
        strncpy(write_buf, ":3:JAR", write_buf_size);

        APPEND_UINT(bct->klen);

        /* Key */
        strncat(write_buf, ":", write_buf_size);
        strncat(write_buf, bct->key, write_buf_size);

        /* Deprecated content type shit */
        strncat(write_buf, ":1: ", write_buf_size);

        APPEND_UINT(uintlen(bct->original_size));
        APPEND_UINT(bct->original_size);

        APPEND_UINT(uintlen(bct->data_size));
        APPEND_UINT(bct->data_size);

        APPEND_UINT(uintlen(bct->data_offset));
        APPEND_UINT(bct->data_offset);

        strncat(write_buf, "\n", write_buf_size);
        ret = write(db->aolfd, write_buf, write_buf_size);
        check(ret > -1, "Could not write JAR to AOL file.");
    } else if (strncmp(cmd, "SCOOP", 5) == 0) {
        const size_t write_buf_size =
            strlen(":5:SCOOP:") + uintlen(bct->klen) +
            strlen(":") + strlen(bct->key) + strlen("\n");

        char write_buf[write_buf_size + 1];
        memset(write_buf, '\0', write_buf_size);

        /* CMD */
        strncpy(write_buf, ":5:SCOOP", write_buf_size);

        APPEND_UINT(bct->klen);

        /* Key */
        strncat(write_buf, ":", write_buf_size);
        strncat(write_buf, bct->key, write_buf_size);
        strncat(write_buf, "\n", write_buf_size);

        ret = write(db->aolfd, write_buf, write_buf_size);
        check(ret > -1, "Could not write SCOOP to AOL file.");
    } else if (strncmp(cmd, "SPOIL", 5) == 0) {
        char exptime[21] = {'\0'};
        _serialize_time(bct->expiration, exptime);

        const size_t write_buf_size =
            strlen(":5:SPOIL") +
            strlen(":") + uintlen(bct->klen) +
            strlen(":") + strlen(bct->key) +
            strlen(":") + uintlen(strlen(exptime)) +
            strlen(":") + strlen(exptime) +
            strlen("\n");

        char write_buf[write_buf_size + 1];
        memset(write_buf, '\0', write_buf_size);

        /* CMD */
        strncpy(write_buf, ":5:SPOIL", write_buf_size);

        APPEND_UINT(bct->klen);

        /* Key */
        strncat(write_buf, ":", write_buf_size);
        strncat(write_buf, bct->key, write_buf_size);

        APPEND_UINT(strlen(exptime));

        strncat(write_buf, ":", write_buf_size);
        strncat(write_buf, exptime, write_buf_size);

        strncat(write_buf, "\n", write_buf_size);

        ret = write(db->aolfd, write_buf, write_buf_size);
        check(ret > -1, "Could not write SPOIL to AOL file.");
    } else {
        ol_log_msg(LOG_ERR, "No such command '%s'", cmd);
        return -1;
    }

    check(ret > -1, "Error writing to file.");
    return ol_aol_sync(db);
error:
    return -1;
}

ol_string _ol_read_data(FILE *fd) {
    ol_string data = {0};

    int c = fgetc(fd);
    if (c == ':'){
        int i = 0;
        size_t l = 0;
        char buf[20] = {0};
        while ((c = fgetc(fd)) != ':') {
            check(isdigit(c) != 0, "Wrong data read, should be a digit.");
            buf[i] = c;
            ++i;
        }
        buf[i + 1] = '\0';
        l = (size_t)strtol(buf, NULL, 10);
        const size_t total_size = l+1;
        data.data = calloc(1, total_size);
        check(fread(data.data, l, 1, fd) == 1, "Could not read from AOL file.");
        data.data[l] = '\0';
        data.dlen = l; /* Don't use total_size here because it's an off-by-1. */
    } else if (c == EOF) {
        data.dlen = 0;
        data.data = NULL;
    }

    return data;

error:
    free(data.data);
    return data;
}

int ol_aol_restore_from_file(ol_database *target_db,
        const char aol_fname[AOL_FILENAME_ALLOC],
        const unsigned char *values_data) {

    ol_string command = {0},
              key = {0},
              ct = {0},
              read_org_size = {0},
              read_data_size = {0},
              value = {0};

    FILE *fd = fopen(aol_fname, "r");
    check(fd, "Error opening file");

    while (!feof(fd)) {
        command = _ol_read_data(fd);

        /* Kind of a hack to check for EOF. If the struct is blank, then we
         * read past EOF in _ol_read_data. feof is rarely useful I guess... */
        if (command.data == NULL)
            break;

        key = _ol_read_data(fd);
        check(key.data, "Error reading"); /* Everything needs a key */

        if (strncmp(command.data, "JAR", 3) == 0) {
            ct = _ol_read_data(fd);
            check(ct.data, "Error reading");

            read_org_size = _ol_read_data(fd);
            check(read_org_size.data, "Error reading");

            read_data_size = _ol_read_data(fd);
            check(read_data_size.data, "Error reading");

            value = _ol_read_data(fd);
            check(value.data, "Error reading");

            size_t original_size = (size_t)strtol(read_org_size.data, NULL, 10);
            size_t compressed_size = (size_t)strtol(read_data_size.data, NULL, 10);
            size_t data_offset = (size_t)strtol(value.data, NULL, 10);

            /* Pointer in the values file to where the data for this command
             * should be. */
            const unsigned char *data_ptr = values_data + data_offset;

            /* Short circuit check to see if the memory in the location is all
             * null. */
            int memory_is_not_null = 0;
            int i = 0;
            for(;i < compressed_size; i++) {
                if ('\0' != data_ptr[i]) {
                    debug("Data is not null on %zu.", data_offset + i);
                    memory_is_not_null = 1;
                    break;
                }
            }

            if (memory_is_not_null) {
                /* Turns out that in rare cases LZ4 will compress to exactly
                 * the same size as it's starting string. This means we can't
                 * just check to see if original_size != compressed_size, so
                 * instead we first attempt to decompress and check how many
                 * chars were processed.
                 */
                char tmp_data[original_size];
                char *ret = memset(&tmp_data, 0, original_size);
                check(ret == tmp_data, "Could not initialize tmp_data parameter.");

                int processed = LZ4_decompress_fast((const char*)data_ptr, (char *)tmp_data, original_size);

                if (processed == compressed_size) {
                    ol_jar(target_db, key.data, key.dlen, (unsigned char*)tmp_data, original_size);
                } else {
                    if (original_size != compressed_size)
                        ol_log_msg(LOG_WARN, "Could not decompress data that is probably compressed. Data may have been deleted.");
                    /* Now that we've tried to decompress and failed, send off the raw data instead. */
                    ol_jar(target_db, key.data, key.dlen, data_ptr, compressed_size);
                }
            }
#ifdef DEBUG
            /* This happens a lot and isn't bad, so I'm commenting it out. */
            else {
                ol_log_msg(LOG_WARN, "No data in values file that corresponds with this key. Key has been deleted or updated.");
            }
#endif

            /* Important: Set the new offset to compressed_size + data_offset.
             * We need to do this because compaction/squishing will leave holes
             * in the data that we need to account for during replay.
             *
             * ...except when we're commiting a transaction. Then we assume whatever
             * is there is correct.
             */
            if (target_db->state != OL_S_COMMITTING)
                target_db->val_size = compressed_size + data_offset;
            /* TODO: What happens here if a bucket is reallocated? We don't
             * actually expand the extents, so in that case would we have a
             * bug?
             */

            free(read_org_size.data);
            free(read_data_size.data);
            free(ct.data);
            free(value.data);
        } else if (strncmp(command.data, "SCOOP", 5) == 0) {
            ol_scoop(target_db, key.data, key.dlen);
        } else if (strncmp(command.data, "SPOIL", 5) == 0) {
            ol_string spoil = _ol_read_data(fd);
            check(spoil.data, "Could not read the rest of SPOIL command for AOL.");

            struct tm time = {0};
            _deserialize_time(&time, spoil.data);

            ol_spoil(target_db, key.data, key.dlen, &time);
            free(spoil.data);
        }

        /* Strip the newline char after each "record" */
        char c;
        check(fread(&c, 1, 1, fd) != 0, "Error reading");
        check(c == '\n', "Could not strip newline");

        free(command.data);
        free(key.data);
    }
    fclose(fd);
    return 0;

error:
    ol_log_msg(LOG_ERR, "Restore failed. Corrupt AOL?");

    /* Free all the stuff */
    free(command.data);
    free(key.data);
    free(value.data);
    free(ct.data);
    free(read_org_size.data);
    free(read_data_size.data);
    if (fd != NULL) {
        fclose(fd);
    }

    return -1;
}

int ol_aol_restore(ol_database *db) {
    return ol_aol_restore_from_file(db, db->aol_file, db->values);
}
