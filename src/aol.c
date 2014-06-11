#include "aol.h"
#include "data.h"
#include "oleg.h"
#include "logging.h"
#include "errhandle.h"
#include "lz4.h"

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <time.h>

int ol_aol_init(ol_database *db) {
    if (db->is_enabled(OL_F_APPENDONLY, &db->feature_set)) {
        debug("Opening append only log");
        debug("Append only log: %s", db->aol_file);
        db->aolfd = fopen(db->aol_file, "ab+");
        check(db->aolfd != NULL, "Error opening append only file");
    }

    return 0;
error:
    return -1;
}

int ol_aol_fsync(FILE *fd) {
    check(fflush(fd) == 0, "Could not fflush.");
    check(fsync(fileno(fd)) == 0, "Could not fsync");
    return 0;
error:
    return -1;
}

static inline void _serialize_time(struct tm *time, char *buf) {
    strftime(buf, 21, "%FT%TZ", time);
}

void _deserialize_time(struct tm *fillout, char *buf) {
    /* Example 8601 datestamp: 2014-03-08T11:17:39Z */
    char year[4]={0}, month[2]={0}, day[2]={0};
    char hour[2]={0}, min[2]={0}, sec[2]={0};

    memcpy(&year, &buf, 4);
    memcpy(&month, &buf[5], 2);
    memcpy(&day, &buf[8], 2);

    memcpy(&hour, &buf[11], 2);
    memcpy(&min, &buf[14], 2);
    memcpy(&sec, &buf[17], 2);

    memset(fillout, '\0', sizeof(struct tm));
    fillout->tm_year = strtol(year, NULL, 10) - 1900;
    fillout->tm_mon = strtol(month, NULL, 10) - 1;
    fillout->tm_mday = strtol(day, NULL, 10);

    fillout->tm_hour = strtol(hour, NULL, 10);
    fillout->tm_min = strtol(min, NULL, 10);
    fillout->tm_sec = strtol(sec, NULL, 10);
}

int ol_aol_write_cmd(ol_database *db, const char *cmd, ol_bucket *bct) {
    int ret;

    if (strncmp(cmd, "JAR", 3) == 0) {
        /* I'LL RIGOR YER MORTIS */
        debug("Writing: \"%.*s\"", (int)bct->klen, bct->key);
        char aol_str[] =
            ":%zu:%s"       /* cmd length, cmd */
            ":%zu:%.*s"     /* klen size, key */
            ":%zu:%.*s"     /* ctype size, content_type */
            ":%zu:%0*d"     /* sizeof(original_size), original_size */
            ":%zu:%0*d"     /* sizeof(size_t), data_size */
            ":%zu:%0*d";    /* sizeof(size_t), offset into file */
        ret = fprintf(db->aolfd, aol_str,
                strlen(cmd), cmd,
                bct->klen, (int)bct->klen, bct->key,
                bct->ctype_size, (int)bct->ctype_size, bct->content_type,
                sizeof(size_t), (int)sizeof(size_t), bct->original_size,
                sizeof(size_t), (int)sizeof(size_t), bct->data_size,
                sizeof(size_t), (int)sizeof(size_t), bct->data_offset);
        check(ret > -1, "Error writing to file.");
        ret = fprintf(db->aolfd, "\n");
    } else if (strncmp(cmd, "SCOOP", 5) == 0) {
        ret = fprintf(db->aolfd, ":%zu:%s:%zu:%s\n",
                strlen(cmd), cmd,
                bct->klen, bct->key);
        check(ret > -1, "Error writing to file.");
    } else if (strncmp(cmd, "SPOIL", 5) == 0) {
        char exptime[21] = {'\0'};
        _serialize_time(bct->expiration, exptime);

        ret = fprintf(db->aolfd, ":%zu:%s:%zu:%s:%zu:%*s\n",
                strlen(cmd), cmd,
                bct->klen, bct->key,
                strlen(exptime), 20, exptime);
        check(ret > -1, "Error writing to file.");
    } else {
        ol_log_msg(LOG_ERR, "No such command '%s'", cmd);
        return -1;
    }

    check(ret > -1, "Error writing to file.");

    /* Force the OS to flush write to hardware */
    check(ol_aol_fsync(db->aolfd) == 0, "Could not fsync. Panic!");
    return 0;
error:
    return -1;
}

ol_string *_ol_read_data(FILE *fd) {
    ol_string *data = calloc(1, sizeof(ol_string));

    int c = fgetc(fd);
    if (c == ':'){
        int i = 0;
        size_t l = 0;
        char buf[20] = {0};
        while ((c = fgetc(fd)) != ':') {
            buf[i] = c;
            ++i;
        }
        buf[i + 1] = '\0';
        l = (size_t)strtol(buf, NULL, 10);
        const size_t total_size = l+1;
        data->data = calloc(1, total_size);
        check(fread(data->data, l, 1, fd) == 1, "Could not read from AOL file.");
        data->data[l] = '\0';
        data->dlen = total_size;
        return data;
    } else if (c == EOF) {
        ol_log_msg(LOG_WARN, "_ol_read_data EOF");
        data->dlen = 0;
        data->data = NULL;
        return data; /* A NULL ol_string means EOF was reached */
    }

    free(data);
    return NULL;

error:
    ol_string_free(&data);
    return NULL;
}

int ol_aol_restore(ol_database *db) {
    ol_string *command = NULL,
              *key = NULL,
              *value = NULL,
              *ct = NULL,
              *read_data_size = NULL,
              *read_org_size = NULL;

    FILE *fd = fopen(db->aol_file, "r");
    check(fd, "Error opening file");
    while (!feof(fd)) {
        command = _ol_read_data(fd);
        check(command, "Error reading");

        /* Kind of a hack to check for EOF. If the struct is blank, then we
         * read past EOF in _ol_read_data. feof is rarely useful I guess... */
        if (command->data == NULL) {
            free(command);
            break;
        }

        key = _ol_read_data(fd);
        check(key, "Error reading"); /* Everything needs a key */

        if (strncmp(command->data, "JAR", 3) == 0) {
            ct = _ol_read_data(fd);
            check(ct, "Error reading");

            read_org_size = _ol_read_data(fd);
            check(read_org_size, "Error reading");

            read_data_size = _ol_read_data(fd);
            check(read_data_size, "Error reading");

            value = _ol_read_data(fd);
            check(value, "Error reading");

            size_t original_size = (size_t)strtol(read_org_size->data, NULL, 10);
            size_t compressed_size = (size_t)strtol(read_data_size->data, NULL, 10);
            size_t data_offset = (size_t)strtol(value->data, NULL, 10);

            unsigned char *data_ptr = db->values + data_offset;

            //Data is compressed
            if (original_size != compressed_size) {
                ol_log_msg(LOG_WARN, "DEBUG data is compressed @ %x", data_ptr);

                /* Data is compressed, gotta deal with that. */
                char *tmp_data = calloc(1, original_size);
                check(tmp_data != NULL, "Could not initialize tmp_data parameter.");

                int processed = LZ4_decompress_fast((const char*)data_ptr, tmp_data, original_size);
                check(processed == compressed_size, "Could not decompress data. Data may have been previously deleted. %d != %d", (int)processed, (int)compressed_size);

                ol_jar_ct(db, key->data, key->dlen, (unsigned char*)tmp_data, original_size, ct->data, ct->dlen);
                free(tmp_data);
            } else {
                /* Data is uncompressed, no need for trickery. */
                if (data_ptr[0] != '\0') {
                    ol_jar_ct(db, key->data, key->dlen, data_ptr, compressed_size, ct->data, ct->dlen);
                } else {
                    ol_log_msg(LOG_WARN, "No data in values file that corresponds with this key. Deleted?");
                }
            }
            ol_string_free(&read_org_size);
            ol_string_free(&read_data_size);
            ol_string_free(&ct);
            ol_string_free(&value);
        } else if (strncmp(command->data, "SCOOP", 5) == 0) {
            ol_scoop(db, key->data, key->dlen);
        } else if (strncmp(command->data, "SPOIL", 5) == 0) {
            ol_string *spoil = _ol_read_data(fd);
            check(spoil != NULL, "Could not read the rest of SPOIL command for AOL.");

            struct tm time = {0};
            _deserialize_time(&time, spoil->data);

            check(spoil, "Error reading");
            ol_spoil(db, key->data, key->dlen, &time);
            ol_string_free(&spoil);
        }

        /* Strip the newline char after each "record" */
        char c;
        check(fread(&c, 1, 1, fd) != 0, "Error reading");
        check(c == '\n', "Could not strip newline");

        ol_string_free(&command);
        ol_string_free(&key);
    }
    fclose(fd);
    return 0;

error:
    ol_log_msg(LOG_ERR, "Restore failed. Corrupt AOL?");

    /* Free all the stuff */
    ol_string_free(&command);
    ol_string_free(&key);
    ol_string_free(&value);
    ol_string_free(&ct);
    ol_string_free(&read_org_size);
    ol_string_free(&read_data_size);
    if (fd != NULL) {
        fclose(fd);
    }

    return -1;
}
