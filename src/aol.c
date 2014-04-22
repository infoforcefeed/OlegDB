/* The MIT License (MIT)
* 
* Copyright (c) 2014 Quinlan Pfiffer, Kyle Terry
* 
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
* 
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
* 
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
* THE SOFTWARE.
*/
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

static inline void _deserialize_time(struct tm *time, char *buf) {
    /* Example 8601 datestamp: 2014-03-08T11:17:39Z */
    char year[4], month[2], day[2];
    char hour[2], min[2], sec[2];

    memcpy(&year, &buf[0], 4);
    memcpy(&month, &buf[5], 2);
    memcpy(&day, &buf[8], 2);

    memcpy(&hour, &buf[11], 2);
    memcpy(&min, &buf[14], 2);
    memcpy(&sec, &buf[17], 2);

    time->tm_year = atoi(year) - 1900;
    time->tm_mon = atoi(month) - 1;
    time->tm_mday = atoi(day);

    time->tm_hour = atoi(hour);
    time->tm_min = atoi(min);
    time->tm_sec = atoi(sec);
}

int ol_aol_write_cmd(ol_database *db, const char *cmd, ol_bucket *bct) {
    int ret;

    if (strncmp(cmd, "JAR", 3) == 0) {
        /* I'LL RIGOR YER MORTIS */
        debug("Writing: \"%.*s\"", (int)bct->klen, bct->key);
        char aol_str[] =
            ":%zu:%s:"      /* cmd length, cmd */
            "%zu:%.*s:"     /* klen size, key */
            "%zu:%.*s:"     /* ctype size, content_type */
            "%zu:%0*i:"     /* sizeof(original_size, original_size */
            "%zu:";         /* data size, to be followed by data */
        ret = fprintf(db->aolfd, aol_str,
                strlen(cmd), cmd,
                bct->klen, (int)bct->klen, bct->key,
                bct->ctype_size, (int)bct->ctype_size, bct->content_type,
                sizeof(size_t), (int)sizeof(size_t), bct->original_size,
                bct->data_size);
        check(ret > -1, "Error writing to file.");
        ret = fwrite(bct->data_ptr, bct->data_size, 1, db->aolfd);
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
    int c;
    int i;
    size_t l;
    char buf[20] = {0};
    ol_string *data = calloc(1, sizeof(ol_string));

    c = fgetc(fd);
    if (c == ':'){
        i = 0;
        while ((c = fgetc(fd)) != ':') {
            buf[i] = '\0';
            buf[i] = c;
            i++;
        }
        buf[i + 1] = '\0';
        l = (size_t)strtol(buf, NULL, 10);
        data->data = calloc(1, l);
        check(fread(data->data, l, 1, fd) != 0, "Error reading");
        data->dlen = l;
        return data;
    } else if (c == EOF) {
        data->dlen = 0;
        data->data = NULL;
        return data; /* A NULL ol_string means EOF was reached */
    }

    return NULL;

error:
    free(data->data);
    free(data);
    return NULL;
}

int ol_aol_restore(ol_database *db) {
    char c[1];
    FILE *fd;
    ol_string *command, *key, *value, *ct, *read_org_size;
    fd = fopen(db->aol_file, "r");
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

            value = _ol_read_data(fd);
            check(value, "Error reading");

            size_t original_size = (size_t)atoi(read_org_size->data);
            if (original_size != (size_t)value->dlen) {
                /* Data is compressed, gotta deal with that. */
                unsigned char tmp_data[original_size];
                void * ret = memset(&tmp_data, 0, original_size);
                check(ret != &tmp_data, "Could not initialize tmp_data parameter.");

                int processed = 0;
                processed = LZ4_decompress_fast((const char*)value->data,
                                                (char *)&tmp_data, original_size);
                check(processed != original_size, "Could not decompress data.");
                ol_jar_ct(db, key->data, key->dlen, tmp_data, original_size,
                        ct->data, ct->dlen);
            } else {
                /* Data is uncompressed, no need for trickery. */
                ol_jar_ct(db, key->data, key->dlen, (unsigned char*)value->data, value->dlen,
                        ct->data, ct->dlen);
            }
            free(read_org_size->data);
            free(read_org_size);
            free(value->data);
            free(value);
        } else if (strncmp(command->data, "SCOOP", 5) == 0) {
            ol_scoop(db, key->data, key->dlen);
        } else if (strncmp(command->data, "SPOIL", 5) == 0) {
            ol_string *spoil;
            spoil = _ol_read_data(fd);

            struct tm time;
            _deserialize_time(&time, spoil->data);

            check(spoil, "Error reading");
            ol_spoil(db, key->data, key->dlen, &time);
        }

        free(command->data);
        free(command);
        free(key->data);
        free(key);

        /* Strip the newline char after each "record" */
        check(fread(c, 1, 1, fd) != 0, "Error reading");
        check(*c == '\n', "Could not strip newline");
    }
    fclose(fd);
    return 0;

error:
    ol_log_msg(LOG_ERR, "Restore failed. Corrupt AOL?");
    return -1;
}
