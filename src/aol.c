#include "aol.h"
#include "data.h"
#include "oleg.h"
#include "logging.h"
#include "errhandle.h"

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
    char year[4]={0}, month[2]={0}, day[2]={0};
    char hour[2]={0}, min[2]={0}, sec[2]={0};

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
        ret = fprintf(db->aolfd, ":%zu:%s:%zu:%.*s:%zu:%.*s:%zu:",
                strlen(cmd), cmd,
                bct->klen, (int)bct->klen, bct->key,
                bct->ctype_size, (int)bct->ctype_size, bct->content_type,
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
    ol_string *command, *k, *v, *ct;
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

        k = _ol_read_data(fd);
        check(k, "Error reading"); /* Everything needs a key */

        if (strncmp(command->data, "JAR", 3) == 0) {
            ct = _ol_read_data(fd);
            check(ct, "Error reading");
            v = _ol_read_data(fd);
            check(v, "Error reading");
            ol_jar_ct(db, k->data, k->dlen, (unsigned char*)v->data, v->dlen,
                    ct->data, ct->dlen);
            free(ct->data);
            free(ct);
            free(v->data);
            free(v);
        } else if (strncmp(command->data, "SCOOP", 5) == 0) {
            ol_scoop(db, k->data, k->dlen);
        } else if (strncmp(command->data, "SPOIL", 5) == 0) {
            ol_string *spoil;
            spoil = _ol_read_data(fd);

            struct tm time = {0};
            _deserialize_time(&time, spoil->data);

            check(spoil, "Error reading");
            ol_spoil(db, k->data, k->dlen, &time);
            free(spoil->data);
            free(spoil);
        }

        free(command->data);
        free(command);
        free(k->data);
        free(k);

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
