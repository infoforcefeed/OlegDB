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
#include "oleg.h"
#include "logging.h"
#include "errhandle.h"

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>

int ol_aol_init(ol_database *db) {
    if (db->is_enabled(OL_F_APPENDONLY, &db->feature_set)) {
        debug("Opening append only log");
        debug("Append only log: %s", db->aol_file);
        db->aolfd = fopen(db->aol_file, "a+");
        check(db->aolfd != NULL, "Error opening append only file");
    }

    return 0;
error:
    return -1;
}

int ol_aol_fsync(int fd) {
    int ret;
    ret = fsync(fd);
    return ret;
}

int ol_aol_write_cmd(ol_database *db, const char *cmd, ol_bucket *bct) {
    int ret;

    if (strcmp(cmd, "JAR") == 0) {
        ret = fprintf(db->aolfd, ":%zu:%s:%zu:%s:%zu:%s\n", strlen(cmd), cmd,
                strlen(bct->key), bct->key, bct->data_size, bct->data_ptr);
    } else if (strcmp(cmd, "SCOOP") == 0) {
        ret = fprintf(db->aolfd, ":%zu:%s:%zu:%s\n", strlen(cmd), cmd,
                strlen(bct->key), bct->key);
    } else {
        ol_log_msg(LOG_ERR, "No such command '%s'", cmd);
        return -1;
    }

    check(ret > -1, "Error writing to file.");

    /* Force the OS to flush write to hardware */
    check(ol_aol_fsync(fileno(db->aolfd)) == 0, "Could not fsync. Panic!");
    return 0;
error:
    return -1;
}

struct ol_aol_data *_ol_read_data(FILE *stream) {
    int c, i, l;
    char tmp_buf[20];
    char *ret_buf;
    struct ol_aol_data *data = malloc(sizeof(struct ol_aol_data));
    check_mem(data);

    c = fgetc(stream);
    if (c == ':') {
        i = 0;
        for (c = fgetc(stream); c != ':'; c = fgetc(stream)) {
            tmp_buf[i] = c;
            i++;
        }
        tmp_buf[i+1] = '\0'; /* #yoyo */
        l = strtol(tmp_buf, NULL, 10);
        check(l > 0, "Data length was 0");
        ret_buf = malloc(l);
        check_mem(ret_buf);
        check(fread(ret_buf, l, 1, stream) > 0, "Error reading");
        data->len = l;
        data->data = ret_buf;
        return data;
    }
    return NULL;
error:
    return NULL;
}

int ol_aol_restore(ol_database *db) {
    char tmp_buf[1];
    struct ol_aol_data *command, *k, *v;

    ol_log_msg(LOG_INFO, "Restore DB from AOL file");
    if (!db->aolfd)
        db->aolfd = fopen(db->aol_file, "r");
    while (!feof(db->aolfd)) {
        command = _ol_read_data(db->aolfd);
        check(command != NULL, "Could not read command");
        k = _ol_read_data(db->aolfd);
        check(k != NULL, "Could not read key");
        if (strncmp(command->data, "JAR", command->len) == 0) {
            v = _ol_read_data(db->aolfd);
            check(v != NULL, "Could not read key");
            check(ol_jar(db, k->data, (unsigned char*)v->data, v->len) == 0,
                    "Could not jar!");
        } else if (strncmp(command->data, "SCOOP", command->len) == 0) {
            check(ol_scoop(db, k->data) == 0, "Could not scoop!");
        }
        check(fread(tmp_buf, 1, 1, db->aolfd) == 1, "Could not strip newline!");
        free(k);
        free(v);
        free(command);
    }
    return 0;

error:
    free(command->data);
    free(command);
    return -1;
}
