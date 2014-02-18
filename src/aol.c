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

    if (strncmp(cmd, "JAR", 3) == 0) {
        ret = fprintf(db->aolfd, ":%zu:%s:%zu:%s:%zu:%s\n", strlen(cmd), cmd,
                strlen(bct->key), bct->key, bct->data_size, bct->data_ptr);
    } else if (strncmp(cmd, "SCOOP", 5) == 0) {
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
    }
    l = (size_t)strtol(buf, NULL, 10);
    data->data = calloc(1, l);
    fread(data->data, l, 1, fd);
    data->dlen = l;

    return data;
}

int ol_aol_restore(ol_database *db) {
    char c[1];
    FILE *fd;
    ol_string *command, *key, *value;
    fd = fopen(db->aol_file, "r");
    while (!feof(fd)) {
        command = _ol_read_data(fd);
        key = _ol_read_data(fd);
        ol_log_msg(LOG_INFO, "Command: %s", command->data);
        ol_log_msg(LOG_INFO, "Key: %s", key->data);

        if (strncmp(command->data, "JAR", 3) == 0) {
            value = _ol_read_data(fd);
            ol_log_msg(LOG_INFO, "Value: %s", value->data);
            ol_jar(db, key->data, key->dlen, (unsigned char*)value->data, value->dlen);
        } else if (strncmp(command->data, "SCOOP", 5) == 0) {
            ol_scoop(db, key->data, key->dlen);
        }

        /* Strip the newline char after each "record" */
        fread(c, 1, 1, fd);
        check(*c == '\n', "Could not strip newline");
    }
    return 0;

error:
    return -1;
}
