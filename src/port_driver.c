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
*
* This is where the magic happens.
*/
#include <string.h>
#include "ei.h"
#include "erl_driver.h"
#include "logging.h"
#include "oleg.h"

/* Needed for R14B or earlier */
#if (ERL_DRV_EXTENDED_MAJOR_VERSION < 2)
#define ErlDrvSizeT int
#endif

/* This is used to store and manipulate state. */
typedef struct {
    ErlDrvPort port;
    ol_database *db;
} oleg_data;

/* This should match the object in olegdb.htl */
typedef struct {
    char database_name[DB_NAME_SIZE];
    char key[KEY_SIZE];
    int ct_len;
    int data_len;
    int version;
    char content_type[255];
    unsigned char *data;
} ol_record;

static ErlDrvData oleg_start(ErlDrvPort port, char *buff) {
    oleg_data *d = (oleg_data*)driver_alloc(sizeof(oleg_data));
    d->port = port;
    d->db = NULL;
    return (ErlDrvData)d;
}

static void oleg_stop(ErlDrvData data) {
    oleg_data *d = (oleg_data*)data;
    if (d->db != NULL) {
        ol_close(d->db);
    }
    driver_free(data);
}

/* Converts a string of binary data from erlang into something we can use */
/* Buf is what we're reading from, index is where to start. */
static ol_record *read_record(char *buf, int index) {
    ol_record *new_obj = driver_alloc(sizeof(ol_record));
    int arity = 0;
    char atom[64];
    long len = 0; /* Not really used, but ei_decode_binary wants it */
    int type = 0; /* Also not used. */
    int data_size = 0; /* Used to get the length of the data we'll be storing. */

    /* TODO: Error checking in here somewhere. */
    if (ei_decode_version(buf, &index, &new_obj->version))
        ol_log_msg(LOG_WARN, "Could not decode version.\n");
    /* Gives us how many items are in the tuple */
    if (ei_decode_tuple_header(buf, &index, &arity))
        ol_log_msg(LOG_WARN, "Could not decode tuple header.\n");

    if (arity != 6)
        ol_log_msg(LOG_WARN, "Arity was not as expected.\n");

    if (ei_decode_atom(buf, &index, atom))
        ol_log_msg(LOG_WARN, "Could not decode ol_record atom\n");
    if (ei_decode_binary(buf, &index, new_obj->database_name, &len))
        ol_log_msg(LOG_WARN, "Could not get database name.\n");
    if (ei_decode_binary(buf, &index, new_obj->key, &len))
        ol_log_msg(LOG_WARN, "Could not get key.\n");
    if (ei_decode_string(buf, &index, new_obj->content_type))
        ol_log_msg(LOG_WARN, "Could not get content-type.");
    if (ei_decode_long(buf, &index, (long*)&new_obj->ct_len))
        ol_log_msg(LOG_WARN, "Could not get ct_len.\n");

    /* This stuff is all to get the data. */
    ei_get_type(buf, &index, &type, &data_size);
    new_obj->data = NULL;
    if (data_size > 0) {
        new_obj->data = driver_alloc(data_size);
        if (ei_decode_binary(buf, &index, new_obj->data, &len))
            ol_log_msg(LOG_WARN, "Could not get data.\n");

    } else {
        ol_log_msg(LOG_WARN, "Could not get data.\n");
    }

    return new_obj;
}

static void oleg_output(ErlDrvData data, char *cmd, ErlDrvSizeT clen) {
    oleg_data *d = (oleg_data*)data;
    char fn = cmd[0];
    int res = 0;
    ol_record *obj = NULL;

    /* Turn Erlang into Oleg */
    obj = read_record(cmd, 1);

    /* Open up a db if we don't have on already */
    if (d->db == NULL)
        d->db = ol_open("/tmp", obj->database_name, OL_CONSUME_DIR);

    if (fn == 1) {
        /* ol_jar */
        res = ol_jar_ct(d->db, obj->key, obj->data, strlen((char*)obj->data),
               obj->content_type, strlen((char*)obj->content_type));
    } else if (fn == 2) {
        /* ol_unjar */
        /* TODO: Refactor when we know how big out data is. */
        /* Good enough for demo. */
        unsigned char *to_send = (unsigned char *)driver_alloc_binary(1024);
        unsigned char *data = ol_unjar(d->db, obj->key);
        memcpy(to_send, data, 1024);
        driver_output_binary(d->port, NULL, 0,
            (ErlDrvBinary*)to_send, 0, strlen((char *)to_send));
    }
    /* Send that shit back */
    driver_output(d->port, (char *)&res, 1);

    if (obj) {
        if (obj->data) driver_free(obj->data);
        driver_free(obj);
    }
}

/* Various callbacks */
ErlDrvEntry ol_driver_entry = {
    NULL,
    oleg_start,
    oleg_stop,
    oleg_output,
    NULL,
    NULL,
    "libolegserver",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL
};

DRIVER_INIT(libolegserver) {
    return &ol_driver_entry;
}
