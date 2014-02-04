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
    if (ei_decode_string(buf, &index, new_obj->database_name))
        ol_log_msg(LOG_WARN, "Could not get database name.\n");
    if (ei_decode_string(buf, &index, new_obj->key))
        ol_log_msg(LOG_WARN, "Could not get key.\n");
    if (ei_decode_string(buf, &index, new_obj->content_type))
        ol_log_msg(LOG_WARN, "Could not get content-type.");
    if (ei_decode_long(buf, &index, (long*)&new_obj->ct_len))
        ol_log_msg(LOG_WARN, "Could not get ct_len.\n");

    return new_obj;
}

static void oleg_output(ErlDrvData data, char *cmd, ErlDrvSizeT clen) {
    oleg_data *d = (oleg_data*)data;

    char fn = cmd[0], *res = NULL;
    ol_record *obj = NULL;
    if (fn == 1) {
        /* ol_jar */
        obj = read_record(cmd, 1);
    } else if (fn == 2) {
        /* ol_unjar */
        obj = read_record(cmd, 1);
    }
    driver_output(d->port, res, 1);
    if (obj)
        driver_free(obj);
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
