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
#include "erl_driver.h"
#include "oleg.h"

/* This is used to store and manipulate state. */
typedef struct {
    ErlDrvPort port,
    ol_database *db;
} oleg_data;

static ErlDrvData oleg_start(ErlDrvPort port, char *buff) {
    oleg_data *data = (oleg_data*)driver_alloc(sizeof(oleg_data));
    data->port = port;
    data->db = NULL;
    return (ErlDrvData)d;
}

static void oleg_stop(ErlDrvData data) {
    oleg_data *data = (oleg_data*)data;
    if (data->db != NULL) {
        ol_close(data->db);
    }
    driver_free((char *)data);
}

static void oleg_output(ErlDrvData data, char *cmd, ErlDrvSizeT clen) {
    printf("Hello, world!\n");
}

/* Various callbacks */
ErlDrvEntry ol_driver_entry = {
    NULL,
    oleg_start,
    oleg_stop,
    oleg_output,
    NULL,
    NULL,
    "liboleg",
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

DRIVER_INIT(liboleg) {
    return &ol_driver_entry;
}
