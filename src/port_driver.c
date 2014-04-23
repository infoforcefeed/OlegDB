/*
* This is where the magic happens.
*/
#include <string.h>
#include "ei.h"
#include "erl_driver.h"

#include "aol.h"
#include "errhandle.h"
#include "oleg.h"
#include "logging.h"

/* Needed for R14B or earlier */
#if (ERL_DRV_EXTENDED_MAJOR_VERSION < 2)
#define ErlDrvSizeT int
#endif

/* This is used to store and manipulate state. */
typedef struct {
    ErlDrvPort port;
    ol_database *db;
    char db_loc[255];
} oleg_data;

/* This should match the object in olegdb.htl */
typedef struct {
    char database_name[DB_NAME_SIZE];
    char *key;
    size_t klen;
    size_t ct_len;
    size_t data_len;
    int version;
    long expiration;
    char content_type[255];
    unsigned char *data;
} ol_record;

static ErlDrvData oleg_start(ErlDrvPort port, char *buff) {
    oleg_data *d = (oleg_data*)driver_alloc(sizeof(oleg_data));
    d->port = port;
    d->db = NULL;
    d->db_loc[0] = '\0';
    return (ErlDrvData)d;
}

static void oleg_stop(ErlDrvData data) {
    oleg_data *d = (oleg_data*)data;
    if (d->db != NULL) {
        ol_close(d->db);
    }
    driver_free(data);
}

static void _gen_atom(ei_x_buff *to_send, const char *str) {
    ei_x_new_with_version(to_send);
    ei_x_encode_atom(to_send, str);
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

    if (arity != 7)
        ol_log_msg(LOG_WARN, "Arity was not as expected.\n");

    if (ei_decode_atom(buf, &index, atom))
        ol_log_msg(LOG_WARN, "Could not decode ol_record atom\n");
    if (ei_decode_binary(buf, &index, new_obj->database_name, &len))
        ol_log_msg(LOG_WARN, "Could not get database name.\n");
    new_obj->database_name[len] = '\0';

    if (ei_decode_long(buf, &index, &new_obj->expiration))
        ol_log_msg(LOG_WARN, "Could not get expiration.\n");

    int key_size = -1;
    int erl_type = -1;
    if (ei_get_type(buf, &index, &erl_type, &key_size))
        ol_log_msg(LOG_WARN, "Could not get key size.\n");
    new_obj->key = driver_alloc(key_size);

    if (ei_decode_binary(buf, &index, new_obj->key, &len))
        ol_log_msg(LOG_WARN, "Could not get key.\n");
    new_obj->klen = len;
    debug("Key: %s", new_obj->key);
    debug("get_type klen: %zu", new_obj->klen);

    if (ei_decode_binary(buf, &index, new_obj->content_type, &len))
        ol_log_msg(LOG_WARN, "Could not get content-type.");
    new_obj->ct_len = len;
    new_obj->content_type[len] = '\0';
    debug("Content type: %s", new_obj->content_type);

    /* This stuff is all to get the data. */
    ei_get_type(buf, &index, &type, &data_size);
    new_obj->data = NULL;
    if (data_size > 0) {
        new_obj->data = driver_alloc(data_size);
        if (ei_decode_binary(buf, &index, new_obj->data, &len))
            ol_log_msg(LOG_WARN, "Could not get data.\n");
        new_obj->data_len = len;
        debug("Data length: %zu", len);
    }

    return new_obj;
}

static void oleg_output(ErlDrvData data, char *cmd, ErlDrvSizeT clen) {
    oleg_data *d = (oleg_data*)data;
    int res = 0;
    int fn = cmd[0];
    ol_record *obj = NULL;

    debug("Command from server: %i", fn);
    if (fn == 0) {
        int tmp_index = 1;
        int version = 0;

        if (ei_decode_version(cmd, &tmp_index, &version))
            ol_log_msg(LOG_WARN, "Could not decode version.\n");

        if (ei_decode_string(cmd, &tmp_index, d->db_loc))
            ol_log_msg(LOG_WARN, "Could not get database location.\n");

        /* Send back 'ok' */
        ei_x_buff to_send;
        _gen_atom(&to_send, "ok");
        driver_output(d->port, to_send.buff, to_send.index);
        ei_x_free(&to_send);

        return;
    }

    /* Check to see if someone called ol_init */
    if (d->db_loc[0] == '\0') {
        ei_x_buff to_send;
        _gen_atom(&to_send, "no_db_location");
        driver_output(d->port, to_send.buff, to_send.index);
        ei_x_free(&to_send);
        return;
    }

    /* Turn Erlang into Oleg */
    obj = read_record(cmd, 1);

    /* Open up a db if we don't have on already */
    if (d->db == NULL) {
        ol_database *db;
        db = ol_open(d->db_loc, obj->database_name, OL_F_APPENDONLY | OL_F_LZ4 | OL_F_SPLAYTREE);
        d->db = db;
    }

    if (fn == 1) {
        /* ol_jar */
        res = ol_jar_ct(d->db, obj->key, obj->klen, obj->data, obj->data_len,
               obj->content_type, obj->ct_len);

        debug("Expiration: %li", obj->expiration);
        if (obj->expiration != -1) {
            struct tm new_expire;
            time_t passed_time = (time_t)obj->expiration;
            localtime_r(&passed_time, &new_expire);
            ol_spoil(d->db, obj->key, obj->klen, &new_expire);
        }
        /* TODO: Actually return useful info here. */
        ei_x_buff to_send;
        ei_x_new_with_version(&to_send);

        if (res != 0)
            _gen_atom(&to_send, "error");
        else
            _gen_atom(&to_send, "ok");

        driver_output(d->port, to_send.buff, to_send.index);
        ei_x_free(&to_send);
    } else if (fn == 2) {
        /* ol_unjar */
        size_t val_size;
        /* TODO: Fix this when we have one clean function to retrieve content type
         * and data at the same time.
         */
        unsigned char *data = NULL;
        int ret = ol_unjar_ds(d->db, obj->key, obj->klen, &data, &val_size);
        ei_x_buff to_send;
        ei_x_new_with_version(&to_send);
        if (ret == 0) {
            char *content_type_retrieved = ol_content_type(d->db, obj->key, obj->klen);
            ei_x_encode_tuple_header(&to_send, 3);
            ei_x_encode_atom(&to_send, "ok");
            ei_x_encode_binary(&to_send, content_type_retrieved, strlen(content_type_retrieved));
            ei_x_encode_binary(&to_send, data, val_size);
        } else {
            ei_x_encode_atom(&to_send, "not_found");
        }
        driver_output(d->port, to_send.buff, to_send.index);
        ei_x_free(&to_send);
        free(data);
    } else if (fn == 3) {
        /* ol_scoop */
        int ret = ol_scoop(d->db, obj->key, obj->klen);
        ei_x_buff to_send;
        ei_x_new_with_version(&to_send);
        if (ret == 0)
            ei_x_encode_atom(&to_send, "ok");
        else
            ei_x_encode_atom(&to_send, "not_found");
        driver_output(d->port, to_send.buff, to_send.index);
        ei_x_free(&to_send);
    } else if (fn == 4) {
        /* ol_bucket_meta */
        char *content_type_retrieved = ol_content_type(d->db, obj->key, obj->klen);
        ei_x_buff to_send;
        ei_x_new_with_version(&to_send);
        if (content_type_retrieved != NULL) {
            struct tm *time_retrieved = NULL;
            time_retrieved = ol_expiration_time(d->db, obj->key, obj->klen);

            /* If we have an expiration time, send it back with the content type. */
            if (time_retrieved != NULL) {
                ei_x_encode_tuple_header(&to_send, 4);
                ei_x_encode_atom(&to_send, "ok");
                ei_x_encode_binary(&to_send, content_type_retrieved, strlen(content_type_retrieved));
                ei_x_encode_long(&to_send, (long)d->db->rcrd_cnt);
                ei_x_encode_long(&to_send, (long)timelocal(time_retrieved));
            } else {
                ei_x_encode_tuple_header(&to_send, 3);
                ei_x_encode_atom(&to_send, "ok");
                ei_x_encode_binary(&to_send, content_type_retrieved, strlen(content_type_retrieved));
                ei_x_encode_long(&to_send, (long)d->db->rcrd_cnt);
            }
        } else {
            ei_x_encode_atom(&to_send, "not_found");
        }
        driver_output(d->port, to_send.buff, to_send.index);
        ei_x_free(&to_send);
    } else {
        /* Send something back so we're not blocking. */
        ei_x_buff to_send;
        _gen_atom(&to_send, "not_found");
        driver_output(d->port, to_send.buff, to_send.index);
        ei_x_free(&to_send);
    }

    if (obj) {
        if (obj->data) driver_free(obj->data);
        driver_free(obj);
    }
    driver_free(obj->key);
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
