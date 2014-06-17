/*
* This is where the magic happens.
*/
#include <string.h>
#include "ei.h"
#include "erl_driver.h"

#include "aol.h"
#include "errhandle.h"
#include "cursor.h"
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

static void port_driver_init(oleg_data *d, char *cmd) {
    /* ol_init */
    int tmp_index = 1;
    int version = 0;

    if (ei_decode_version(cmd, &tmp_index, &version))
        ol_log_msg(LOG_WARN, "Could not decode version.\n");

    /* Decode passed string into our persistent data structure: */
    if (ei_decode_string(cmd, &tmp_index, d->db_loc))
        ol_log_msg(LOG_WARN, "Could not get database location.\n");

    /* Send back 'ok' */
    ei_x_buff to_send;
    _gen_atom(&to_send, "ok");
    driver_output(d->port, to_send.buff, to_send.index);
    ei_x_free(&to_send);
}

static void port_driver_jar(oleg_data *d, ol_record *obj) {
    int res = 0;
    res = ol_jar_ct(d->db, obj->key, obj->klen, obj->data, obj->data_len,
           obj->content_type, obj->ct_len);

    debug("Expiration: %li", obj->expiration);
    if (obj->expiration != -1) {
        struct tm new_expire;
        time_t passed_time = (time_t)obj->expiration;
        localtime_r(&passed_time, &new_expire);
        ol_spoil(d->db, obj->key, obj->klen, &new_expire);
    }
    ei_x_buff to_send;
    ei_x_new_with_version(&to_send);

    if (res != 0)
        _gen_atom(&to_send, "error");
    else
        _gen_atom(&to_send, "ok");

    driver_output(d->port, to_send.buff, to_send.index);
    ei_x_free(&to_send);
}

static void port_driver_unjar(oleg_data *d, ol_record *obj) {
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
}

static void port_driver_scoop(oleg_data *d, ol_record *obj) {
    int ret = ol_scoop(d->db, obj->key, obj->klen);
    ei_x_buff to_send;
    ei_x_new_with_version(&to_send);
    if (ret == 0)
        ei_x_encode_atom(&to_send, "ok");
    else
        ei_x_encode_atom(&to_send, "not_found");
    driver_output(d->port, to_send.buff, to_send.index);
    ei_x_free(&to_send);
}

static void port_driver_bucket_meta(oleg_data *d, ol_record *obj) {
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
}

static void port_driver_not_found(oleg_data *d) {
    /* Send something back so we're not blocking. */
    ei_x_buff to_send;
    _gen_atom(&to_send, "not_found");
    driver_output(d->port, to_send.buff, to_send.index);
    ei_x_free(&to_send);
}

static void port_driver_error(oleg_data *d) {
    /* Send something back so we're not blocking. */
    ei_x_buff to_send;
    _gen_atom(&to_send, "error");
    driver_output(d->port, to_send.buff, to_send.index);
    ei_x_free(&to_send);
}

/* Common successful return methods used by _first, _last, _next and _prev. */
static inline void port_driver_cursor_response(oleg_data *d, const ol_bucket *bucket,
                    unsigned char *data, const size_t val_size) {

    ei_x_buff to_send;
    ei_x_new_with_version(&to_send);
    ei_x_encode_tuple_header(&to_send, 4);

    /* Send back ok, content type, key for bucket, and value for bucket */
    ei_x_encode_atom(&to_send, "ok");
    ei_x_encode_binary(&to_send, bucket->content_type, bucket->ctype_size);
    ei_x_encode_binary(&to_send, bucket->key, bucket->klen);
    ei_x_encode_binary(&to_send, data, val_size);

    driver_output(d->port, to_send.buff, to_send.index);

    ei_x_free(&to_send);
    free(data);
    return;
}

static void port_driver_cursor_next(oleg_data *d, ol_record *obj) {
    char _key[KEY_SIZE] = {'\0'};
    size_t _klen = 0;

    ol_bucket *bucket = ol_get_bucket(d->db, obj->key, obj->klen, &_key, &_klen);

    if (bucket == NULL)
        return port_driver_not_found(d);

    ol_splay_tree_node *node = bucket->node;
    ol_splay_tree_node *maximum = ols_subtree_maximum(d->db->tree->root);

    /* Get the next successor to this node. */
    if (!_olc_next(&node, maximum) || node == bucket->node) {
        /* Could not find next node. */
        port_driver_not_found(d);
        return;
    }

    /* Found next node. */
    unsigned char *data = NULL;
    size_t val_size;

    ol_bucket *next_bucket = (ol_bucket *)node->ref_obj;

    /* Let ol_unjar_ds handle decompression and whatever else for us: */
    int ret = ol_unjar_ds(d->db, next_bucket->key, next_bucket->klen, &data, &val_size);
    if (ret == 0) {
        port_driver_cursor_response(d, next_bucket, data, val_size);
        return;
    }

    ol_log_msg(LOG_ERR, "Something went horribly wrong. We couldn't get the data of a bucket we just found in the tree.");
    port_driver_not_found(d);
    return;
}

static void port_driver_cursor_prev(oleg_data *d, ol_record *obj) {
    char _key[KEY_SIZE] = {'\0'};
    size_t _klen = 0;

    ol_bucket *bucket = ol_get_bucket(d->db, obj->key, obj->klen, &_key, &_klen);

    if (bucket == NULL)
        return port_driver_not_found(d);

    ol_splay_tree_node *node = bucket->node;
    ol_splay_tree_node *minimum = ols_subtree_minimum(d->db->tree->root);

    /* Get the next successor to this node. */
    if (!_olc_prev(&node, minimum) || node == bucket->node) {
        /* Could not find next node. */
        port_driver_not_found(d);
        return;
    }

    /* Found next node. */
    unsigned char *data = NULL;
    size_t val_size;

    ol_bucket *prev_bucket = (ol_bucket *)node->ref_obj;

    /* Let ol_unjar_ds handle decompression and whatever else for us: */
    int ret = ol_unjar_ds(d->db, prev_bucket->key, prev_bucket->klen, &data, &val_size);
    if (ret == 0) {
        port_driver_cursor_response(d, prev_bucket, data, val_size);
        return;
    }

    ol_log_msg(LOG_ERR, "Something went horribly wrong. We couldn't get the data of a bucket we just found in the tree.");
    port_driver_not_found(d);
    return;
}

static void port_driver_cursor_first(oleg_data *d, ol_record *obj) {
    ol_splay_tree_node *minimum = ols_subtree_minimum(d->db->tree->root);

    if (minimum == NULL) {
        return port_driver_not_found(d);
    }

    /* Found next node. */
    unsigned char *data = NULL;
    size_t val_size;

    ol_bucket *bucket = (ol_bucket *)minimum->ref_obj;

    /* Let ol_unjar_ds handle decompression and whatever else for us: */
    int ret = ol_unjar_ds(d->db, bucket->key, bucket->klen, &data, &val_size);
    if (ret == 0) {
        port_driver_cursor_response(d, bucket, data, val_size);
        return;
    }

    ol_log_msg(LOG_ERR, "Something went horribly wrong. We couldn't get the data of a bucket we just found in the tree.");
    port_driver_not_found(d);
    return;
}

static void port_driver_cursor_last(oleg_data *d, ol_record *obj) {
    ol_splay_tree_node *maximum = ols_subtree_maximum(d->db->tree->root);

    if (maximum == NULL) {
        return port_driver_not_found(d);
    }

    /* Found next node. */
    unsigned char *data = NULL;
    size_t val_size;

    ol_bucket *bucket = (ol_bucket *)maximum->ref_obj;

    /* Let ol_unjar_ds handle decompression and whatever else for us: */
    int ret = ol_unjar_ds(d->db, bucket->key, bucket->klen, &data, &val_size);
    if (ret == 0) {
        port_driver_cursor_response(d, bucket, data, val_size);
        return;
    }

    ol_log_msg(LOG_ERR, "Something went horribly wrong. We couldn't get the data of a bucket we just found in the tree.");
    port_driver_not_found(d);
    return;
}

static void port_driver_smoosh(oleg_data *d) {
    if (d->db == NULL)
        return port_driver_error(d);

    const int ret = ol_smoosh(d->db);

    if (ret) {
        ei_x_buff to_send;
        _gen_atom(&to_send, "ok");
        driver_output(d->port, to_send.buff, to_send.index);
        ei_x_free(&to_send);
        return;
    }
    return port_driver_error(d);
}

/* So this is where all the magic happens. If you want to know how we switch
 * on different commands, go look at ol_database:encode/1.
 */
static void oleg_output(ErlDrvData data, char *cmd, ErlDrvSizeT clen) {
    oleg_data *d = (oleg_data*)data;
    int fn = cmd[0];
    ol_record *obj = NULL;

    debug("Command from server: %i", fn);
    if (fn == 0) {
        return port_driver_init(d, cmd);
    } else if (fn == 9) {
        /* This is one of the more unique commands in that we don't
         * need a decoded obj and in-fact aren't given one. */
        return port_driver_smoosh(d);
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
        db = ol_open(d->db_loc, obj->database_name, OL_F_APPENDONLY | OL_F_AOL_FFLUSH | OL_F_LZ4 | OL_F_SPLAYTREE);
        if (db == NULL)
            return port_driver_error(d);
        d->db = db;
    }

    /* Figure out what the frontend wants us to do */
    switch (fn) {
        case 1:
            port_driver_jar(d, obj);
            break;
        case 2:
            port_driver_unjar(d, obj);
            break;
        case 3:
            port_driver_scoop(d, obj);
            break;
        case 4:
            port_driver_bucket_meta(d, obj);
            break;
        case 5:
            port_driver_cursor_next(d, obj);
            break;
        case 6:
            port_driver_cursor_prev(d, obj);
            break;
        case 7:
            port_driver_cursor_first(d, obj);
            break;
        case 8:
            port_driver_cursor_last(d, obj);
            break;
        default:
            port_driver_not_found(d);
    }

    if (obj) {
        if (obj->data) driver_free(obj->data);
        driver_free(obj);
        driver_free(obj->key);
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
