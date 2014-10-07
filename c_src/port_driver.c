/*
* This is where the magic happens.
*/
#include <string.h>
#include "ei.h"
#include "erl_driver.h"

#include "aol.h"
#include "errhandle.h"
#include "file.h"
#include "cursor.h"
#include "oleg.h"
#include "logging.h"
#include "tree.h"

/* Needed for R14B or earlier */
#if (ERL_DRV_EXTENDED_MAJOR_VERSION < 2)
#define ErlDrvSizeT int
#endif

/* This is used to store and manipulate state. */
typedef struct {
    ErlDrvPort port;
    ol_splay_tree *databases;
    char db_loc[255];
} oleg_data;

/* This should match the object in olegdb.htl */
typedef struct {
    char database_name[DB_NAME_SIZE];
    char *key;
    size_t klen;
    size_t data_len;
    int version;
    long expiration;
    unsigned char *data;
} ol_record;

static ErlDrvData oleg_start(ErlDrvPort port, char *buff) {
    oleg_data *d = (oleg_data*)driver_alloc(sizeof(oleg_data));
    d->port = port;
    d->databases = NULL;
    d->db_loc[0] = '\0';
    return (ErlDrvData)d;
}

static void oleg_stop(ErlDrvData data) {
    oleg_data *d = (oleg_data*)data;
    ol_splay_tree *tree = d->databases;

    ol_cursor cursor;
    check(olc_generic_init(tree, &cursor), "Could not init cursor.");
    while(olc_step(&cursor)) {
        const ol_splay_tree_node *node = _olc_get_node(&cursor);
        check(node != NULL, "Could not retrieve a node.");
        ol_database *db = (ol_database *)node->ref_obj;

        if (db != NULL)
            ol_close(db);
    }

error:
    /* Fall through to here. */
    driver_free(data);
    ols_close(tree);
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

    if (arity != 6)
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

static void port_driver_jar(oleg_data *d, ol_database *db, ol_record *obj) {
    int res = 0;
    res = ol_jar(db, obj->key, obj->klen, obj->data, obj->data_len);

    debug("Expiration: %li", obj->expiration);
    if (obj->expiration != -1) {
        struct tm new_expire;
        time_t passed_time = (time_t)obj->expiration;
        localtime_r(&passed_time, &new_expire);
        ol_spoil(db, obj->key, obj->klen, &new_expire);
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

static void port_driver_unjar(oleg_data *d, ol_database *db, ol_record *obj) {
    size_t val_size;
    /* TODO: Fix this when we have one clean function to retrieve content type
     * and data at the same time.
     */
    unsigned char *data = NULL;
    int ret = ol_unjar(db, obj->key, obj->klen, &data, &val_size);
    ei_x_buff to_send;
    ei_x_new_with_version(&to_send);
    if (ret == 0) {
        ei_x_encode_tuple_header(&to_send, 2);
        ei_x_encode_atom(&to_send, "ok");
        ei_x_encode_binary(&to_send, data, val_size);
    } else {
        ei_x_encode_atom(&to_send, "not_found");
    }
    driver_output(d->port, to_send.buff, to_send.index);
    ei_x_free(&to_send);
    free(data);
}

static void port_driver_scoop(oleg_data *d, ol_database *db, ol_record *obj) {
    int ret = ol_scoop(db, obj->key, obj->klen);
    ei_x_buff to_send;
    ei_x_new_with_version(&to_send);
    if (ret == 0)
        ei_x_encode_atom(&to_send, "ok");
    else
        ei_x_encode_atom(&to_send, "not_found");
    driver_output(d->port, to_send.buff, to_send.index);
    ei_x_free(&to_send);
}

static void port_driver_bucket_meta(oleg_data *d, ol_database *db, ol_record *obj) {
    int exists = ol_exists(db, obj->key, obj->klen);
    ei_x_buff to_send;
    ei_x_new_with_version(&to_send);
    if (exists == 0) {
        struct tm *time_retrieved = NULL;
        time_retrieved = ol_expiration_time(db, obj->key, obj->klen);

        /* If we have an expiration time, send it back with the content type. */
        if (time_retrieved != NULL) {
            ei_x_encode_tuple_header(&to_send, 3);
            ei_x_encode_atom(&to_send, "ok");
            ei_x_encode_long(&to_send, (long)db->rcrd_cnt);
            ei_x_encode_long(&to_send, (long)mktime(time_retrieved));
        } else {
            ei_x_encode_tuple_header(&to_send, 2);
            ei_x_encode_atom(&to_send, "ok");
            ei_x_encode_long(&to_send, (long)db->rcrd_cnt);
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

static void port_driver_error(oleg_data *d, const char *msg) {
    /* Send something back so we're not blocking. */
    ei_x_buff to_send;
    ei_x_new_with_version(&to_send);
    ei_x_encode_tuple_header(&to_send, 2);
    ei_x_encode_atom(&to_send, "error");
    ei_x_encode_binary(&to_send, msg, strlen(msg));
    driver_output(d->port, to_send.buff, to_send.index);
    ei_x_free(&to_send);
}

/* Common successful return methods used by _first, _last, _next and _prev. */
static inline void port_driver_cursor_response(oleg_data *d, const ol_bucket *bucket,
                    unsigned char *data, const size_t val_size) {

    ei_x_buff to_send;
    ei_x_new_with_version(&to_send);
    ei_x_encode_tuple_header(&to_send, 3);

    /* Send back ok, content type, key for bucket, and value for bucket */
    ei_x_encode_atom(&to_send, "ok");
    ei_x_encode_binary(&to_send, bucket->key, bucket->klen);
    ei_x_encode_binary(&to_send, data, val_size);

    driver_output(d->port, to_send.buff, to_send.index);

    ei_x_free(&to_send);
    free(data);
    return;
}

static void port_driver_cursor_next(oleg_data *d, ol_database *db, ol_record *obj) {
    char _key[KEY_SIZE] = {'\0'};
    size_t _klen = 0;

    ol_bucket *bucket = ol_get_bucket(db, obj->key, obj->klen, &_key, &_klen);

    if (bucket == NULL)
        return port_driver_not_found(d);

    ol_splay_tree_node *node = bucket->node;
    ol_splay_tree_node *maximum = ols_subtree_maximum(db->tree->root);

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

    /* Let ol_unjar handle decompression and whatever else for us: */
    int ret = ol_unjar(db, next_bucket->key, next_bucket->klen, &data, &val_size);
    if (ret == 0) {
        port_driver_cursor_response(d, next_bucket, data, val_size);
        return;
    }

    ol_log_msg(LOG_ERR, "Something went horribly wrong. We couldn't get the data of a bucket we just found in the tree.");
    port_driver_not_found(d);
    return;
}

static void port_driver_cursor_prev(oleg_data *d, ol_database *db, ol_record *obj) {
    char _key[KEY_SIZE] = {'\0'};
    size_t _klen = 0;

    ol_bucket *bucket = ol_get_bucket(db, obj->key, obj->klen, &_key, &_klen);

    if (bucket == NULL)
        return port_driver_not_found(d);

    ol_splay_tree_node *node = bucket->node;
    ol_splay_tree_node *minimum = ols_subtree_minimum(db->tree->root);

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

    /* Let ol_unjar handle decompression and whatever else for us: */
    int ret = ol_unjar(db, prev_bucket->key, prev_bucket->klen, &data, &val_size);
    if (ret == 0) {
        port_driver_cursor_response(d, prev_bucket, data, val_size);
        return;
    }

    ol_log_msg(LOG_ERR, "Something went horribly wrong. We couldn't get the data of a bucket we just found in the tree.");
    port_driver_not_found(d);
    return;
}

static void port_driver_cursor_first(oleg_data *d, ol_database *db, ol_record *obj) {
    ol_splay_tree_node *minimum = ols_subtree_minimum(db->tree->root);

    if (minimum == NULL) {
        return port_driver_not_found(d);
    }

    /* Found next node. */
    unsigned char *data = NULL;
    size_t val_size;

    ol_bucket *bucket = (ol_bucket *)minimum->ref_obj;

    /* Let ol_unjar handle decompression and whatever else for us: */
    int ret = ol_unjar(db, bucket->key, bucket->klen, &data, &val_size);
    if (ret == 0) {
        port_driver_cursor_response(d, bucket, data, val_size);
        return;
    }

    ol_log_msg(LOG_ERR, "Something went horribly wrong. We couldn't get the data of a bucket we just found in the tree.");
    port_driver_not_found(d);
    return;
}

static void port_driver_cursor_last(oleg_data *d, ol_database *db, ol_record *obj) {
    ol_splay_tree_node *maximum = ols_subtree_maximum(db->tree->root);

    if (maximum == NULL) {
        return port_driver_not_found(d);
    }

    /* Found next node. */
    unsigned char *data = NULL;
    size_t val_size;

    ol_bucket *bucket = (ol_bucket *)maximum->ref_obj;

    /* Let ol_unjar handle decompression and whatever else for us: */
    int ret = ol_unjar(db, bucket->key, bucket->klen, &data, &val_size);
    if (ret == 0) {
        port_driver_cursor_response(d, bucket, data, val_size);
        return;
    }

    ol_log_msg(LOG_ERR, "Something went horribly wrong. We couldn't get the data of a bucket we just found in the tree.");
    port_driver_not_found(d);
    return;
}

static void port_driver_prefix_match(oleg_data *d, ol_database *db, ol_record *obj) {
    ol_key_array matches_list = NULL;
    int match_num = ol_prefix_match(db, obj->key, obj->klen, &matches_list);
    if (match_num > 0) {
        ei_x_buff to_send;
        ei_x_new_with_version(&to_send);

        /* Response looks like this: */
        /* {ok, ["list", "of", "matches"]} */
        ei_x_encode_tuple_header(&to_send, 3);
        ei_x_encode_atom(&to_send, "ok");
        ei_x_encode_long(&to_send, (long)match_num);
        ei_x_encode_list_header(&to_send, match_num);

        int i = 0;
        for(; i < match_num; i++) {
            /* Matches should be null terminated. Unless somebody fucks with
             * something. */
            ei_x_encode_binary(&to_send, matches_list[i], strlen(matches_list[i]));
        }
        /* Apparently erlang is dumb and you need to end a list with an
         * empty one. */
        ei_x_encode_empty_list(&to_send);

        driver_output(d->port, to_send.buff, to_send.index);
        ei_x_free(&to_send);

        /* Free everything we just sent over the wire. */
        for (i = 0; i < match_num; i++) {
            free(matches_list[i]);
        }
        free(matches_list);

        return;
    }

    return port_driver_not_found(d);
}

static int port_driver_squish(oleg_data *d, ol_database *db) {
    if (db == NULL)
        return 0;

    return ol_squish(db);
}

static int port_driver_sync(oleg_data *d, ol_database *db) {
    if (db == NULL)
        return 0;

    return ol_sync(db);
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
        if (d->databases == NULL)
            return port_driver_error(d, "No databases initialized.");

        /* This is one of the more unique commands in that we don't
         * need a decoded obj. We aren't even given one. Squish everyone. */
        ol_cursor cursor;
        olc_generic_init(d->databases, &cursor);

        int ret = 1;
        while(olc_step(&cursor)) {
            const ol_splay_tree_node *node = _olc_get_node(&cursor);
            ol_database *db = (ol_database *)node->ref_obj;

            if (db != NULL)
                ret = ret && port_driver_squish(d, db);
        }

        if (ret) {
            ei_x_buff to_send;
            _gen_atom(&to_send, "ok");
            driver_output(d->port, to_send.buff, to_send.index);
            ei_x_free(&to_send);
        } else {
            port_driver_error(d, "Could not squish all databases.");
        }

        /* Don't do anything else. */
        return;
    } else if (fn == 11) {
        if (d->databases == NULL)
            return port_driver_error(d, "No databases initialized.");

        /* Similar to squish above. */
        ol_cursor cursor;
        olc_generic_init(d->databases, &cursor);

        int ret = 0;
        while(olc_step(&cursor)) {
            const ol_splay_tree_node *node = _olc_get_node(&cursor);
            ol_database *db = (ol_database *)node->ref_obj;

            if (db != NULL)
                ret = ret && port_driver_sync(d, db);
        }

        if (ret) {
            ei_x_buff to_send;
            _gen_atom(&to_send, "ok");
            driver_output(d->port, to_send.buff, to_send.index);
            ei_x_free(&to_send);
        } else {
            port_driver_error(d, "Could not fsync all databases.");
        }

        /* Don't do anything else. */
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
    if (d->databases == NULL) {
        ols_init(&(d->databases));
    }

    /* Find the database in the list of opened databases. */
    const size_t dbname_len = strnlen(obj->database_name, DB_NAME_SIZE);
    ol_splay_tree_node *found = ols_find(d->databases, obj->database_name, dbname_len);

    /* If we didn't find it, create it. */
    if (found == NULL && obj->database_name != NULL) {
        ol_database *db = NULL;
        db = ol_open(d->db_loc, obj->database_name, OL_F_APPENDONLY | OL_F_LZ4 | OL_F_SPLAYTREE);
        if (db == NULL)
            return port_driver_error(d, "Could not open database.");
        ol_splay_tree_node *node = ols_insert(d->databases, obj->database_name, dbname_len, db);
        found = node;
    } else if (obj->database_name == NULL) {
        return port_driver_error(d, "Something went wrong, trying to operate on a null database.");
    }

    ol_database *found_db = (ol_database *)found->ref_obj;

    /* Figure out what the frontend wants us to do */
    switch (fn) {
        case 1:
            port_driver_jar(d, found_db, obj);
            break;
        case 2:
            port_driver_unjar(d, found_db, obj);
            break;
        case 3:
            port_driver_scoop(d, found_db, obj);
            break;
        case 4:
            port_driver_bucket_meta(d, found_db, obj);
            break;
        case 5:
            port_driver_cursor_next(d, found_db, obj);
            break;
        case 6:
            port_driver_cursor_prev(d, found_db, obj);
            break;
        case 7:
            port_driver_cursor_first(d, found_db, obj);
            break;
        case 8:
            port_driver_cursor_last(d, found_db, obj);
            break;
        case 10:
            port_driver_prefix_match(d, found_db, obj);
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
