/* Unit tests. */
#include <stdlib.h>
#include <unistd.h>
#include "aol.h"
#include "cursor.h"
#include "errhandle.h"
#include "file.h"
#include "logging.h"
#include "oleg.h"
#include "test.h"
#include "tree.h"
#include "transaction.h"

#define DB_DEFAULT_FEATURES OL_F_SPLAYTREE | OL_F_LZ4

/* Helper function to open databases, so we don't have to change API code
 * in a million places when we modify it.
 */
ol_database *_test_db_open(const ol_feature_flags features) {
    char template[] = "/tmp/oleg-XXXXXX";
    char *DB_PATH = mkdtemp(template);
    ol_log_msg(LOG_INFO, "Opening %s", DB_PATH);
    if (DB_PATH == NULL) {
        ol_log_msg(LOG_ERR, "Can't create unique directory");
        return NULL;
    }
    chmod(DB_PATH, 0755);
    ol_database *db = ol_open(DB_PATH, DB_NAME, features);
    if (db != NULL) {
        ol_log_msg(LOG_INFO, "Opened DB: %p.", db);
    } else {
        ol_log_msg(LOG_ERR, "Could not open database.");
    }
    return db;
}

static int _test_db_close(ol_database *db) {
    if (!db)
        return -1;

    char values_filename[DB_NAME_SIZE] = { 0 };
    db->get_db_file_name(db, VALUES_FILENAME, values_filename);

    char aol_filename[DB_NAME_SIZE] = { 0 };
    strncpy(aol_filename, db->aol_file, DB_NAME_SIZE);
    int should_delete_aol = db->is_enabled(OL_F_APPENDONLY, &db->feature_set);

    char DB_PATH[DB_NAME_SIZE] = {0};
    strncpy(DB_PATH, db->path, DB_NAME_SIZE);

    int ret = ol_close(db);

    ol_log_msg(LOG_INFO, "Unlinking %s", values_filename);
    unlink(values_filename);

    if (should_delete_aol) {
        ol_log_msg(LOG_INFO, "Unlinking %s", aol_filename);
        unlink(aol_filename);
    }

    char tx_dir_path[PATH_LENGTH] = {0};
    snprintf(tx_dir_path, PATH_LENGTH, "%s/%s", DB_PATH, "tx");

    struct stat st = {0};
    /* Check to see if the DB exists */
    if (_ol_get_stat(tx_dir_path, &st) || S_ISDIR(st.st_mode)) {
        rmdir(tx_dir_path);
    }
    rmdir(DB_PATH);

    return ret;
}

int test_open_close(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    int ret = _test_db_close(db);
    if (ret > 0){
        ol_log_msg(LOG_INFO, "Couldn't free all memory.");
    } else {
        ol_log_msg(LOG_INFO, "Closed DB.");
    }
    return 0;
}

int test_bucket_max(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    int expected_bucket_max = HASH_MALLOC / 8;

    ol_log_msg(LOG_INFO, "Expected max is: %i", expected_bucket_max);
    int generated_bucket_max = ol_ht_bucket_max(db->cur_ht_size);
    if (expected_bucket_max != generated_bucket_max) {
        ol_log_msg(LOG_INFO, "Unexpected bucket max. Got: %d", generated_bucket_max);
        _test_db_close(db);
        return 1;
    }
    ol_log_msg(LOG_INFO, "Generated max is: %i", expected_bucket_max);
    _test_db_close(db);
    return 0;
}

int test_zero_length_keys(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    char *key1 = NULL;
    char key2[] = "";
    unsigned char value[] = "lkjasldkjflakjsdf";

    ol_transaction *tx = olt_begin(db);
    check(tx != NULL, "Could not begin transaction.");
    check(olt_jar(tx, key1, 0, value, strlen((char *)value)) != 0, "jar'd key when we shouldn't have.");
    check(olt_jar(tx, key2, 0, value, strlen((char *)value)) != 0, "jar'd key when we shouldn't have.");
    check(olt_unjar(tx, key2, 0, NULL, NULL) != 0, "unjar'd key when we shouldn't have.");
    check(olt_spoil(tx, key1, 0, NULL) != 0, "spoil'd key when we shouldn't have.");
    check(olt_spoil(tx, key2, 0, NULL) != 0, "spoil'd key when we shouldn't have.");
    check(olt_scoop(tx, key1, 0) != 0, "scoop'd key when we shouldn't have.");
    check(olt_scoop(tx, key2, 0) != 0, "scoop'd key when we shouldn't have.");
    check(olt_commit(tx) == 0, "Could not commit transaction.");

    _test_db_close(db);
    return 0;

error:
    olt_abort(tx);
    _test_db_close(db);
    return 1;
}

int test_basic_transaction(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    ol_transaction *tx = NULL;
    char key[] = "rampant destruction";
    unsigned char value[] = "The Churning Black Waters";
    size_t vsize = strlen((char*)value);

    check(ol_jar(db, key, strnlen(key, KEY_SIZE), value, vsize) == 0, "Could not jar key.");
    tx = olt_begin(db);

    check(tx != NULL, "Could not begin transaction.");
    check(olt_commit(tx) == 0, "Could not commit transaction.");

    _test_db_close(db);
    return 0;
error:
    olt_abort(tx);
    _test_db_close(db);
    return 1;
}

int test_transaction_row_locking(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    ol_transaction *tx = NULL;
    char key[] = "rampant destruction";
    unsigned char value[] = "The Churning Black Waters";
    size_t vsize = strlen((char*)value);

    check(ol_jar(db, key, strnlen(key, KEY_SIZE), value, vsize) == 0, "Could not jar key.");
    tx = olt_begin(db);

    
}

int test_cas(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    char key[] = "menopause";

    unsigned char value[] = "<p>The jackal wishes you a merry christmas.</p>";
    size_t vsize = strlen((char *)value);

    unsigned char new_value[] = "<p>The jackal does not wish upon you anything at this time.</p>";
    size_t nvsize = strlen((char *)new_value);

    check(ol_jar(db, key, strnlen(key, KEY_SIZE), value, vsize) == 0, "Could not jar key.");

    check(ol_cas(db, key, strnlen(key, KEY_SIZE), new_value, nvsize, value, vsize) == 0, "Could not CAS.");
    check(ol_cas(db, key, strnlen(key, KEY_SIZE), value, vsize, value, vsize) != 0, "CAS's when we shouldn't have.");

    _test_db_close(db);
    return 0;

error:
    _test_db_close(db);
    return 1;
}

int test_jar(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    ol_transaction *tx = olt_begin(db);
    check(tx != NULL, "Could not beign transaction.");

    int i;
    int max_records = RECORD_COUNT;
    unsigned char to_insert[] = "123456789";
    for (i = 0; i < max_records; i++) {
        char key[64] = "crazy hash";
        char append[10] = "";

        sprintf(append, "%i", i);
        strcat(key, append);

        size_t len = strlen((char *)to_insert);
        int insert_result = olt_jar(tx, key, strlen(key), to_insert, len);

        check(insert_result == 0, "Could not insert.");
        check(tx->transaction_db->rcrd_cnt == i + 1, "Record count is not higher.");
    }
    ol_log_msg(LOG_INFO, "Records inserted: %i.", db->rcrd_cnt);
    ol_log_msg(LOG_INFO, "Saw %d collisions.", db->meta->key_collisions);

    olt_commit(tx);
    check(_test_db_close(db) == 0, "Could not close DB.");
    return 0;

error:
    olt_abort(tx);
    _test_db_close(db);
    return 1;
}

int test_can_jump_cursor(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    ol_transaction *tx = olt_begin(db);
    unsigned char *r_val = NULL;
    check(tx != NULL, "Could not begin transaction.");

    int max_records = 10;
    unsigned char to_insert[] = "roadkill";
    int i;
    for (i = 0; i < max_records; i++) {
        char key[64] = "mmmmmars";
        char append[10] = "";

        sprintf(append, "%i", i);
        strcat(key, append);

        size_t len = strlen((char *)to_insert);
        size_t klen = strlen(key);
        int insert_result = olt_jar(tx, key, klen, to_insert, len);

        check(insert_result == 0, "Coult not insert.");
        check(tx->transaction_db->rcrd_cnt == i + 1, "Record count is not higher.");
    }
    olt_commit(tx);

    /* Create and jump the cursor to a random hash. */
    const char key[] = "mmmmmars6";
    ol_cursor cursor;
    check(olc_init(db, &cursor), "Could not init cursor.");
    check(olc_jump(&cursor, key, strlen(key)) == 0, "Could not jump cursor.");

    /* Get the node from the cursors now jumped position */
    const ol_splay_tree_node *node = _olc_get_node(&cursor);
    check(node != NULL, "Could not retrieve node.");

    /* Prep some variables so we can check them */
    char r_key[KEY_SIZE] = {'0'};
    size_t r_vsize;

    const int ret = olc_get(&cursor, &r_key, &r_val, &r_vsize);
    check(ret == 0, "Could not retrieve key and value from cursor.");
    free(r_val);

    check(strncmp(r_key, key, KEY_SIZE) == 0, "Returned key is not the same.");

    check(_test_db_close(db) == 0, "Could not close database.");

    return 0;

error:
    olt_abort(tx);
    _test_db_close(db);
    if (r_val != NULL)
        free(r_val);
    return 1;

}

int test_can_find_all_nodes(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    ol_transaction *tx = olt_begin(db);
    check(tx != NULL, "Could not begin transaction.");

    int i;
    int max_records = RECORD_COUNT;
    unsigned char to_insert[] = "123456789";
    for (i = 0; i < max_records; i++) {
        char key[64] = "crazy hash";
        char append[10] = "";

        sprintf(append, "%i", i);
        strcat(key, append);

        size_t len = strlen((char *)to_insert);
        size_t klen = strlen(key);
        int insert_result = olt_jar(tx, key, klen, to_insert, len);

        check(insert_result == 0, "Coult not insert.");
        check(tx->transaction_db->rcrd_cnt = i + 1, "Record count is not higher.");

        ol_splay_tree_node *found = ols_find(tx->transaction_db->tree, key, klen);
        check(found != NULL, "Could not find key we just inserted.");
        check(found->klen == klen, "key length for inserted key in wrong.");
        check(strncmp(found->key, key, klen) == 0, "Incorrect key returned.");

    }
    olt_commit(tx);

    ol_log_msg(LOG_INFO, "Records inserted: %i.", db->rcrd_cnt);
    ol_log_msg(LOG_INFO, "Saw %d collisions.", db->meta->key_collisions);
    ol_log_msg(LOG_INFO, "Now searching for all keys.");

    check(_test_db_close(db) == 0, "Could not free all memory.");
    return 0;

error:
    olt_abort(tx);
    _test_db_close(db);
    return 1;
}
int test_lots_of_deletes(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    ol_transaction *tx = olt_begin(db);
    check(tx != NULL, "Could not begin transaction.");

    int i;
    int max_records = RECORD_COUNT;
    unsigned char to_insert[] = "123456789AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
    for (i = 0; i < max_records; i++) {
        char key[KEY_SIZE] = "A";
        char append[32] = "";

        sprintf(append, "%i", i);
        strcat(key, append);

        size_t len = strlen((char *)to_insert);
        int insert_result = olt_jar(tx, key, strlen(key), to_insert, len);

        check(insert_result == 0, "Coult not insert.");
        check(tx->transaction_db->rcrd_cnt == i + 1, "Record count is not higher.");
    }
    ol_log_msg(LOG_INFO, "Records inserted: %i.", db->rcrd_cnt);
    ol_log_msg(LOG_INFO, "Saw %d collisions.", db->meta->key_collisions);

    for (i = 0; i < max_records; i++) {
        char key[KEY_SIZE] = "A";
        char append[32] = "";

        sprintf(append, "%i", i);
        strcat(key, append);

        int delete_result = olt_scoop(tx, key, strlen(key));

        check(delete_result == 0, "Could not delete.");
        check(tx->transaction_db->rcrd_cnt == max_records - i - 1, "Record count is not lower.");
    }

    check(olt_commit(tx) == 0, "Could not commit transaction.");;
    _test_db_close(db);
    return 0;

error:
    olt_abort(tx);
    _test_db_close(db);
    return 1;
}

int test_unjar_ds(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    ol_transaction *tx = olt_begin(db);
    unsigned char *item = NULL;
    check(tx != NULL, "Could not begin transaction.");

    char key[64] = "FANCY KEY IS YO MAMA";
    unsigned char val[] = "invariable variables invariably trip up programmers";
    size_t val_len = strlen((char*)val);
    int inserted = olt_jar(tx, key, strlen(key), val, val_len);

    check(inserted == 0, "Could not insert.");

    size_t to_test;
    olt_unjar(tx, key, strlen(key), &item, &to_test);
    ol_log_msg(LOG_INFO, "Retrieved value.");
    check(item != NULL, "Coult not find key.");

    check(memcmp(item, val, strlen((char*)val)) == 0, "Returned value was not the same.");
    check(to_test == val_len, "Sizes were not the same.");

    check(olt_commit(tx) == 0, "Could not commit transaction.");;
    _test_db_close(db);
    free(item);
    return 0;

error:
    olt_abort(tx);
    _test_db_close(db);
    free(item);
    return 1;
}
int test_unjar(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    ol_transaction *tx = olt_begin(db);
    unsigned char *item = NULL;
    check(tx != NULL, "Could not begin transaction.");

    char key[64] = "muh_hash_tho";
    unsigned char val[] = "Hello I am some data for you and I am rather "
        "a lot of data aren't I? Bigger data is better, as the NoSQL world is "
        "fond of saying. Geez, I hope senpai notices me today! That would be "
        "so marvelous, really. Hopefully I don't segfault again! Wooooooooooo!"
        "{json: \"ain't real\"}";
    int inserted = olt_jar(tx, key, strlen(key), val, strlen((char*)val));

    check(inserted == 0, "Could not insert.");

    olt_unjar(tx, key, strlen(key), &item, NULL);
    check(item != NULL, "Could not find key.");
    check(memcmp(item, val, strlen((char*)val)) == 0, "Returned value was not the same.");

    check(olt_commit(tx) == 0, "Could not commit transaction.");;
    _test_db_close(db);
    free(item);
    return 0;

error:
    olt_abort(tx);
    free(item);
    _test_db_close(db);
    return 1;
}

int test_unjar_msgpack(const ol_feature_flags features) {
    ol_database *db = _test_db_open(OL_F_SPLAYTREE);
    ol_transaction *tx = olt_begin(db);
    unsigned char *item = NULL;
    check(tx != NULL, "Could not begin transaction.");

    char key[64] = "all_jobs";
    /* msgpack-encoded std::pair<bool, std::string>: */
    unsigned char val[] = {
        "\x91\x92\xc2\xda\x00\x28\x2f\x74"
        "\x6d\x70\x2f\x6b\x79\x6f\x74\x6f"
        "\x70\x61\x6e\x74\x72\x79\x5f\x74"
        "\x65\x73\x74\x2f\x2e\x2f\x74\x61"
        "\x72\x62\x61\x6c\x6c\x5f\x61\x2e"
        "\x74\x61\x72\x2e\x67\x7a"
    };

    int inserted = olt_jar(tx, key, strlen(key), val, sizeof(val) / sizeof(unsigned char));

    check(inserted == 0, "Could not insert.");

    olt_unjar(tx, key, strlen(key), &item, NULL);

    check(item != NULL, "Could not find key.");
    check(memcmp(item, val, sizeof(val) / sizeof(unsigned char)) == 0, "Returned value was not the same.");

    check(olt_commit(tx) == 0, "Could not commit transaction.");;
    _test_db_close(db);
    free(item);
    return 0;

error:
    olt_abort(tx);
    free(item);
    _test_db_close(db);
    return 1;
}

int test_scoop(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    ol_transaction *tx = olt_begin(db);
    check(tx != NULL, "Could not begin transaction.");

    char key[64] = "muh_hash_tho";
    unsigned char val[] = "{json: \"ain't real\"}";
    int inserted = olt_jar(tx, key, strlen(key), val, strlen((char*)val));
    check(tx->transaction_db->rcrd_cnt == 1 && inserted == 0, "Could not insert.");

    check(olt_scoop(tx, key, strlen(key)) == 0, "Could not delete record.");
    check(db->rcrd_cnt == 0, "Could not delete record.");

    check(olt_commit(tx) == 0, "Could not commit transaction.");;
    _test_db_close(db);
    return 0;

error:
    olt_abort(tx);
    _test_db_close(db);
    return 1;
}

int test_update(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    ol_transaction *tx = olt_begin(db);
    unsigned char *item = NULL;
    check(tx != NULL, "Could not begin transaction.");

    char key[64] = "muh_hash_thoalk";
    unsigned char val[] = "{json: \"ain't real\", bowser: \"sucked\"}";
    int inserted = olt_jar(tx, key, strlen(key), val, strlen((char*)val));
    check(db->rcrd_cnt == 0 && tx->transaction_db->rcrd_cnt == 1 && inserted == 0, "Could not insert.");

    olt_unjar(tx, key, strlen(key), &item, NULL);
    check(item != NULL, "Could not find key.");

    check(memcmp(item, val, strlen((char*)val)) == 0,
            "Returned value was not the same.");
    free(item);
    item = NULL;

    unsigned char new_val[] = "WOW THAT WAS COOL, WASNT IT?";
    inserted = olt_jar(tx, key, strlen(key), new_val, strlen((char*)new_val));
    check(inserted == 0, "Could not insert.");

    olt_unjar(tx, key, strlen(key), &item, NULL);
    check(item != NULL, "Could not find key.");

    check(memcmp(item, new_val, strlen((char*)new_val)) == 0,
            "Returned value was not the new value.");
    ol_log_msg(LOG_INFO, "New value returned successfully.");

    check(olt_commit(tx) == 0, "Could not commit transaction.");;
    _test_db_close(db);
    free(item);
    return 0;
error:
    olt_abort(tx);
    _test_db_close(db);
    if (item != NULL)
        free(item);
    return 1;
}

static int _insert_keys(ol_database *db, unsigned int NUM_KEYS) {
    int i;
    unsigned char to_insert[] = "Hello I am some data for you and I am rather "
        "a lot of data aren't I? Bigger data is better, as the NoSQL world is "
        "fond of saying. Geez, I hope senpai notices me today! That would be "
        "so marvelous, really. Hopefully I don't segfault again! Wooooooooooo!";
    for (i = 0; i < NUM_KEYS; i++) {
        /* DONT NEED YOUR SHIT, GCC */
        char key[64] = "crazy hash";
        char append[10] = "";

        sprintf(append, "%i", i);
        strcat(key, append);

        size_t len = strlen((char *)to_insert);
        int insert_result = ol_jar(db, key, strlen(key), to_insert, len);

        if (insert_result > 0) {
            ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", insert_result);
            _test_db_close(db);
            return 2;
        }

        if (db->rcrd_cnt != i+1) {
            ol_log_msg(LOG_ERR, "Record count is not higher. Hash collision?. Error code: %i\n", insert_result);
            _test_db_close(db);
            return 3;
        }
    }
    return 0;
}

int test_feature_flags(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);

    db->enable(OL_F_SPLAYTREE, &db->feature_set);
    if (!db->is_enabled(OL_F_SPLAYTREE, &db->feature_set)) {
        ol_log_msg(LOG_ERR, "Feature did not enable correctly.");
        _test_db_close(db);
        return 1;
    }
    ol_log_msg(LOG_INFO, "Feature was enabled.");

    db->disable(OL_F_SPLAYTREE, &db->feature_set);
    if(db->is_enabled(OL_F_SPLAYTREE, &db->feature_set)) {
        ol_log_msg(LOG_ERR, "Feature did not disable correctly.");
        _test_db_close(db);
        return 2;
    }
    ol_log_msg(LOG_INFO, "Feature was disabled.");

    _test_db_close(db);
    return 0;
}

int _test_aol(const ol_feature_flags features, ol_database **db) {
    if (db == NULL || *db == NULL) {
        ol_log_msg(LOG_ERR, "No database provided");
        return 3;
    }

    /* Anable AOL and INIT, no need to restore */
    (*db)->enable(OL_F_APPENDONLY, &(*db)->feature_set);
    ol_aol_init(*db);

    const int max_records = 3;
    ol_log_msg(LOG_INFO, "Inserting %i records.", max_records);
    int ret = _insert_keys(*db, max_records);
    if (ret > 0) {
        ol_log_msg(LOG_ERR, "Error inserting keys. Error code: %d\n", ret);
        return 1;
    }

    /* Delete a key */
    if (ol_scoop(*db, "crazy hash2", strlen("crazy hash2")) == 0) {
        ol_log_msg(LOG_INFO, "Deleted record.");
    } else {
        ol_log_msg(LOG_ERR, "Could not delete record.");
        _test_db_close(*db);
        return 2;
    }

    struct tm *now;
    time_t current_time;
    time(&current_time);
    now = gmtime(&current_time);

    /* Expire a key */
    const char to_spoil[] = "crazy hash0";
    if (ol_spoil(*db, to_spoil, strlen(to_spoil), now) == 0) {
        ol_log_msg(LOG_INFO, "Spoiled record.");
    } else {
        ol_log_msg(LOG_ERR, "Could not spoil record.");
        _test_db_close(*db);
        return 2;
    }

    if ((*db)->rcrd_cnt != max_records - 1) {
        ol_log_msg(LOG_ERR, "Record count was off: %d", (*db)->rcrd_cnt);
        _test_db_close(*db);
        return 4;
    }

    char DB_PATH[DB_NAME_SIZE] = {0};
    strncpy(DB_PATH, (*db)->path, DB_NAME_SIZE);

    /* We don't want to use test_db_close here because we want to retrieve
     * values again. */
    ol_close(*db);

    ol_log_msg(LOG_INFO, "Restoring database.");
    *db = ol_open(DB_PATH, DB_NAME, DB_DEFAULT_FEATURES | OL_F_APPENDONLY);
    if (*db == NULL) {
        ol_log_msg(LOG_ERR, "Could not open database");
        return 6;
    }

    if ((*db)->rcrd_cnt != max_records - 1) {
        ol_log_msg(LOG_ERR, "Record count was off: %d", (*db)->rcrd_cnt);
        _test_db_close(*db);
        return 6;
    }

    return 0;
}

int test_aol(const ol_feature_flags features) {
    ol_log_msg(LOG_INFO, "Writing database.");
    ol_database *db = _test_db_open(features);

    int to_return = _test_aol(features, &db);

    _test_db_close(db);
    return to_return;
}

int test_aol_and_compaction(const ol_feature_flags features) {
    ol_log_msg(LOG_INFO, "Writing database.");
    ol_database *db = _test_db_open(features);
    check(db != NULL, "db is null.");

    int to_return = _test_aol(features, &db);
    check(to_return == 0, "Could not test aol subfeatures.");

    ol_log_msg(LOG_INFO, "Squishing database.");
    ol_squish(db);

    char DB_PATH[DB_NAME_SIZE] = {0};
    strncpy(DB_PATH, db->path, DB_NAME_SIZE);

    ol_close(db);

    db = ol_open(DB_PATH, DB_NAME, DB_DEFAULT_FEATURES | OL_F_APPENDONLY);
    if (db == NULL)
        return 6;

    size_t to_test;
    const char key[] = "crazy hash1";
    unsigned char *item = NULL;
    ol_unjar(db, key, strlen(key), &item, &to_test);
    ol_log_msg(LOG_INFO, "Retrieved value.");
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        _test_db_close(db);
        free(item);
        return 7;
    }

    free(item);
    _test_db_close(db);
    return to_return;

error:
    return 8;
}

int test_expiration(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);

    /* Get the current time */
    struct tm now;
    time_t current_time;

    time(&current_time);
    localtime_r(&current_time, &now);
    ol_log_msg(LOG_INFO, "Current time: %lu", current_time);

    const char key[] = "testKey";
    unsigned char value[] = "TestValue yo";

    check(ol_jar(db, key, strlen(key), value, strlen((char *)value)) == 0, "Could not insert.");
    check(ol_spoil(db, key, strlen(key), &now) == 0, "Could not set expiration");
    check(ol_exists(db, key, strlen(key)) == 1, "Key did not expire properly.");

    _test_db_close(db);

    return 0;

error:
    return 1;
}

int test_sync(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);

    const char key[] = "testKey";
    unsigned char value[] = "TestValue yo";

    check(ol_jar(db, key, strlen(key), value, strlen((char *)value)) == 0, "Could not insert.");
    check(ol_sync(db) == 0, "Could not sync database.");

    _test_db_close(db);

    return 0;

error:
    return 1;
}

int test_lz4(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    db->enable(OL_F_LZ4, &db->feature_set);

    /* This is basically the Unjar_ds test, since keys, AOL, expiration and
     * Content-type aren't compressed, it's useless to test those.
     */

    char key[] = "THE MIGHTY LZ4 TEST";
    unsigned char val[] = "111 THIS 111 MUST 111 BE 111 COMPRESSED 111 SOMEHOW";
    size_t val_len = strlen((char*)val);
    int inserted = ol_jar(db, key, strlen(key), val, val_len);

    if (inserted > 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        _test_db_close(db);
        return 1;
    }

    size_t to_test;
    unsigned char *item = NULL;
    ol_unjar(db, key, strlen(key), &item, &to_test);
    ol_log_msg(LOG_INFO, "Retrieved value.");
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        _test_db_close(db);
        free(item);
        return 2;
    }

    if (memcmp(item, val, strlen((char*)val)) != 0) {
        ol_log_msg(LOG_ERR, "Returned value was not the same.\n");
        _test_db_close(db);
        free(item);
        return 3;
    }

    if (to_test != val_len) {
        ol_log_msg(LOG_ERR, "Sizes were not the same. %p (to_test) vs. %p (val_len)\n", to_test, val_len);
        _test_db_close(db);
        free(item);
        return 4;
    }

    free(item);
    _test_db_close(db);
    return 0;
}

int test_magic_string_compression(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    db->enable(OL_F_LZ4, &db->feature_set);

    /* Sometimes LZ4 compresses strings to the same size. This test makes sure
     * decompress still works in that case.
     */

    char key[] = "test_key";
    /* Known fucked up string: */
    unsigned char val[] = "{\"test\": 2, \"test3\": [\"a\", \"b\", \"c\"]}";
    size_t val_len = strlen((char*)val);
    int inserted = ol_jar(db, key, strlen(key), val, val_len);

    if (inserted > 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        _test_db_close(db);
        return 1;
    }

    size_t to_test;
    unsigned char *item = NULL;
    ol_unjar(db, key, strlen(key), &item, &to_test);
    ol_log_msg(LOG_INFO, "Retrieved value.");
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        _test_db_close(db);
        free(item);
        return 2;
    }

    if (memcmp(item, val, strlen((char*)val)) != 0) {
        ol_log_msg(LOG_ERR, "Returned value was not the same.\n");
        _test_db_close(db);
        free(item);
        return 3;
    }

    if (to_test != val_len) {
        ol_log_msg(LOG_ERR, "Sizes were not the same. %p (to_test) vs. %p (val_len)\n", to_test, val_len);
        _test_db_close(db);
        free(item);
        return 4;
    }

    free(item);
    _test_db_close(db);
    return 0;
}

int test_can_get_next_in_tree(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    int next_records = 10;
    ol_log_msg(LOG_INFO, "Inserting %i records.", next_records);
    int ret = _insert_keys(db, next_records);
    if (ret > 0) {
        ol_log_msg(LOG_ERR, "Error inserting keys. Error code: %d\n", ret);
        return 1;
    }

    int found = 0;

    ol_cursor cursor;
    check(olc_init(db, &cursor), "Could not init cursor.");
    if (cursor.current_node != NULL)
        found++;
    while(olc_step(&cursor)) {
        const ol_splay_tree_node *node = _olc_get_node(&cursor);
        check(node != NULL, "Could not retrieve a node.");
        ol_log_msg(LOG_INFO, "Node found: %s", node->key);

        unsigned char *r_val = NULL;
        char r_key[KEY_SIZE] = {'0'};
        size_t r_vsize;

        const int ret = olc_get(&cursor, &r_key, &r_val, &r_vsize);
        check(ret == 0, "Could not retrieve key and value from cursor.");

        found++;
    }

    check(found == next_records, "Did not find enough records. Only found %i.", found);

    _test_db_close(db);
    return 0;

error:
    if (db != NULL)
        _test_db_close(db);
    return 1;
}

int test_can_get_prev_in_tree(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    int next_records = 10;
    ol_log_msg(LOG_INFO, "Inserting %i records.", next_records);
    int ret = _insert_keys(db, next_records);
    if (ret > 0) {
        ol_log_msg(LOG_ERR, "Error inserting keys. Error code: %d\n", ret);
        return 1;
    }

    int found = 0;

    ol_cursor cursor;
    check(olc_init(db, &cursor), "Could not init cursor.");
    if (cursor.current_node != NULL)
        found++;
    /* Move the node to the end of the tree */
    while(olc_step(&cursor)) { }
    /* Now we start stepping backwards */
    while(olc_step_back(&cursor)) {
        const ol_splay_tree_node *node = _olc_get_node(&cursor);
        check(node != NULL, "Could not retrieve a node.");
        ol_log_msg(LOG_INFO, "Node found: %s", node->key);
        found++;
    }

    check(found == next_records, "Did not find enough records. Only found %i.", found);

    _test_db_close(db);
    return 0;

error:
    _test_db_close(db);
    return 1;
}

int test_compaction(const ol_feature_flags features) {
    ol_log_msg(LOG_INFO, "Writing database.");
    ol_database *db = _test_db_open(features);

    /* Anable AOL and INIT, no need to restore */
    db->enable(OL_F_APPENDONLY, &db->feature_set);
    ol_aol_init(db);

    const int max_records = 10;
    ol_log_msg(LOG_INFO, "Inserting %i records.", max_records);
    int ret = _insert_keys(db, max_records);
    if (ret > 0) {
        ol_log_msg(LOG_ERR, "Error inserting keys. Error code: %d\n", ret);
        return 1;
    }

    /* Delete a couple records. */
    int i = 0;
    for(; i < 2; i ++) {
        char key[64] = "crazy hash";
        char buf[20] = {0};
        sprintf(buf, "%i", i);
        strncat(key, buf, 30);
        check(ol_scoop(db, key, strnlen(key, 64)) == 0, "Could not delete record %s.", key);
    }

    struct tm *now;
    time_t current_time;
    time(&current_time);
    now = gmtime(&current_time);

    /* Expire a couple records. */
    for(; i < 5; i ++) {
        char key[64] = "crazy hash";
        char buf[20] = {0};
        sprintf(buf, "%i", i);
        strncat(key, buf, 30);
        check(ol_spoil(db, key, strnlen(key, 64), now) == 0, "Could not spoil record %s.", key)
    }

    check(ol_squish(db) == 0, "Could not compact database.");

    _test_db_close(db);
    return 0;

error:
    _test_db_close(db);
    return 9;
}

int test_can_match_prefixes(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);
    int next_records = 10;
    ol_log_msg(LOG_INFO, "Inserting %i records.", next_records);
    int ret = _insert_keys(db, next_records);
    if (ret > 0) {
        ol_log_msg(LOG_ERR, "Error inserting keys. Error code: %d\n", ret);
        return 1;
    }

    char key[] = "test";
    unsigned char to_insert[] = "Thjis lsl;ajfldskjf";
    size_t len = strlen((char *)to_insert);
    ret = ol_jar(db, key, strlen(key), to_insert, len);
    if (ret > 0) {
        ol_log_msg(LOG_ERR, "Error inserting keys. Error code: %d\n", ret);
        return 1;
    }

    char key2[] = "crazy";
    unsigned char to_insert2[] = "This should not match.";
    len = strlen((char *)to_insert2);
    ret = ol_jar(db, key2, strlen(key2), to_insert2, len);
    if (ret > 0) {
        ol_log_msg(LOG_ERR, "Error inserting keys. Error code: %d\n", ret);
        return 1;
    }

    /* Really fuck this tree up */
    int max_records = next_records;
    int i = 0;
    for (i = 0; i < max_records; i++) {
        char key3[] = "random_shit";
        char prepend[64] = "";

        sprintf(prepend, "%i", i);
        strcat(prepend, key3);

        size_t len = strlen((char *)to_insert);
        int insert_result = ol_jar(db, prepend, strlen(prepend), to_insert, len);

        if (insert_result > 0) {
            ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", insert_result);
            _test_db_close(db);
            return 2;
        }
    }

    char key3[] = "crazy hash666";
    len = strlen((char *)to_insert2);
    ret = ol_jar(db, key3, strlen(key3), to_insert2, len);
    if (ret > 0) {
        ol_log_msg(LOG_ERR, "Error inserting keys. Error code: %d\n", ret);
        return 1;
    }

    ol_key_array matches_list = NULL;
    ret = ol_prefix_match(db, "crazy hash", strlen("crazy hash"), &matches_list);
    if (ret != next_records + 1) {
        ol_log_msg(LOG_ERR, "Found the wrong number of matches. Error code: %d\n", ret);
        return 1;
    }

    for (i = 0; i < ret; i++) {
        ol_log_msg(LOG_INFO, "Found key: %s", matches_list[i]);
        free(matches_list[i]);
    }
    free(matches_list);

    /* Test if we can dump all keys */
    matches_list = NULL;
    ret = ol_key_dump(db, &matches_list);
    if (ret != db->rcrd_cnt) {
        ol_log_msg(LOG_ERR, "Found the wrong number of matches. Error code: %d\n", ret);
        return 1;
    }

    for (i = 0; i < ret; i++) {
        ol_log_msg(LOG_INFO, "Found key: %s", matches_list[i]);
        free(matches_list[i]);
    }
    free(matches_list);

    _test_db_close(db);
    return 0;

}

void run_tests(int results[2]) {
    int tests_run = 0;
    int tests_failed = 0;

    ol_test_start();

    /* These tests are special and depend on certain features being enabled
     * or disabled. */
    const ol_feature_flags feature_set = DB_DEFAULT_FEATURES;
    ol_run_test(test_basic_transaction);
    ol_run_test(test_can_jump_cursor);
    ol_run_test(test_unjar_msgpack);
    ol_run_test(test_aol_and_compaction);
    ol_run_test(test_aol);
    ol_run_test(test_lz4);
    ol_run_test(test_magic_string_compression);
    ol_run_test(test_can_find_all_nodes);
    ol_run_test(test_can_get_next_in_tree);
    ol_run_test(test_can_get_prev_in_tree);
    ol_run_test(test_can_match_prefixes);

    /* Permute all features enabled for these tests, so that we get all of our
     * code paths tested. */
    int i = 0;
    const int FEATURE_NUM = 5;
    for(; i <= FEATURE_NUM; i++) {
        /* OL_F_AOL_FFLUSH is slow as balls and we don't care about it */
        const ol_feature_flags feature_set = (i | OL_F_AOL_FFLUSH) & OL_F_APPENDONLY;
        /* Fucking macros man */
        ol_run_test(test_jar);
        ol_run_test(test_unjar_ds);
        ol_run_test(test_cas);
        ol_run_test(test_unjar);
        ol_run_test(test_sync);
        ol_run_test(test_compaction);
        ol_run_test(test_open_close);
        ol_run_test(test_bucket_max);
        ol_run_test(test_zero_length_keys);
        ol_run_test(test_scoop);
        ol_run_test(test_expiration);
        ol_run_test(test_update);
        ol_run_test(test_lots_of_deletes);
        ol_run_test(test_feature_flags);
    }

/* Skip all tests when one fails */
error:
    results[0] = tests_run;
    results[1] = tests_failed;
}
