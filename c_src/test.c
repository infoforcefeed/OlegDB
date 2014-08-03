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

#define DB_DEFAULT_FEATURES OL_F_SPLAYTREE | OL_F_LZ4

/* Helper function to open databases, so we don't have to change API code
 * in a million places when we modify it.
 */
ol_database *_test_db_open(const ol_feature_flags features) {
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

    int ret = ol_close(db);

    ol_log_msg(LOG_INFO, "Unlinking %s", values_filename);
    unlink(values_filename);

    if (should_delete_aol) {
        ol_log_msg(LOG_INFO, "Unlinking %s", aol_filename);
        unlink(aol_filename);
    }

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

    check(ol_jar(db, key1, 0, value, strlen((char *)value)) != 0, "jar'd key when we shouldn't have.");
    check(ol_jar(db, key2, 0, value, strlen((char *)value)) != 0, "jar'd key when we shouldn't have.");
    check(ol_unjar(db, key1, 0, NULL) != 0, "jar'd key when we shouldn't have.");
    check(ol_unjar(db, key2, 0, NULL) != 0, "jar'd key when we shouldn't have.");
    check(ol_spoil(db, key1, 0, NULL) != 0, "spoil'd key when we shouldn't have.");
    check(ol_spoil(db, key2, 0, NULL) != 0, "spoil'd key when we shouldn't have.");
    check(ol_scoop(db, key1, 0) != 0, "scoop'd key when we shouldn't have.");
    check(ol_scoop(db, key2, 0) != 0, "scoop'd key when we shouldn't have.");

    _test_db_close(db);
    return 0;

error:
    _test_db_close(db);
    return 1;
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

    int i;
    int max_records = RECORD_COUNT;
    unsigned char to_insert[] = "123456789";
    for (i = 0; i < max_records; i++) {
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
    ol_log_msg(LOG_INFO, "Records inserted: %i.", db->rcrd_cnt);
    ol_log_msg(LOG_INFO, "Saw %d collisions.", db->meta->key_collisions);

    if (_test_db_close(db) != 0) {
        ol_log_msg(LOG_ERR, "Couldn't free all memory.\n");
        return 1;
    }
    return 0;
}

int test_can_find_all_nodes(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);

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
        int insert_result = ol_jar(db, key, klen, to_insert, len);

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


        ol_splay_tree_node *found = ols_find(db->tree, key, klen);
        if (found == NULL) {
            ol_log_msg(LOG_ERR, "Could not find key we just inserted!");
            _test_db_close(db);
            return 4;
        }

        if (found->klen != klen ||
            strncmp(found->key, key, klen) != 0) {
            ol_log_msg(LOG_ERR, "The bucket we got back isn't actuall what we asked for.");
            _test_db_close(db);
            return 5;
        }

    }
    ol_log_msg(LOG_INFO, "Records inserted: %i.", db->rcrd_cnt);
    ol_log_msg(LOG_INFO, "Saw %d collisions.", db->meta->key_collisions);
    ol_log_msg(LOG_INFO, "Now searching for all keys.");

    /* This slows things down a lot but will do a thourough search. */
    /*
    for (i = 0; i < max_records; i++) {
        char key[64] = "crazy hash";
        char append[10] = "";

        sprintf(append, "%i", i);
        strcat(key, append);

        size_t length = strlen(key);
        ol_splay_tree_node *found = ols_find(db->tree, key, length);
        if (found == NULL) {
            ol_log_msg(LOG_ERR, "Could not find key: %s", key);
            _test_db_close(db);
            return 6;
        }

        if (found->klen != strlen(key) ||
            strncmp(found->key, key, strlen(key)) != 0) {
            ol_log_msg(LOG_ERR, "The bucket we got back isn't actuall what we asked for.");
            _test_db_close(db);
            return 7;
        }
    }
    */

    if (_test_db_close(db) != 0) {
        ol_log_msg(LOG_ERR, "Couldn't free all memory.\n");
        return 1;
    }
    return 0;
}
int test_lots_of_deletes(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);

    int i;
    int max_records = RECORD_COUNT;
    unsigned char to_insert[] = "123456789AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
    for (i = 0; i < max_records; i++) {
        char key[KEY_SIZE] = "A";
        char append[32] = "";

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
    ol_log_msg(LOG_INFO, "Records inserted: %i.", db->rcrd_cnt);
    ol_log_msg(LOG_INFO, "Saw %d collisions.", db->meta->key_collisions);

    for (i = 0; i < max_records; i++) {
        char key[KEY_SIZE] = "A";
        char append[32] = "";

        sprintf(append, "%i", i);
        strcat(key, append);

        int delete_result = ol_scoop(db, key, strlen(key));

        if (delete_result > 0) {
            ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", delete_result);
            _test_db_close(db);
            return 2;
        }

        if (db->rcrd_cnt != max_records - i - 1) {
            ol_log_msg(LOG_INFO, "Record count: %i max_records - i: %i", db->rcrd_cnt, max_records - i);
            ol_log_msg(LOG_ERR, "Record count is not lower. Error code: %i", delete_result);
            _test_db_close(db);
            return 3;
        }
    }

    if (_test_db_close(db) != 0) {
        ol_log_msg(LOG_ERR, "Couldn't free all memory.\n");
        return 1;
    }
    return 0;
}

int test_unjar_ds(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);

    char key[64] = "FANCY KEY IS YO MAMA";
    unsigned char val[] = "invariable variables invariably trip up programmers";
    size_t val_len = strlen((char*)val);
    int inserted = ol_jar(db, key, strlen(key), val, val_len);

    if (inserted > 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        _test_db_close(db);
        return 1;
    }

    size_t to_test;
    unsigned char *item = NULL;
    ol_unjar_ds(db, key, strlen(key), &item, &to_test);
    ol_log_msg(LOG_INFO, "Retrieved value.");
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        free(item);
        _test_db_close(db);
        return 2;
    }

    if (memcmp(item, val, strlen((char*)val)) != 0) {
        ol_log_msg(LOG_ERR, "Returned value was not the same.");
        free(item);
        _test_db_close(db);
        return 3;
    }

    if (to_test != val_len) {
        ol_log_msg(LOG_ERR, "Sizes were not the same. %p (to_test) vs. %p (val_len)\n", to_test, val_len);
        free(item);
        _test_db_close(db);
        return 4;
    }

    _test_db_close(db);
    free(item);
    return 0;
}
int test_unjar(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);

    char key[64] = "muh_hash_tho";
    unsigned char val[] = "Hello I am some data for you and I am rather "
        "a lot of data aren't I? Bigger data is better, as the NoSQL world is "
        "fond of saying. Geez, I hope senpai notices me today! That would be "
        "so marvelous, really. Hopefully I don't segfault again! Wooooooooooo!"
        "{json: \"ain't real\"}";
    int inserted = ol_jar(db, key, strlen(key), val, strlen((char*)val));

    if (inserted > 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        _test_db_close(db);
        return 1;
    }

    unsigned char *item = NULL;
    ol_unjar(db, key, strlen(key), &item);
    ol_log_msg(LOG_INFO, "Retrieved value.");
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        free(item);
        _test_db_close(db);
        return 2;
    }

    if (memcmp(item, val, strlen((char*)val)) != 0) {
        ol_log_msg(LOG_ERR, "Returned value was not the same.");
        free(item);
        _test_db_close(db);
        return 3;
    }

    _test_db_close(db);
    free(item);
    return 0;
}

int test_unjar_msgpack(const ol_feature_flags features) {
    ol_database *db = _test_db_open(OL_F_SPLAYTREE);

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

    int inserted = ol_jar(db, key, strlen(key), val, sizeof(val) / sizeof(unsigned char));

    if (inserted > 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        _test_db_close(db);
        return 1;
    }

    unsigned char *item = NULL;
    ol_unjar(db, key, strlen(key), &item);
    ol_log_msg(LOG_INFO, "Retrieved value.");
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        free(item);
        _test_db_close(db);
        return 2;
    }

    if (memcmp(item, val, sizeof(val) / sizeof(unsigned char)) != 0) {
        ol_log_msg(LOG_ERR, "Returned value was not the same.");
        free(item);
        _test_db_close(db);
        return 3;
    }

    _test_db_close(db);
    free(item);
    return 0;
}

int test_scoop(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);

    char key[64] = "muh_hash_tho";
    unsigned char val[] = "{json: \"ain't real\"}";
    int inserted = ol_jar(db, key, strlen(key), val, strlen((char*)val));
    if (db->rcrd_cnt != 1 || inserted > 0) {
        ol_log_msg(LOG_ERR, "Record not inserted. Record count: %i\n", db->rcrd_cnt);
        return 2;
    }
    ol_log_msg(LOG_INFO, "Value inserted. Records: %i", db->rcrd_cnt);


    if (ol_scoop(db, key, strlen(key)) == 0) {
        ol_log_msg(LOG_INFO, "Deleted record.");
    } else {
        ol_log_msg(LOG_ERR, "Could not delete record.\n");
        _test_db_close(db);
        return 1;
    }

    if (db->rcrd_cnt != 0) {
        ol_log_msg(LOG_ERR, "Record not deleted.\n");
        return 2;
    }

    ol_log_msg(LOG_INFO, "Record count is: %i", db->rcrd_cnt);
    _test_db_close(db);
    return 0;
}

int test_update(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);

    char key[64] = "muh_hash_thoalk";
    unsigned char val[] = "{json: \"ain't real\", bowser: \"sucked\"}";
    int inserted = ol_jar(db, key, strlen(key), val, strlen((char*)val));

    if (inserted > 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        _test_db_close(db);
        return 1;
    }

    unsigned char *item = NULL;
    ol_unjar(db, key, strlen(key), &item);
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        _test_db_close(db);
        free(item);
        return 2;
    }

    if (memcmp(item, val, strlen((char*)val)) != 0) {
        ol_log_msg(LOG_ERR, "Returned value was not the same.");
        _test_db_close(db);
        free(item);
        return 3;
    }
    free(item);

    unsigned char new_val[] = "WOW THAT WAS COOL, WASNT IT?";
    inserted = ol_jar(db, key, strlen(key), new_val, strlen((char*)new_val));
    if (inserted != 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        _test_db_close(db);
        return 1;
    }

    ol_unjar(db, key, strlen(key), &item);
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        _test_db_close(db);
        free(item);
        return 2;
    }

    if (memcmp(item, new_val, strlen((char*)new_val)) != 0) {
        ol_log_msg(LOG_ERR, "Returned value was not the new value.\nVal: %s\n", item);
        _test_db_close(db);
        free(item);
        return 3;
    }
    ol_log_msg(LOG_INFO, "New value returned successfully.");

    _test_db_close(db);
    free(item);
    return 0;
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

int test_ct(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);

    char key1[] = "test_key", key2[] = "test_key2";
    char ct1[] = "application/json", ct2[] = "image/png";
    unsigned char v1[] = "ILL BURN YOUR HOUSE DOWN",
                  v2[] = "test_Value";
    int inserted = ol_jar_ct(db, key1, strlen(key1),
        v1, strlen((char*)v2),
        ct1, strlen(ct1));

    if (inserted > 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        _test_db_close(db);
        return 1;
    }

    inserted = ol_jar_ct(db, key2, strlen(key2),
        v2, strlen((char*)v2),
        ct2, strlen(ct2));

    if (inserted > 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        _test_db_close(db);
        return 1;
    }

    int ret = ol_exists(db, key1, strlen(key1));
    if (ret != 0) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key1);
        _test_db_close(db);
        return 2;
    }

    ret = ol_exists(db, key2, strlen(key2));
    if (ret != 0) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key2);
        _test_db_close(db);
        return 2;
    }

    char *ctr = ol_content_type(db, key1, strlen(key1));
    if (strncmp(ct1, ctr, strlen(ct1)) != 0) {
        ol_log_msg(LOG_ERR, "Content types were different.\n");
        return 3;
    }

    char *ctr2 = ol_content_type(db, key2, strlen(key2));
    if (strncmp(ct2, ctr2, strlen(ct2)) != 0) {
        ol_log_msg(LOG_ERR, "Content types were different.\n");
        return 3;
    }
    ol_log_msg(LOG_INFO, "Content type retrieved successfully.");

    _test_db_close(db);
    return 0;
}

int test_feature_flags(const ol_feature_flags features) {
    ol_database *db = _test_db_open(features);

    db->enable(OL_F_APPENDONLY, &db->feature_set);
    if (!db->is_enabled(OL_F_APPENDONLY, &db->feature_set)) {
        ol_log_msg(LOG_ERR, "Feature did not enable correctly.");
        _test_db_close(db);
        return 1;
    }
    ol_log_msg(LOG_INFO, "Feature was enabled.");

    db->disable(OL_F_APPENDONLY, &db->feature_set);
    if(db->is_enabled(OL_F_APPENDONLY, &db->feature_set)) {
        ol_log_msg(LOG_ERR, "Feature did not disable correctly.");
        _test_db_close(db);
        return 2;
    }
    ol_log_msg(LOG_INFO, "Feature was disabled.");

    _test_db_close(db);
    return 0;
}

int _test_aol(const ol_feature_flags features, ol_database *db) {
    /* Anable AOL and INIT, no need to restore */
    db->enable(OL_F_APPENDONLY, &db->feature_set);
    ol_aol_init(db);

    const int max_records = 3;
    ol_log_msg(LOG_INFO, "Inserting %i records.", max_records);
    int ret = _insert_keys(db, max_records);
    if (ret > 0) {
        ol_log_msg(LOG_ERR, "Error inserting keys. Error code: %d\n", ret);
        return 1;
    }

    /* Delete a key */
    if (ol_scoop(db, "crazy hash2", strlen("crazy hash2")) == 0) {
        ol_log_msg(LOG_INFO, "Deleted record.");
    } else {
        ol_log_msg(LOG_ERR, "Could not delete record.");
        _test_db_close(db);
        return 2;
    }

    struct tm *now;
    time_t current_time;
    time(&current_time);
    now = gmtime(&current_time);

    /* Expire a key */
    const char to_spoil[] = "crazy hash0";
    if (ol_spoil(db, to_spoil, strlen(to_spoil), now) == 0) {
        ol_log_msg(LOG_INFO, "Spoiled record.");
    } else {
        ol_log_msg(LOG_ERR, "Could not spoil record.");
        _test_db_close(db);
        return 2;
    }

    if (db->rcrd_cnt != max_records - 1) {
        ol_log_msg(LOG_ERR, "Record count was off: %d", db->rcrd_cnt);
        _test_db_close(db);
        return 4;
    }

    /* We don't want to use test_db_close here because we want to retrieve
     * values again. */
    ol_close(db);

    ol_log_msg(LOG_INFO, "Restoring database.");
    db = ol_open(DB_PATH, DB_NAME, DB_DEFAULT_FEATURES | OL_F_APPENDONLY);
    if (db == NULL) {
        ol_log_msg(LOG_ERR, "Could not open database");
        return 6;
    }

    if (db->rcrd_cnt != max_records - 1) {
        ol_log_msg(LOG_ERR, "Record count was off: %d", db->rcrd_cnt);
        _test_db_close(db);
        return 6;
    }

    return 0;
}

int test_aol(const ol_feature_flags features) {
    ol_log_msg(LOG_INFO, "Writing database.");
    ol_database *db = _test_db_open(features);

    int to_return = _test_aol(features, db);

    _test_db_close(db);
    return to_return;
}

int test_aol_and_compaction(const ol_feature_flags features) {
    ol_log_msg(LOG_INFO, "Writing database.");
    ol_database *db = _test_db_open(features);

    int to_return = _test_aol(features, db);

    ol_log_msg(LOG_INFO, "Squishing database.");
    ol_squish(db);
    ol_close(db);

    db = ol_open(DB_PATH, DB_NAME, DB_DEFAULT_FEATURES | OL_F_APPENDONLY);
    if (db == NULL)
        return 6;

    size_t to_test;
    const char key[] = "crazy hash1";
    unsigned char *item = NULL;
    ol_unjar_ds(db, key, strlen(key), &item, &to_test);
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
    ol_unjar_ds(db, key, strlen(key), &item, &to_test);
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
    ol_unjar_ds(db, key, strlen(key), &item, &to_test);
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
        ol_splay_tree_node *node = _olc_get_node(&cursor);
        check(node != NULL, "Could not retrieve a node.");
        ol_log_msg(LOG_INFO, "Node found: %s", node->key);
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
        ol_splay_tree_node *node = _olc_get_node(&cursor);
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

    check(ol_squish(db), "Could not compact database.");

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

    ol_val_array matches_list = NULL;
    ret = ol_prefix_match(db, "crazy hash", strlen("crazy hash"), &matches_list);
    if (ret != next_records + 1) {
        ol_log_msg(LOG_ERR, "Found the wrong number of matches. Error code: %d\n", ret);
        return 1;
    }

    for (i = 0; i < ret; i++) {
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
    const int FEATURE_NUM = 4;
    for(; i <= FEATURE_NUM; i++) {
        const ol_feature_flags feature_set = i;
        /* Fucking macros man */
        ol_run_test(test_sync);
        ol_run_test(test_compaction);
        ol_run_test(test_open_close);
        ol_run_test(test_bucket_max);
        ol_run_test(test_zero_length_keys);
        ol_run_test(test_jar);
        ol_run_test(test_cas);
        ol_run_test(test_unjar);
        ol_run_test(test_scoop);
        ol_run_test(test_expiration);
        ol_run_test(test_update);
        ol_run_test(test_lots_of_deletes);
        ol_run_test(test_unjar_ds);
        ol_run_test(test_ct);
        ol_run_test(test_feature_flags);
    }

/* Skip all tests when one fails */
error:
    results[0] = tests_run;
    results[1] = tests_failed;
}
