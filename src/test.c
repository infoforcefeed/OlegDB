/* Unit tests. */
#include <stdlib.h>
#include "aol.h"
#include "cursor.h"
#include "errhandle.h"
#include "logging.h"
#include "test.h"
#include "tree.h"

/* Helper function to open databases, so we don't have to change API code
 * in a million places when we modify it.
 */
ol_database *_test_db_open() {
    ol_database *db = ol_open(DB_PATH, DB_NAME, OL_F_SPLAYTREE | OL_F_LZ4);
    if (db != NULL) {
        ol_log_msg(LOG_INFO, "Opened DB: %p.", db);
    } else {
        ol_log_msg(LOG_ERR, "Could not open database.");
    }
    return db;
}

int test_open_close() {
    ol_database *db = _test_db_open();
    int ret = ol_close(db);
    if (ret > 0){
        ol_log_msg(LOG_INFO, "Couldn't free all memory.");
    } else {
        ol_log_msg(LOG_INFO, "Closed DB.");
    }
    return 0;
}

int test_bucket_max() {
    ol_database *db = _test_db_open();
    int expected_bucket_max = HASH_MALLOC / 8;

    ol_log_msg(LOG_INFO, "Expected max is: %i", expected_bucket_max);
    int generated_bucket_max = ol_ht_bucket_max(db->cur_ht_size);
    if (expected_bucket_max != generated_bucket_max) {
        ol_log_msg(LOG_INFO, "Unexpected bucket max. Got: %d", generated_bucket_max);
        ol_close(db);
        return 1;
    }
    ol_log_msg(LOG_INFO, "Generated max is: %i", expected_bucket_max);
    ol_close(db);
    return 0;
}

int test_jar() {
    ol_database *db = _test_db_open();

    int i;
    int max_records = RECORD_COUNT;
    unsigned char to_insert[] = "123456789";
    for (i = 0; i < max_records; i++) { /* 8======D */
        char key[64] = "crazy hash";
        char append[10] = "";

        sprintf(append, "%i", i);
        strcat(key, append);

        size_t len = strlen((char *)to_insert);
        int insert_result = ol_jar(db, key, strlen(key), to_insert, len);

        if (insert_result > 0) {
            ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", insert_result);
            ol_close(db);
            return 2;
        }

        if (db->rcrd_cnt != i+1) {
            ol_log_msg(LOG_ERR, "Record count is not higher. Hash collision?. Error code: %i\n", insert_result);
            ol_close(db);
            return 3;
        }
    }
    ol_log_msg(LOG_INFO, "Records inserted: %i.", db->rcrd_cnt);
    ol_log_msg(LOG_INFO, "Saw %d collisions.", db->key_collisions);

    if (ol_close(db) != 0) {
        ol_log_msg(LOG_ERR, "Couldn't free all memory.\n");
        return 1;
    }
    return 0;
}

int test_can_find_all_nodes() {
    ol_database *db = _test_db_open();

    int i;
    int max_records = RECORD_COUNT;
    unsigned char to_insert[] = "123456789";
    for (i = 0; i < max_records; i++) { /* 8======D */
        char key[64] = "crazy hash";
        char append[10] = "";

        sprintf(append, "%i", i);
        strcat(key, append);

        size_t len = strlen((char *)to_insert);
        size_t klen = strlen(key);
        int insert_result = ol_jar(db, key, klen, to_insert, len);

        if (insert_result > 0) {
            ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", insert_result);
            ol_close(db);
            return 2;
        }

        if (db->rcrd_cnt != i+1) {
            ol_log_msg(LOG_ERR, "Record count is not higher. Hash collision?. Error code: %i\n", insert_result);
            ol_close(db);
            return 3;
        }


        ol_splay_tree_node *found = ols_find(db->tree, key, klen);
        if (found == NULL) {
            ol_log_msg(LOG_ERR, "Could not find key we just inserted!");
            ol_close(db);
            return 4;
        }

        if (found->klen != klen ||
            strncmp(found->key, key, klen) != 0) {
            ol_log_msg(LOG_ERR, "The bucket we got back isn't actuall what we asked for.");
            ol_close(db);
            return 5;
        }

    }
    ol_log_msg(LOG_INFO, "Records inserted: %i.", db->rcrd_cnt);
    ol_log_msg(LOG_INFO, "Saw %d collisions.", db->key_collisions);
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
            ol_close(db);
            return 6;
        }

        if (found->klen != strlen(key) ||
            strncmp(found->key, key, strlen(key)) != 0) {
            ol_log_msg(LOG_ERR, "The bucket we got back isn't actuall what we asked for.");
            ol_close(db);
            return 7;
        }
    }
    */

    if (ol_close(db) != 0) {
        ol_log_msg(LOG_ERR, "Couldn't free all memory.\n");
        return 1;
    }
    return 0;
}
int test_lots_of_deletes() {
    ol_database *db = _test_db_open();

    int i;
    int max_records = RECORD_COUNT;
    unsigned char to_insert[] = "123456789AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
    for (i = 0; i < max_records; i++) { /* 8======D */
        char key[KEY_SIZE] = "A";
        char append[32] = "";

        sprintf(append, "%i", i);
        strcat(key, append);

        size_t len = strlen((char *)to_insert);
        int insert_result = ol_jar(db, key, strlen(key), to_insert, len);

        if (insert_result > 0) {
            ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", insert_result);
            ol_close(db);
            return 2;
        }

        if (db->rcrd_cnt != i+1) {
            ol_log_msg(LOG_ERR, "Record count is not higher. Hash collision?. Error code: %i\n", insert_result);
            ol_close(db);
            return 3;
        }
    }
    ol_log_msg(LOG_INFO, "Records inserted: %i.", db->rcrd_cnt);
    ol_log_msg(LOG_INFO, "Saw %d collisions.", db->key_collisions);

    for (i = 0; i < max_records; i++) { /* 8======D */
        char key[KEY_SIZE] = "A";
        char append[32] = "";

        sprintf(append, "%i", i);
        strcat(key, append);

        int delete_result = ol_scoop(db, key, strlen(key));

        if (delete_result > 0) {
            ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", delete_result);
            ol_close(db);
            return 2;
        }

        if (db->rcrd_cnt != max_records - i - 1) {
            ol_log_msg(LOG_INFO, "Record count: %i max_records - i: %i", db->rcrd_cnt, max_records - i);
            ol_log_msg(LOG_ERR, "Record count is not lower. Error code: %i", delete_result);
            ol_close(db);
            return 3;
        }
    }

    if (ol_close(db) != 0) {
        ol_log_msg(LOG_ERR, "Couldn't free all memory.\n");
        return 1;
    }
    return 0;
}

int test_unjar_ds() {
    ol_database *db = _test_db_open();

    char key[64] = "FANCY KEY IS YO MAMA";
    unsigned char val[] = "invariable variables invariably trip up programmers";
    size_t val_len = strlen((char*)val);
    int inserted = ol_jar(db, key, strlen(key), val, val_len);

    if (inserted > 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        ol_close(db);
        return 1;
    }

    size_t to_test;
    unsigned char *item = NULL;
    ol_unjar_ds(db, key, strlen(key), &item, &to_test);
    ol_log_msg(LOG_INFO, "Retrieved value.");
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        free(item);
        ol_close(db);
        return 2;
    }

    if (memcmp(item, val, strlen((char*)val)) != 0) {
        ol_log_msg(LOG_ERR, "Returned value was not the same.\n");
        free(item);
        ol_close(db);
        return 3;
    }

    if (to_test != val_len) {
        ol_log_msg(LOG_ERR, "Sizes were not the same. %p (to_test) vs. %p (val_len)\n", to_test, val_len);
        free(item);
        ol_close(db);
        return 4;
    }

    ol_close(db);
    free(item);
    return 0;
}
int test_unjar() {
    ol_database *db = _test_db_open();

    char key[64] = "muh_hash_tho";
    unsigned char val[] = "Hello I am some data for you and I am rather"
        "a lot of data aren't I? Bigger data is better, as the NoSQL world is"
        "fond of saying. Geez, I hope senpai notices me today! That would be "
        "so marvelous, really. Hopefully I don't segfault again! Wooooooooooo!"
        "{json: \"ain't real\"}";
    int inserted = ol_jar(db, key, strlen(key), val, strlen((char*)val));

    if (inserted > 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        ol_close(db);
        return 1;
    }

    unsigned char *item = NULL;
    ol_unjar(db, key, strlen(key), &item);
    ol_log_msg(LOG_INFO, "Retrieved value.");
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        free(item);
        ol_close(db);
        return 2;
    }

    if (memcmp(item, val, strlen((char*)val)) != 0) {
        ol_log_msg(LOG_ERR, "Returned value was not the same.\n");
        free(item);
        ol_close(db);
        return 3;
    }

    ol_close(db);
    free(item);
    return 0;
}

int test_scoop() {
    ol_database *db = _test_db_open();

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
        ol_close(db);
        return 1;
    }

    if (db->rcrd_cnt != 0) {
        ol_log_msg(LOG_ERR, "Record not deleted.\n");
        return 2;
    }

    ol_log_msg(LOG_INFO, "Record count is: %i", db->rcrd_cnt);
    ol_close(db);
    return 0;
}

int test_uptime() {
    ol_database *db = _test_db_open();

    sleep(3);
    int uptime = ol_uptime(db);
    ol_log_msg(LOG_INFO, "Uptime: %.i seconds", uptime);

    if (uptime < 3) {
        ol_log_msg(LOG_ERR, "Uptime incorrect.\n");
        ol_close(db);
        return 1;
    }

    ol_close(db);
    return 0;
}

int test_update() {
    ol_database *db = _test_db_open();

    char key[64] = "muh_hash_thoalk";
    unsigned char val[] = "{json: \"ain't real\", bowser: \"sucked\"}";
    int inserted = ol_jar(db, key, strlen(key), val, strlen((char*)val));

    if (inserted > 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        ol_close(db);
        return 1;
    }

    unsigned char *item = NULL;
    ol_unjar(db, key, strlen(key), &item);
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        ol_close(db);
        free(item);
        return 2;
    }

    if (memcmp(item, val, strlen((char*)val)) != 0) {
        ol_log_msg(LOG_ERR, "Returned value was not the same.\n");
        ol_close(db);
        free(item);
        return 3;
    }
    free(item);

    unsigned char new_val[] = "WOW THAT WAS COOL, WASNT IT?";
    inserted = ol_jar(db, key, strlen(key), new_val, strlen((char*)new_val));
    if (inserted != 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        ol_close(db);
        return 1;
    }

    if (inserted > 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        ol_close(db);
        return 1;
    }


    ol_unjar(db, key, strlen(key), &item);
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        ol_close(db);
        free(item);
        return 2;
    }

    if (memcmp(item, new_val, strlen((char*)new_val)) != 0) {
        ol_log_msg(LOG_ERR, "Returned value was not the new value.\nVal: %s\n", item);
        ol_close(db);
        free(item);
        return 3;
    }
    ol_log_msg(LOG_INFO, "New value returned successfully.");

    ol_close(db);
    free(item);
    return 0;
}

static int _insert_keys(ol_database *db, unsigned int NUM_KEYS) {
    int i;
    unsigned char to_insert[] = "Hello I am some data for you and I am rather"
        "a lot of data aren't I? Bigger data is better, as the NoSQL world is"
        "fond of saying. Geez, I hope senpai notices me today! That would be "
        "so marvelous, really. Hopefully I don't segfault again! Wooooooooooo!";
    for (i = 0; i < NUM_KEYS; i++) { // 8======D
        /* DONT NEED YOUR SHIT, GCC */
        char key[64] = "crazy hash";
        char append[10] = "";

        sprintf(append, "%i", i);
        strcat(key, append);

        size_t len = strlen((char *)to_insert);
        int insert_result = ol_jar(db, key, strlen(key), to_insert, len);

        if (insert_result > 0) {
            ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", insert_result);
            ol_close(db);
            return 2;
        }

        if (db->rcrd_cnt != i+1) {
            ol_log_msg(LOG_ERR, "Record count is not higher. Hash collision?. Error code: %i\n", insert_result);
            ol_close(db);
            return 3;
        }
    }
    return 0;
}

int test_dump_forking() {
    ol_database *db = _test_db_open();

    int ret;
    unsigned int num_keys = RECORD_COUNT;
    ret = _insert_keys(db, num_keys);
    if (ret > 0) {
        ol_log_msg(LOG_ERR, "Error inserting keys. Error code: %d\n", ret);
        return 1;
    }

    ret = ol_background_save(db);

    if (ret != 0) {
        ol_log_msg(LOG_ERR, "Could not background save DB");
        return 1;
    }

    /* Close the DB to try and load it */
    ol_close(db);

    db = _test_db_open();

    char tmp_path[512];
    db->get_db_file_name(db, "dump", tmp_path);

    ol_log_msg(LOG_INFO, "Loading DB from disk");
    ol_load_db(db, tmp_path);

    if (db->rcrd_cnt != num_keys) {
        ol_log_msg(LOG_ERR, "Not all records were loaded. %i\n", db->rcrd_cnt);
        return 2;
    }
    ol_log_msg(LOG_INFO, "Loaded %i records from dump file.", db->rcrd_cnt);
    ol_log_msg(LOG_INFO, "Checking for corruption.");
    int i;
    for (i = 0; i < (db->cur_ht_size/sizeof(ol_bucket*));i++) {
        ol_bucket *temp = db->hashes[i];

        if (temp != NULL) {
            do {
                if (temp->data_ptr == NULL || temp->data_size == 0) {
                    ol_log_msg(LOG_ERR, "Records were corrupt or something. I: %i\n", i);
                    exit(1);
                }
                temp = temp->next;
            } while(temp != NULL);
        }
    }
    /* Finally, close and exit */
    ol_close(db);
    return 0;
}

int test_ct() {
    ol_database *db = _test_db_open();

    char key1[] = "test_key", key2[] = "test_key2";
    char ct1[] = "application/json", ct2[] = "image/png";
    unsigned char v1[] = "ILL BURN YOUR HOUSE DOWN",
                  v2[] = "test_Value";
    int inserted = ol_jar_ct(db, key1, strlen(key1),
        v1, strlen((char*)v2),
        ct1, strlen(ct1));

    if (inserted > 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        ol_close(db);
        return 1;
    }

    inserted = ol_jar_ct(db, key2, strlen(key2),
        v2, strlen((char*)v2),
        ct2, strlen(ct2));

    if (inserted > 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        ol_close(db);
        return 1;
    }

    int ret = ol_exists(db, key1, strlen(key1));
    if (ret != 0) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key1);
        ol_close(db);
        return 2;
    }

    ret = ol_exists(db, key2, strlen(key2));
    if (ret != 0) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key2);
        ol_close(db);
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

    ol_close(db);
    return 0;
}

int test_dump() {
    ol_database *db = _test_db_open();

    int ret;
    unsigned int num_keys = RECORD_COUNT;
    ret = _insert_keys(db, num_keys);
    if (ret > 0) {
        ol_log_msg(LOG_ERR, "Error inserting keys. Error code: %d\n", ret);
        return 1;
    }
    if (db->rcrd_cnt != num_keys) {
        ol_log_msg(LOG_ERR, "Not all records were inserted.\n", ret);
        return 2;
    }

    ol_log_msg(LOG_INFO, "Dumping DB to disk.");
    ret = ol_save_db(db);
    if (ret != 0) {
        ol_log_msg(LOG_ERR, "Could not save DB\n");
        return 1;
    }
    ol_log_msg(LOG_INFO, "Dumped %i records", num_keys);

    ol_close(db);

    db = _test_db_open();

    char tmp_path[512];
    db->get_db_file_name(db, "dump", tmp_path);

    ol_log_msg(LOG_INFO, "Loading DB from disk.");
    ol_load_db(db, tmp_path);

    if (db->rcrd_cnt != num_keys) {
        ol_log_msg(LOG_ERR, "Not all records were loaded. %i\n", db->rcrd_cnt);
        return 2;
    }
    ol_log_msg(LOG_INFO, "Loaded %i records from dump file.", db->rcrd_cnt);
    ol_log_msg(LOG_INFO, "Checking for corruption.");
    int i;
    for (i = 0; i < (db->cur_ht_size/sizeof(ol_bucket*));i++) {
        ol_bucket *temp = db->hashes[i];

        if (temp != NULL) {
            do {
                if (temp->data_ptr == NULL || temp->data_size == 0) {
                    ol_log_msg(LOG_ERR, "Records were corrupt or something. I: %i\n", i);
                    exit(1);
                }
                temp = temp->next;
            } while(temp != NULL);
        }
    }

    ol_close(db);
    return 0;
}

int test_feature_flags() {
    ol_database *db = _test_db_open();

    db->enable(OL_F_APPENDONLY, &db->feature_set);

    if (!db->is_enabled(OL_F_APPENDONLY, &db->feature_set)) {
        ol_log_msg(LOG_ERR, "Feature did not enable correctly.");
        return 1;
    }
    ol_log_msg(LOG_INFO, "Feature was enabled.");

    db->disable(OL_F_APPENDONLY, &db->feature_set);

    if(db->is_enabled(OL_F_APPENDONLY, &db->feature_set)) {
        ol_log_msg(LOG_ERR, "Feature did not disable correctly.");
        return 2;
    }
    ol_log_msg(LOG_INFO, "Feature was disabled.");

    return 0;
}

int test_aol() {
    ol_database *db = _test_db_open();
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
        ol_close(db);
        return 2;
    }

    struct tm *now;
    time_t current_time;
    time(&current_time);
    now = gmtime(&current_time);

    /* Expire a key */
    if (ol_spoil(db, "crazy hash1", strlen("crazy hash1"), now) == 0) {
        ol_log_msg(LOG_INFO, "Spoiled record.");
    } else {
        ol_log_msg(LOG_ERR, "Could not spoil record.");
        ol_close(db);
        return 2;
    }

    if (db->rcrd_cnt != max_records - 1) {
        ol_log_msg(LOG_ERR, "Record count was off: %d", db->rcrd_cnt);
        ol_close(db);
        return 4;
    }

    ol_close(db);

    db = ol_open(DB_PATH, DB_NAME, OL_F_APPENDONLY);

    if (db->rcrd_cnt != max_records - 1) {
        ol_log_msg(LOG_ERR, "Record count was off: %d", db->rcrd_cnt);
        ol_close(db);
        return 6;
    }

    ol_log_msg(LOG_INFO, "Cleaning up files created...");
    if (unlink(db->aol_file) != 0) {
        ol_log_msg(LOG_ERR, "Could not remove file: %s", db->aol_file);
        ol_close(db);
        return 7;
    }

    ol_close(db);
    return 0;
}

int test_expiration() {
    ol_database *db = _test_db_open();

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

    ol_close(db);

    return 0;

error:
    return 1;
}

int test_lz4() {
    ol_database *db = _test_db_open();
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
        ol_close(db);
        return 1;
    }

    size_t to_test;
    unsigned char *item = NULL;
    ol_unjar_ds(db, key, strlen(key), &item, &to_test);
    ol_log_msg(LOG_INFO, "Retrieved value.");
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        ol_close(db);
        free(item);
        return 2;
    }

    if (memcmp(item, val, strlen((char*)val)) != 0) {
        ol_log_msg(LOG_ERR, "Returned value was not the same.\n");
        ol_close(db);
        free(item);
        return 3;
    }

    if (to_test != val_len) {
        ol_log_msg(LOG_ERR, "Sizes were not the same. %p (to_test) vs. %p (val_len)\n", to_test, val_len);
        ol_close(db);
        free(item);
        return 4;
    }

    free(item);
    ol_close(db);
    return 0;
}

int test_can_get_next_in_tree() {
    ol_database *db = _test_db_open();
    int next_records = 10;
    ol_log_msg(LOG_INFO, "Inserting %i records.", next_records);
    int ret = _insert_keys(db, next_records);
    if (ret > 0) {
        ol_log_msg(LOG_ERR, "Error inserting keys. Error code: %d\n", ret);
        return 1;
    }

    int found = 0;

    ol_cursor cursor;
    olc_init(db, &cursor);
    if (cursor.current_node != NULL)
        found++;
    while(olc_step(&cursor)) {
        ol_splay_tree_node *node = _olc_get_node(&cursor);
        check(node != NULL, "Could not retrieve a node.");
        ol_log_msg(LOG_INFO, "Node found: %s", node->key);
        found++;
    }

    check(found == next_records, "Did not find enough records. Only found %i.", found);

    ol_close(db);
    return 0;

error:
    if (db)
        ol_close(db);
    return 1;
}

int test_can_match_prefixes() {
    ol_database *db = _test_db_open();
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

    /* Realy fuck this tree up */
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
            ol_close(db);
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
    ol_close(db);
    return 0;

}

void run_tests(int results[2]) {
    int tests_run = 0;
    int tests_failed = 0;

    ol_test_start();
    ol_run_test(test_expiration);
    ol_run_test(test_aol);
    ol_run_test(test_open_close);
    ol_run_test(test_bucket_max);
    ol_run_test(test_jar);
    ol_run_test(test_lots_of_deletes);
    ol_run_test(test_unjar);
    ol_run_test(test_unjar_ds);
    ol_run_test(test_scoop);
    ol_run_test(test_update);
    ol_run_test(test_ct);
    ol_run_test(test_dump);
    ol_run_test(test_dump_forking);
    ol_run_test(test_feature_flags);
    ol_run_test(test_can_find_all_nodes);
    //ol_run_test(test_uptime);
    ol_run_test(test_lz4);
    ol_run_test(test_can_get_next_in_tree);
    ol_run_test(test_can_match_prefixes);

    results[0] = tests_run;
    results[1] = tests_failed;
}
