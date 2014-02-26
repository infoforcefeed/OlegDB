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

#include <stdlib.h>
#include "test.h"
#include "aol.h"
#include "logging.h"

int test_open_close(void) {
    ol_database *db = ol_open(DB_PATH, DB_NAME);
    ol_log_msg(LOG_INFO, "Opened DB: %p.", db);
    int ret = ol_close(db);
    if (ret > 0){
        ol_log_msg(LOG_INFO, "Couldn't free all memory.");
    } else {
        ol_log_msg(LOG_INFO, "Closed DB: %p.", db);
    }
    return 0;
}

int test_bucket_max(void) {
    int expected_bucket_max = HASH_MALLOC / 8;
    ol_database *db = ol_open(DB_PATH, DB_NAME);

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

int test_jar(void) {
    ol_database *db = ol_open(DB_PATH, DB_NAME);
    ol_log_msg(LOG_INFO, "Opened DB: %p.", db);

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

int test_unjar_ds(void) {
    ol_database *db = ol_open(DB_PATH, DB_NAME);
    ol_log_msg(LOG_INFO, "Opened DB: %p.", db);

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
    ol_val item = ol_unjar_ds(db, key, strlen(key), &to_test);
    ol_log_msg(LOG_INFO, "Retrieved value.");
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        ol_close(db);
        return 2;
    }

    if (memcmp(item, val, strlen((char*)val)) != 0) {
        ol_log_msg(LOG_ERR, "Returned value was not the same.\n");
        ol_close(db);
        return 3;
    }

    if (to_test != val_len) {
        ol_log_msg(LOG_ERR, "Sizes were not the same. %p (to_test) vs. %p (val_len)\n", to_test, val_len);
        ol_close(db);
        return 4;
    }

    ol_close(db);
    return 0;
}
int test_unjar(void) {
    /* TODO: This test should actually make sure all of the data is consistent */
    ol_database *db = ol_open(DB_PATH, DB_NAME);
    ol_log_msg(LOG_INFO, "Opened DB: %p.", db);

    char key[64] = "muh_hash_tho";
    unsigned char val[] = "{json: \"ain't real\"}";
    int inserted = ol_jar(db, key, strlen(key), val, strlen((char*)val));

    if (inserted > 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        ol_close(db);
        return 1;
    }

    ol_val item = ol_unjar(db, key, strlen(key));
    ol_log_msg(LOG_INFO, "Retrieved value.");
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        ol_close(db);
        return 2;
    }

    if (memcmp(item, val, strlen((char*)val)) != 0) {
        ol_log_msg(LOG_ERR, "Returned value was not the same.\n");
        ol_close(db);
        return 3;
    }

    ol_close(db);
    return 0;
}

int test_scoop(void) {
    ol_database *db = ol_open(DB_PATH, DB_NAME);
    ol_log_msg(LOG_INFO, "Opened DB: %p.", db);

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

int test_uptime(void) {
    ol_database *db = ol_open(DB_PATH, DB_NAME);
    ol_log_msg(LOG_INFO, "Opened DB: %p.", db);

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

int test_update(void) {
    ol_database *db = ol_open(DB_PATH, DB_NAME);
    ol_log_msg(LOG_INFO, "Opened DB: %p.", db);

    char key[64] = "muh_hash_thoalk";
    unsigned char val[] = "{json: \"ain't real\", bowser: \"sucked\"}";
    int inserted = ol_jar(db, key, strlen(key), val, strlen((char*)val));

    if (inserted > 0) {
        ol_log_msg(LOG_ERR, "Could not insert. Error code: %i\n", inserted);
        ol_close(db);
        return 1;
    }

    ol_val item = ol_unjar(db, key, strlen(key));
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        ol_close(db);
        return 2;
    }

    if (memcmp(item, val, strlen((char*)val)) != 0) {
        ol_log_msg(LOG_ERR, "Returned value was not the same.\n");
        ol_close(db);
        return 3;
    }

    unsigned char new_val[] = "WOW THAT WAS COOL, WASNT IT?";
    inserted = ol_jar(db, key, strlen(key), new_val, strlen((char*)new_val));

    item = ol_unjar(db, key, strlen(key));
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key);
        ol_close(db);
        return 2;
    }

    if (memcmp(item, new_val, strlen((char*)new_val)) != 0) {
        ol_log_msg(LOG_ERR, "Returned value was not the new value.\nVal: %s\n", item);
        ol_close(db);
        return 3;
    }
    ol_log_msg(LOG_INFO, "New value returned successfully.");

    ol_close(db);
    return 0;
}

static int _insert_keys(ol_database *db, unsigned int NUM_KEYS) {
    int i;
    unsigned char to_insert[] = "Hello I am some data for you";
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
    }
    return 0;
}

int test_dump_forking(void) {
    ol_database *db = ol_open(DB_PATH, DB_NAME);
    ol_log_msg(LOG_INFO, "Opened DB: %p.", db);

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

    db = ol_open(DB_PATH, DB_NAME);

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

int test_ct(void) {
    ol_database *db = ol_open(DB_PATH, DB_NAME);
    ol_log_msg(LOG_INFO, "Opened DB: %p.", db);

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

    ol_val item = ol_unjar(db, key1, strlen(key1));
    if (item == NULL) {
        ol_log_msg(LOG_ERR, "Could not find key: %s\n", key1);
        ol_close(db);
        return 2;
    }

    item = ol_unjar(db, key2, strlen(key2));
    if (item == NULL) {
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

int test_dump(void) {
    ol_database *db = ol_open(DB_PATH, DB_NAME);
    ol_log_msg(LOG_INFO, "Opened DB: %p.", db);

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

    db = ol_open(DB_PATH, DB_NAME);

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

int test_feature_flags(void) {
    ol_database *db = ol_open(DB_PATH, DB_NAME);
    ol_log_msg(LOG_INFO, "Opened DB: %p.", db);

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

int test_aol(void) {
    ol_database *db = ol_open(DB_PATH, DB_NAME);
    db->enable(OL_F_APPENDONLY, &db->feature_set);
    ol_aol_init(db);

    int i;
    int max_records = 3;
    unsigned char to_insert[] = "123456789\nthis is a test!";
    ol_log_msg(LOG_INFO, "Inserting %i records.", max_records);
    for (i = 0; i < max_records; i++) { /* 8======D */
        char key[] = "crazy hash";
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

    if (ol_scoop(db, "crazy hash2", strlen("crazy hash2")) == 0) {
        ol_log_msg(LOG_INFO, "Deleted record.");
    } else {
        ol_log_msg(LOG_ERR, "Could not delete record.");
        ol_close(db);
        return 1;
    }

    if (db->rcrd_cnt != max_records - 1) {
        ol_log_msg(LOG_ERR, "Record count was off: %d", db->rcrd_cnt);
        ol_close(db);
        return 4;
    }

    ol_close(db);

    db = ol_open(DB_PATH, DB_NAME);
    db->enable(OL_F_APPENDONLY, &db->feature_set);
    ol_aol_init(db);

    if (ol_aol_restore(db) != 0) {
        ol_log_msg(LOG_ERR, "Error during AOL restore...");
        ol_close(db);
        return 5;
    }

    if (db->rcrd_cnt != max_records - 1) {
        ol_log_msg(LOG_ERR, "Record count was off: %d", db->rcrd_cnt);
        ol_close(db);
        return 6;
    }

    ol_log_msg(LOG_INFO, "Cleaning up files created...");
    if (unlink(db->aol_file) != 0) {
        ol_log_msg(LOG_ERR, "Could not remove file: %s", db->aol_file);
        return 7;
    }

    return 0;
}

void run_tests(int results[2]) {
    int tests_run = 0;
    int tests_failed = 0;

    ol_test_start();
    ol_run_test(test_aol);
    ol_run_test(test_open_close);
    ol_run_test(test_bucket_max);
    ol_run_test(test_jar);
    ol_run_test(test_unjar);
    ol_run_test(test_unjar_ds);
    ol_run_test(test_scoop);
    ol_run_test(test_update);
    ol_run_test(test_ct);
    ol_run_test(test_dump);
    ol_run_test(test_dump_forking);
    ol_run_test(test_feature_flags);
    ol_run_test(test_uptime);

    results[0] = tests_run;
    results[1] = tests_failed;
}
