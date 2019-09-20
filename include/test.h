#pragma once
/* Functions and macros for the unit tests. */

#include <string.h>
#include <unistd.h>

#define DB_NAME "testdb"
#define RECORD_COUNT 100000

#define ol_test_start() int test_return_val = 0;
#define ol_run_test(test) ol_log_msg(LOG_INFO, "----- %s -----\n", #test);\
    ol_log_msg(LOG_INFO, "Testing with: %d", feature_set);\
    test_return_val = test(feature_set);\
    if (test_return_val != 0 || errno != 0) {\
        tests_failed++;\
        ol_log_msg(LOG_ERR, "%c[%dmFailed.%c[%dm", 0x1B, 31, 0x1B, 0);\
        if (errno != 0) ol_log_msg(LOG_ERR, "errno is not 0!");\
        if (test_return_val != 0) ol_log_msg(LOG_ERR, "Retval: %d", test_return_val);\
        ol_log_msg(LOG_ERR, "ERRNO: %s\n", clean_errno());\
        goto error;\
    } else {\
        tests_run++;\
        ol_log_msg(LOG_INFO, "%c[%dmPassed.%c[%dm\n", 0x1B, 32, 0x1B, 0);\
    }

void run_tests();
