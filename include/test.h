#pragma once
//        DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//                    Version 2, December 2004
//
// Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>
//
// Everyone is permitted to copy and distribute verbatim or modified
// copies of this license document, and changing it is allowed as long
// as the name is changed.
//
//            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
//
//  0. You just DO WHAT THE FUCK YOU WANT TO.

#include <string.h>
#include <unistd.h>
#include "oleg.h"

#define DB_PATH "/tmp/oleg_is_king"

#define ol_test_start() int test_return_val = 0;
#define ol_run_test(test) printf("\n-----\n%s\n", #test); test_return_val = test();\
    if (test_return_val != 0) {\
        tests_failed++;\
        printf("[ ] %c[%dmFailed.%c[%dm\n", 0x1B, 31, 0x1B, 0);\
    } else {\
        tests_run++;\
        printf("[X] %c[%dmPassed.%c[%dm\n", 0x1B, 32, 0x1B, 0);\
    }

void run_tests();
