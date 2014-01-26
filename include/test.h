#pragma once
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

#include <string.h>
#include <unistd.h>
#include "oleg.h"
#include "dump.h"

#define DB_PATH "/tmp/oleg_is_king"
#define DB_NAME "testdb"
#define RECORD_COUNT 100

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
