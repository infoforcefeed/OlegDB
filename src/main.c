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

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include "test.h"
#include "oleg.h"
#include "logging.h"

/* I'm sorry Vishnu
* OMG WHY
*/
ol_database *db;

void clean_up(int signum) {
    ol_close(db);
    ol_log_msg(LOG_INFO, "Exiting cleanly.");
    exit(0);
}

void usage(char *name) {
    fprintf(stderr, "Usage: %s test\n", name);
    fprintf(stderr, "       %s\n", name);
}

int main(int argc, char *argv[]) {
    if (argc >= 2) {
        signal(SIGTERM, clean_up);
        signal(SIGINT, clean_up);
        signal(SIGCHLD, SIG_IGN);
        if (strcmp(argv[1], "test") == 0) {
            ol_log_msg(LOG_INFO, "Running tests.");
            int results[2];
            run_tests(results);
            ol_log_msg(LOG_INFO, "Tests passed: %i.\n", results[0]);
        }
        else if (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0) {
            usage(argv[0]);
            return 1;
        }
    }

    printf("No.\n");
    return 0;
}
