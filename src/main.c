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

#include <stdio.h>
#include "test.h"
#include "oleg.h"
#include "server.h"

/* I'm sorry Vishnu */
ol_database *db;

void clean_up(int signum) {
    ol_close(db);
    printf("[-] Exiting cleanly.\n");
    exit(0);
}

void usage(char *name) {
    fprintf(stderr, "Usage: %s test\n", name);
    fprintf(stderr, "       %s\n", name);
}

int main(int argc, char *argv[]) {
    if (argc >= 2) {
        if (strcmp(argv[1], "test") == 0) {
            printf("Running tests.\n");
            int results[2];
            run_tests(results);
            printf("\n-----\nTests passed: %i.\n\n", results[0]);
        }
        else if (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0) {
            usage(argv[0]);
            return 1;
        }
    } else {
        printf("Starting olegdb\n");
        fflush(stdout);

        signal(SIGTERM, clean_up);
        signal(SIGINT, clean_up);

        db = ol_open(DB_PATH,
                OL_MANUFACTURE_DIR | OL_CONSUME_DIR | OL_SLAUGHTER_DIR);
        ol_server(db, LOCAL_PORT);
    }

    printf("No.\n");
    return 0;
}
