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
        // Make the database
        ol_database_obj db = ol_open(DB_PATH,
                OL_CARESS_DIR | OL_CONSUME_DIR | OL_SLAUGHTER_DIR);
        ol_server(db, 6000);
    }

    printf("No.\n");
    return 0;
}
