/* This is the unit tests executable. */
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

int main(int argc, char *argv[]) {
    signal(SIGTERM, clean_up);
    signal(SIGINT, clean_up);
    signal(SIGCHLD, SIG_IGN);

    ol_log_msg(LOG_INFO, "Running tests.");
    int results[2];
    run_tests(results);
    ol_log_msg(LOG_INFO, "Tests passed: %i.\n", results[0]);

    printf("No.\n");
    return results[1];
}
