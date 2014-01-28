#include "logging.h"

#include <sys/time.h>
#include <time.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

void ol_log_msg(const char *fmsg, ...) {
    char buffer[100];
    char msg[800];
    va_list ap;

    /* Time stuff */
    struct timeval timest;
    gettimeofday(&timest, NULL);
    strftime(buffer, sizeof(buffer), "%b %d %H:%M:%S",
            localtime(&timest.tv_sec));

    /* Format msg */
    va_start(ap, fmsg);
    vsnprintf(msg, sizeof(msg), fmsg, ap);
    va_end(ap);

    fprintf(stdout, "%s [-] %s\n", buffer, msg);

    fflush(stdout);
}
