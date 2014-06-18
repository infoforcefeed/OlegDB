#include "logging.h"

#include <sys/time.h>
#include <time.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

void ol_log_msg(log_level level, const char *fmsg, ...) {
    char buffer[100];
    char msg[800];
    va_list ap;
    FILE *fd;

    char *inf = "[%c[%dm-%c[%dm]"; /* Blue [-] */
    char *wrn = "[%c[%dm!%c[%dm]"; /* Yellow [!] */
    char *err = "[%c[%dmx%c[%dm]"; /* Red [x] */
    char sym_buf[15] = {0};


    if (level == LOG_ERR) {
        fd = stderr;
    } else {
        fd = stdout;
    }

    /* Time stuff */
    struct timeval timest;
    gettimeofday(&timest, NULL);
    strftime(buffer, sizeof(buffer), "%b %d %H:%M:%S",
            localtime(&timest.tv_sec));

    switch (level) {
        case LOG_INFO:
            snprintf(sym_buf, strlen(inf), inf, 0x1B, 34, 0x1B, 0);
            break;
        case LOG_WARN:
            snprintf(sym_buf, strlen(wrn), wrn, 0x1B, 33, 0x1B, 0);
            break;
        case LOG_ERR:
            snprintf(sym_buf, strlen(err), err, 0x1B, 31, 0x1B, 0);
            break;
    }

    /* Format msg */
    va_start(ap, fmsg);
    vsnprintf(msg, sizeof(msg), fmsg, ap);
    va_end(ap);

    fprintf(fd, "%s %s %s\n", buffer, sym_buf, msg);

    fflush(fd);
}
