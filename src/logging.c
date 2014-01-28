#include "logging.h"

#include <sys/time.h>
#include <time.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

void ol_log_msg(int level, const char *fmsg, ...) {
    char buffer[100];
    char msg[800];
    va_list ap;
    FILE *fd;

    char *sym = "[-][!][x]";
    char sym_buf[4];

    /* No funny bizz with the level. If it ain't mod 3 and < 7, make it 0 */
    if (level % 3 != 0 || level > 6)
        level = 0;

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

    snprintf(sym_buf, 4, "%s", sym+level);

    /* Format msg */
    va_start(ap, fmsg);
    vsnprintf(msg, sizeof(msg), fmsg, ap);
    va_end(ap);

    fprintf(fd, "%s %s %s\n", buffer, sym_buf, msg);

    fflush(stdout);
}
