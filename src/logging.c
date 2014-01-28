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
