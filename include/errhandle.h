#pragma once
/* Common macros for error handling. */

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "logging.h"

#ifdef DEBUG
#define debug(M, ...) fprintf(stderr,"DEBUG %s:%d: " M "\n",__FILE__, __LINE__,##__VA_ARGS__)
#else
#define debug(M, ...)
#endif

#define clean_errno() (errno == 0 ? "None" : strerror(errno))
#define check(A, M, ...) if(!(A)){ol_log_msg(LOG_ERR, "(%s:%d: errno: %s) " M, __FILE__,__LINE__,clean_errno(),##__VA_ARGS__);\
		errno=0;\
		goto error;}
#define check_warn(A, M, ...) if(!(A)){ol_log_msg(LOG_WARN, "(%s:%d) " M, __FILE__,__LINE__,##__VA_ARGS__);\
		goto error;}
#define sentinel(M, ...)  { log_err(M, ##__VA_ARGS__); errno=0; goto error; }
#define check_mem(A) check((A), "Out of memory.")
#define check_debug(A, M, ...) if(!(A)) {debug(M, ##__VA_ARGS__);\
		errno=0;\
		goto error;}
