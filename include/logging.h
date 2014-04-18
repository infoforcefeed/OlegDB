#pragma once
/* Logging. */

#define LOG_INFO 0
#define LOG_WARN 3
#define LOG_ERR  6

void ol_log_msg(int level, const char *msg, ...);
