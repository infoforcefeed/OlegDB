#pragma once
/* Logging. */

typedef enum {
	LOG_INFO,
	LOG_WARN,
	LOG_ERR
} log_level;

void ol_log_msg(log_level level, const char *msg, ...);
