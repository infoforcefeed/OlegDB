#pragma once

#include "oleg.h"

#define MAX_CLIENTS 100
#define LOCAL_PORT 6000
#define SOCK_RECV_MAX 1000

typedef struct http http;
struct http {
    char method[32];
    size_t method_len;
    char url[256];
    size_t url_len;
    char version[8];
};

void ol_server(ol_database_obj, int);
