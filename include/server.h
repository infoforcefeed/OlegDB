#pragma once
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

#include <sys/socket.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include "oleg.h"

#define MAX_CLIENTS 100
#define LOCAL_PORT 8080
#define SOCK_RECV_MAX 1000
#define CLENGTH_LENGTH 16 // the length of "Content-Length: " in chars

typedef struct http http;
struct http {
    char key[KEY_SIZE];
    unsigned char *data;
    size_t data_len;
    char method[32];
    size_t method_len;
    char url[256];
    size_t url_len;
    char version[8];
};

void ol_server(ol_database *db, int port);
