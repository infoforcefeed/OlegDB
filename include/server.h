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

#define METHOD_MAX 32
#define URL_MAX 256

static const char get_response[] = "HTTP/1.1 200 OK\r\n"
                          "Server: OlegDB/fresh_cuts_n_jams\r\n"
                          "Content-Type: application/json\r\n"
                          "Content-Length: %zu\r\n"
                          "Connection: close\r\n"
                          "\r\n%s";

static const char post_response[] = "HTTP/1.1 200 OK\r\n"
                          "Server: OlegDB/fresh_cuts_n_jams\r\n"
                          "Content-Type: text/plain\r\n"
                          "Connection: close\r\n"
                          "Content-Length: 7\r\n"
                          "\r\n"
                          "MUDADA\n";

static const char not_found_response[] = "HTTP/1.1 404 Not Found\r\n"
                          "Status: 404 Not Found\r\n"
                          "Server: OlegDB/fresh_cuts_n_jams\r\n"
                          "Content-Length: 26\r\n"
                          "Connection: close\r\n"
                          "Content-Type: text/plain\r\n"
                          "\r\n"
                          "These aren't your ghosts.\n";

static const char deleted_response[] = "HTTP/1.1 200 OK\r\n"
                          "Server: OlegDB/fresh_cuts_n_jams\r\n"
                          "Content-Type: text/plain\r\n"
                          "Content-Length: 23\r\n"
                          "Connection: close\r\n"
                          "\r\n"
                          "That key is GONE, man.\n";

typedef struct http http;
struct http {
    char key[KEY_SIZE];
    char method[METHOD_MAX];
    size_t method_len;
    char url[URL_MAX];
    size_t url_len;
    unsigned char *data;
    size_t data_len;
};

void ol_server(ol_database *db, int port);
