#pragma once
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
