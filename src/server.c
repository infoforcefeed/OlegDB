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

#include "parsing.h"
#include "server.h"

static int ol_make_socket(void) {
    int listenfd;
    if ((listenfd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
        fprintf(stderr, "Error creating socket");
        exit(1);
    }
    return listenfd;
}

int build_request(char *req_buf, size_t req_len, http *request) {
    int method_len = 0;
    int url_len = 0;
    int data_len = 0;

    method_len = seek_until_whitespace(req_buf, 0, METHOD_MAX);
    if (method_len <= 0) {
        printf("[X] Error: Could not parse method.\n");
        return 1;
    }
    request->method_len = method_len; /* The length of the method for offsets */
    strncpy(request->method, req_buf, method_len);

    /* Add one to i here to skip the space. */
    int method_actual_end = method_len + 1;
    url_len = seek_until_whitespace(req_buf, method_actual_end, URL_MAX);
    if (url_len <= 0) {
        printf("[X] Error: Could not parse URL.\n");
        return 2;
    }
    request->url_len = url_len;
    strncpy(request->url, req_buf + method_actual_end, url_len);

    /* Attempt to find the path in the URI */
    char *split_key = strtok(request->url, "/");
    if (split_key == NULL) {
        printf("[X] Error: Could not parse Key.\n");
        return 3;
    }
    /* Truncate the key passed in to KEY_SIZE, but don't copy any garbage if it's shorter */
    size_t lesser_of_two_evils = KEY_SIZE < strlen(split_key) ? KEY_SIZE : strlen(split_key);
    strncpy(request->key, split_key, lesser_of_two_evils);

    /* We only read a substring here because it's fast, or something. */
    char *clength_loc = NULL;
    clength_loc = strstr(req_buf, "Content-Len");

    request->data_len = 0;
    request->data = NULL;
    if (clength_loc != NULL) {
        printf("[-] Man, somebody sent us data with a length.\n");
        int j;
        /* Skip from the beginning of 'Content-Length: ' to the end: */
        clength_loc += CLENGTH_LENGTH;
        for (j = 0; j < SOCK_RECV_MAX; j++ ) {
            if (clength_loc[j] != '\r' &&
                clength_loc[j] != '\n') {
                data_len++;
            } else {
                break;
            }
        }
        char *temp_buf = malloc(data_len);
        strncpy(temp_buf, clength_loc, data_len);
        request->data_len = (size_t)atoi(temp_buf);
        printf("[-] Content-Length: %zu\n", request->data_len);
        free(temp_buf);

        /* Okay now we actually need to find the data. The end of the header */
        /* should be specified by either \r\n\r\n or \n\n. */
        char *end_of_header = strstr(req_buf, "\r\n\r\n");
        /* TODO: Check for \n\n to be compliant with shitty clients. Punks. */
        if (end_of_header == NULL) {
            printf("[-] Could not find end of header.\n");
            return 4;
        }
        /* Malloc a buffer with enough size to hold the data posted */
        request->data = malloc(request->data_len);
        strncpy((char*)request->data, end_of_header + 4, request->data_len);
        printf("[-] Data: %s\n", request->data);
    }

    return 0;
}

void handle_not_found(const int connfd, const struct sockaddr_in cliaddr) {
    sendto(connfd, not_found_response,
        sizeof(not_found_response), 0, (struct sockaddr *)&cliaddr,
        sizeof(cliaddr));
}

/* Contrary to it's name, this handles POST requests */
void handle_get(ol_database *db, const http *request,
                const int connfd, const struct sockaddr_in cliaddr) {
    char *resp_buf;
    ol_val data = ol_unjar(db, request->key);
    printf("[-] Looked for key.\n");

    if (data != NULL) {
        /* Fuck I don't know about the 2 man whatever */
        size_t content_size = strlen(get_response) + strlen((char*)data);
        resp_buf = malloc(content_size);

        sprintf(resp_buf, get_response, strlen((char*)data), data);
        sendto(connfd, resp_buf,
            strlen(resp_buf), 0, (struct sockaddr *)&cliaddr,
            sizeof(cliaddr));
        free(resp_buf);
        return;
    }
    printf("[X] Value null.\n");
    handle_not_found(connfd, cliaddr);
}

/* Also handles POST */
void handle_post(ol_database *db, const http *request,
                const int connfd, const struct sockaddr_in cliaddr) {
    if (ol_jar(db, request->key, request->data, request->data_len) > 0) {
        printf("[X] Could not insert\n");
        handle_not_found(connfd, cliaddr);
        return;
    }
    printf("[ ] Inserted new value for key %s.\n", request->key);
    printf("[-] Records: %i\n", db->rcrd_cnt);
    sendto(connfd, post_response,
        sizeof(post_response), 0, (struct sockaddr *)&cliaddr,
        sizeof(cliaddr));
}

void handle_delete(ol_database *db, const http *request,
                const int connfd, const struct sockaddr_in cliaddr) {
    if (ol_scoop(db, request->key) > 0) {
        printf("[X] Could not delete key.\n");
        handle_not_found(connfd, cliaddr);
        return;
    }
    printf("[-] Found and deleted key.\n");
    printf("[-] Records: %i\n", db->rcrd_cnt);
    sendto(connfd, deleted_response,
        sizeof(deleted_response), 0, (struct sockaddr *)&cliaddr,
        sizeof(cliaddr));
}

void _ol_close_client(int connfd) {
    printf("[-] Closing client: %i\n", connfd);
    if(close(connfd) != 0) {
        printf("[x] Error: Could not close client FD\n");
        exit(1);
    }
}

void ol_server(ol_database *db, int port) {
    int sock, connfd;
    struct sockaddr_in servaddr, cliaddr;
    socklen_t clilen;

    sock = ol_make_socket();

    bzero(&servaddr, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
    servaddr.sin_port = htons(port);

    if (bind(sock, (struct sockaddr *)&servaddr, sizeof(servaddr))) {
        printf("[X] Error: Could not bind socket.\n");
        ol_close(db);
        exit(1);
    };

    int optVal = 1;
    setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (void*) &optVal,
               sizeof(optVal));

    listen(sock, 1024);

    printf("[-] Listening on %d\n", ntohs(servaddr.sin_port));

    while (1) { /* 8======D */
        char mesg[1000];
        clilen = sizeof(cliaddr);
        printf("Waiting for connection...\n");
        connfd = accept(sock, (struct sockaddr *)&cliaddr, &clilen);
        printf("\n[-] ------\n");
        printf("[-] Opened client: %i\n", connfd);
        printf("[-] Records: %i\n", db->rcrd_cnt);
        printf("[-] DB: %p\n", db);

        http *request = calloc(1, sizeof(http));

        while (1) { /* 8=========D */
            int n;
            n = recvfrom(connfd, mesg, SOCK_RECV_MAX, 0,
                (struct sockaddr *)&cliaddr, &clilen);

            if (build_request(mesg, n, request) > 0) {
                printf("[X] Error: Could not build request.\n");
                handle_not_found(connfd, cliaddr);
                mesg[0] = '\0';
                break;
            }

            printf("[-] Method: %s\n", request->method);
            printf("[-] URL: %s\n", request->url);
            printf("[-] Key: %s\n", request->key);

            if (strncmp(request->method, "GET", 3) == 0) {
                handle_get(db, request, connfd, cliaddr);
            } else if (strncmp(request->method, "POST", 4) == 0) {
                handle_post(db, request, connfd, cliaddr);
            } else if (strncmp(request->method, "DELETE", 6) == 0) {
                handle_delete(db, request, connfd, cliaddr);
            } else {
                printf("[X] No matching method.\n");
                handle_not_found(connfd, cliaddr);
            }
            _ol_close_client(connfd);
            free(request);
            break;
        }
    }
}
