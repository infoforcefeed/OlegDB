#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include "server.h"
#include "oleg.h"

static int ol_make_socket(void) {
    int listenfd;
    if ((listenfd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
        fprintf(stderr, "Error creating socket");
        exit(1);
    }
    return listenfd;
}

void clear_request(http *request) {
    request->url[0] = '\0';
    request->url_len = 0;
    request->method[0] = '\0';
    request->method_len = 0;
}

int build_request(char *req_buf, size_t req_len, http *request) {
    // Get the length of the command
    printf("[-] Parsing: %s\n", req_buf);
    int i;
    int method_len = 0;
    int url_len = 0;
    for (i = 0; i < SOCK_RECV_MAX; i++ ) {
        if (req_buf[i] != ' ' && req_buf[i] != '\n') {
            method_len++;
        } else {
            break;
        }
    }
    if (method_len <= 0) {
        printf("[X] Error: Could not parse method.\n");
        return 1;
    }

    request->method_len = method_len;
    strncpy(request->method, req_buf, method_len);

    for (i = (method_len+1); i < SOCK_RECV_MAX; i++ ) {
        if (req_buf[i] != ' ' && req_buf[i] != '\r' && req_buf[i] != '\n') {
            url_len++;
        } else {
            break;
        }
    }
    if (url_len <= 0) {
        printf("[-] Info: URL_Len: %i\n", url_len);
        printf("[X] Error: Could not parse URL.\n");
        return 2;
    }
    request->url_len = url_len;
    strncpy(request->url, req_buf + method_len + 1, url_len);

    return 0;
}

int run_method(char *method, size_t keylen, char* message_buf) {
    return 0;
}

void ol_server(ol_database_obj db, int port) {
    int sock, connfd;
    struct sockaddr_in servaddr, cliaddr;
    socklen_t clilen;
    pid_t childpid;
    char mesg[1000];

    sock = ol_make_socket();

    bzero(&servaddr, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
    servaddr.sin_port = htons(LOCAL_PORT);

    if (bind(sock, (struct sockaddr *)&servaddr, sizeof(servaddr))) {
        printf("[X] Error: Could not bind socket.\n");
        exit(1);
    };

    int optVal = 1;
    setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (void*) &optVal,
               sizeof(optVal));

    listen(sock, 1024);

    printf("Listening on %d\n", ntohs(servaddr.sin_port));

    while (1) {
        clilen = sizeof(cliaddr);
        connfd = accept(sock, (struct sockaddr *)&cliaddr, &clilen);

        if ((childpid = fork()) == 0) {
            close(sock);

            int n;
            http request;
            //char *key;
            while (1) {
                n = recvfrom(connfd, mesg, SOCK_RECV_MAX, 0,
                    (struct sockaddr *)&cliaddr, &clilen);

                if (build_request(mesg, n, &request) > 0) {
                    printf("[X] Error: Could not build request.\n");
                    continue;
                }

                printf("[-] Method: %s\n", request.method);
                printf("[-] URL: %s\n", request.url);

                char response[] = "HTTP/1.1 200 OK\r\nConnection: close\r\nWEEABOO\n";
                sendto(connfd, response,
                        sizeof(response), 0, (struct sockaddr *)&cliaddr,
                        sizeof(cliaddr));
                //if (strncmp("GET", mesg, strlen("GET")) == 0) {
                //    // Copy the key into a new buffer
                //    strncpy(key, mesg + 4, n);
                //    // Get the value for that key from the db
                //    ol_val data = ol_unjar(db, key);
                //    //printf("Got data: %s\n", data);
                //    sendto(connfd, data,
                //        strlen((char*)data), 0, (struct sockaddr *)&cliaddr,
                //        sizeof(cliaddr));
                //    free(key);
                //}
                //else if (strncmp("SET", mesg, strlen("SET")) == 0) {
                //    key = (char *)calloc(n -3, 1);
                //    strncpy(key, mesg + 4, n);
                //    //printf("Setting data: %s\n", key);
                //    unsigned char to_insert[] = "Wu-tang cat ain't nothin' to fuck with";
                //    ol_jar(db, key, to_insert, strlen((char*)to_insert));
                //    sendto(connfd, "OK\n", strlen("OK\n"), 0, (struct sockaddr *)&cliaddr, sizeof(cliaddr));
                //    free(key);
                //}
                mesg[n] = 0;
                clear_request(&request);
                exit(0);
            }
        }
    }
}
