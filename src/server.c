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

    bind(sock, (struct sockaddr *)&servaddr, sizeof(servaddr));

    listen(sock, 1024);

    printf("Listening on %d\n", ntohs(servaddr.sin_port));

    while (1) {
        clilen = sizeof(cliaddr);
        connfd = accept(sock, (struct sockaddr *)&cliaddr, &clilen);

        if ((childpid = fork()) == 0) {
            close(sock);

            int n;
            char *key;
            while (1) {
                n = recvfrom(connfd, mesg, 1000, 0, (struct sockaddr *)&cliaddr, &clilen);
                if (strncmp("GET", mesg, strlen("GET")) == 0) {
                    key = (char *)calloc(n -3, 1);
                    strncpy(key, mesg + 4, n);
                    //printf("Getting data: %s\n", key);
                    ol_val data = ol_unjar(db, key);
                    //printf("Got data: %s\n", data);
                    sendto(connfd, data, strlen((char*)data), 0, (struct sockaddr *)&cliaddr, sizeof(cliaddr)); free(key);
                    free(key);
                }
                else if (strncmp("SET", mesg, strlen("SET")) == 0) {
                    key = (char *)calloc(n -3, 1);
                    strncpy(key, mesg + 4, n);
                    //printf("Setting data: %s\n", key);
                    unsigned char to_insert[] = "Wu-tang cat ain't nothin' to fuck with";
                    ol_jar(db, key, to_insert, strlen((char*)to_insert));
                    sendto(connfd, "OK\n", strlen("OK\n"), 0, (struct sockaddr *)&cliaddr, sizeof(cliaddr));
                    free(key);
                }
                mesg[n] = 0;
            }
        }
    }
}
