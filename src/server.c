#include "server.h"

// I can't put this in server.h for some reason
const char get_response[] = "HTTP/1.1 200 OK\n"
                          "Content-Type: application/json\n"
                          "Content-Length: %zu\n"
                          "Connection: close\n"
                          "\n"
                          "{%s}\n";

const char post_response[] = "HTTP/1.1 200 OK\n"
                          "Content-Type: text/plain\n"
                          "Connection: close\n"
                          "Content-Length: 7\n"
                          "\n"
                          "MUDADA\n";

const char not_found_response[] = "HTTP/1.1 404 Not Found\n"
                          "Status: 404 Not Found\n"
                          "Content-Length: 26\n"
                          "Connection: close\n"
                          "Content-Type: text/plain\n"
                          "\n"
                          "These aren't your ghosts.\n";

static int ol_make_socket(void) {
    int listenfd;
    if ((listenfd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
        fprintf(stderr, "Error creating socket");
        exit(1);
    }
    return listenfd;
}

void clear_request(http *request) {
    request->key[0] = '\0';
    request->url[0] = '\0';
    request->url_len = 0;
    request->method[0] = '\0';
    request->method_len = 0;
}

int build_request(char *req_buf, size_t req_len, http *request) {
    // TODO: Make sure theres actually a valid URI in the request
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
        printf("[X] Error: Could not parse URL.\n");
        return 2;
    }
    request->url_len = url_len;
    strncpy(request->url, req_buf + method_len + 1, url_len);

    char *split_key = strtok(request->url, "/");

    if (split_key == NULL) {
        printf("[X] Error: Could not parse Key.\n");
        return 3;
    }
    strncpy(request->key, split_key, strlen(split_key));

    return 0;
}

void ol_server(ol_database *db, int port) {
    int sock, connfd;
    struct sockaddr_in servaddr, cliaddr;
    socklen_t clilen;
    pid_t childpid;
    char mesg[1000];

    sock = ol_make_socket();

    bzero(&servaddr, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
    servaddr.sin_port = htons(port);

    if (bind(sock, (struct sockaddr *)&servaddr, sizeof(servaddr))) {
        printf("[X] Error: Could not bind socket.\n");
        exit(1);
    };

    // Fuck you let me rebind it you asshat
    int optVal = 1;
    setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (void*) &optVal,
               sizeof(optVal));

    listen(sock, 1024);

    printf("[-] Listening on %d\n", ntohs(servaddr.sin_port));

    /* Clean up our poor children */
    signal(SIGCHLD, SIG_IGN);
    while (1) {
        clilen = sizeof(cliaddr);
        connfd = accept(sock, (struct sockaddr *)&cliaddr, &clilen);

        if ((childpid = fork()) == 0) {
            printf("[-] Records: %i\n", db->rcrd_cnt);
            printf("[-] DB: %p\n", db);
            close(sock);

            int n;
            http request;
            char *resp_buf;
            while (1) {
                n = recvfrom(connfd, mesg, SOCK_RECV_MAX, 0,
                    (struct sockaddr *)&cliaddr, &clilen);

                if (build_request(mesg, n, &request) > 0) {
                    printf("[X] Error: Could not build request.\n");
                    continue;
                }

                printf("\n[-] ------\n");
                printf("[-] Method: %s\n", request.method);
                printf("[-] URL: %s\n", request.url);
                printf("[-] Key: %s\n", request.key);

                if (strncmp(request.method, "GET", 3) == 0) {
                    printf("[-] Method is GET.\n");
                    ol_val data = ol_unjar(db, request.key);
                    printf("[-] Looked for key.\n");

                    if (data != NULL) {
                        size_t content_size = sizeof(get_response) + sizeof(data);
                        resp_buf = malloc(content_size);

                        sprintf(resp_buf, get_response, content_size, data);
                        sendto(connfd, resp_buf,
                            sizeof(resp_buf), 0, (struct sockaddr *)&cliaddr,
                            sizeof(cliaddr));
                        free(resp_buf);
                    } else {
                        printf("[X] Value null.\n");
                        sendto(connfd, not_found_response,
                            sizeof(not_found_response), 0, (struct sockaddr *)&cliaddr,
                            sizeof(cliaddr));
                    }

                } else if (strncmp(request.method, "POST", 4) == 0) {
                    printf("[-] Method is POST.\n");
                    unsigned char to_insert[] = "Wu-tang cat ain't nothin' to fuck with";
                    if (ol_jar(db, request.key, to_insert, strlen((char*)to_insert)) > 0) {
                        printf("[X] Could not insert\n");
                        sendto(connfd, not_found_response,
                            sizeof(not_found_response), 0, (struct sockaddr *)&cliaddr,
                            sizeof(cliaddr));
                    } else {
                        printf("[ ] Inserted new value for key %s.\n", request.key);
                        printf("[-] Records: %i\n", db->rcrd_cnt);
                        sendto(connfd, post_response,
                            sizeof(post_response), 0, (struct sockaddr *)&cliaddr,
                            sizeof(cliaddr));
                    }
                } else {
                    printf("[X] No matching method.\n");
                    sendto(connfd, not_found_response,
                        sizeof(not_found_response), 0, (struct sockaddr *)&cliaddr,
                        sizeof(cliaddr));
                }
                close(sock);
                mesg[n] = 0;
                clear_request(&request);
                printf("[-] Thread exiting.\n");
                exit(EXIT_SUCCESS);
            }
        }
    }
}
