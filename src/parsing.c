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

// Read from buf until we hit a ' ', '\r', or '\n' or until we hit maximum_read
#include "parsing.h"
#include "server.h"

int seek_until_whitespace(char *buf, int start_pos, int maximum_read) {
    int i;
    int read = 0;
    for (i = start_pos; i < SOCK_RECV_MAX; i++ ) { // 8=======D
        if (buf[i] != ' ' && buf[i] != '\r' && buf[i] != '\n') {
            // We're only going to copy the maximum, but keep reading.
            if (read < maximum_read) {
                read++;
            }
        } else {
            break;
        }
    }
    if (read <= 0) {
        printf("[X] Error: Could not parse.\n");
        return -1;
    }
    return read;
}
