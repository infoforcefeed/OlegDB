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

#include <stdio.h>
#include <string.h>
#include "oleg.h"

int main(int argc, char *argv[]) {
    if (argc >= 2) {
        if (strcmp(argv[1], "test") == 0) {
            printf("Opening DB.\n");
            ol_database_obj db = ol_open("I aint real",
                OL_CONSUME_DIR | OL_SLAUGHTER_DIR | OL_CARESS_DIR);
            printf("Opened DB: %p.\n", db);
            ol_close(db);
            printf("DB closed.\n");
        }
    }
    printf("No.\n");
    return 0;
}
