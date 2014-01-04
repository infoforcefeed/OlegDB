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
            ol_database_obj db = ol_open("I aint real",
                OL_CONSUME_DIR | OL_SLAUGHTER_DIR | OL_CARESS_DIR);
            printf("Opened DB: %p.\n", db);

            int i;
            unsigned char to_insert[] = "1234567890";
            for (i = 0; i < 100; i++) {
                char key[16] = "hashy";
                char append[5] = "";
                sprintf(append, "%i", i);
                strcat(key, append);
                int insert_result = ol_jar(db, key, to_insert, strlen((char*)to_insert));

                if (insert_result > 0) {
                    printf("Error: Could not insert. Code: %i\n", insert_result);
                    ol_close(db);
                    return 1;
                }
            }
            printf("Record count: %i.\n", db->rcrd_cnt);

            char key[16] = "hashy50";
            ol_val item = ol_unjar(db, key);
            if (item == NULL)
                printf("Could not find %s\n", key);
            //printf("item size is: %zu\n", sizeof(item));
            printf("Our value is: %s\n", item);

            char del_key[16] = "hashy60";
            if(ol_scoop(db, del_key) > 0) {
                printf("A record has been deleted\n");
            } else {
                printf("No record has been deleted\n");
            }

            // update-able values

            ol_close(db);
            printf("DB closed.\n");
        }
    }
    printf("No.\n");
    return 0;
}
