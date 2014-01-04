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
#include <stdlib.h>
#include "oleg.h"


ol_database_obj ol_open(char *path, ol_filemode filemode){
    //open database
    ol_database_obj new_db = malloc(sizeof(struct ol_database));
    return new_db;
}

int ol_close(ol_database_obj database){
    free(database);
    return 0;
}

ol_val ol_unjar(char *key){
    return NULL;
}

int ol_jar(char *key, unsigned char *value){
    return 0;
}
