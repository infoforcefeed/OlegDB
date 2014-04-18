#pragma once
/* Datatype used in the aol for storing string data. */

typedef struct ol_string {
    char *data;
    size_t dlen;
} ol_string;
