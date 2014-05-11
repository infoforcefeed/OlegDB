#pragma once
/* Functions, anecdotes and scary stories about cursors. */
#include "errhandle.h"
#include "oleg.h"
#include "tree.h"
#include "stack.h"

/* The main cursor object. Remembers iteration and whatever. */
typedef struct ol_cursor {
    ol_stack *stack;
} ol_cursor;
