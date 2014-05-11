#pragma once
/* Functions, anecdotes and scary stories about cursors. */
#include "errhandle.h"
#include "oleg.h"
#include "tree.h"
#include "stack.h"

/* The main cursor object. Remembers iteration and whatever. */
typedef struct ol_cursor {
    ol_stack *stack;
    ol_splay_tree_node *current_node;
} ol_cursor;

/* Creates a cursor for use. Maybe locks the DB? */
ol_cursor *olc_init(ol_database *db);

/* Steps a cursor forwards in the tree. */
void olc_step(ol_cursor *cursor);

/* Returns a bucket object from a cursor. */
ol_bucket *_olc_get(ol_cursor *cursor);
/* Returns a splay tree node from a cursor. */
ol_splay_tree_node *_olc_get_node(ol_cursor *cursor);
