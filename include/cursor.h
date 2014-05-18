#pragma once
/* Functions, anecdotes and scary stories about cursors. */
#include <setjmp.h>
#include "oleg.h"
#include "tree.h"
#include "stack.h"

/* The main cursor object. Remembers iteration and whatever. */
typedef struct ol_cursor {
    ol_splay_tree_node *maximum;
    ol_splay_tree_node *current_node;
} ol_cursor;

/* TODO: Lock/unlock the DB for insertion when these happen. */
/* Creates a cursor for use. */
void olc_init(ol_database *db, ol_cursor *cursor_ref);

/* Steps a cursor forward in the tree. */
int olc_step(ol_cursor *cursor);

/* Returns a bucket object from a cursor. */
ol_bucket *_olc_get_bucket(ol_cursor *cursor);
/* Returns a splay tree node from a cursor. */
ol_splay_tree_node *_olc_get_node(ol_cursor *cursor);
