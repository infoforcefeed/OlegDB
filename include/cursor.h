#pragma once
/* Functions, anecdotes, and scary stories about cursors. */
#include <setjmp.h>
#include "oleg.h"
#include "tree.h"
#include "stack.h"

/* The main cursor object. Remembers iteration and whatever. */
typedef struct ol_cursor {
    ol_splay_tree_node *maximum, *minimum;
    ol_splay_tree_node *current_node;
} ol_cursor;

/* Generic init for stepping through a tree. */
int olc_generic_init(ol_splay_tree *tree, ol_cursor *cursor);

/* TODO: Lock/unlock the DB for insertion when these happen. */
/* Creates a cursor for use. Returns 0 on failure, 1 on success.*/
int olc_init(ol_database *db, ol_cursor *cursor_ref);

/* Steps a cursor forward in the tree. */
int olc_step(ol_cursor *cursor);

/* Steps a cursor backward in the tree. */
int olc_step_back(ol_cursor *cursor);

/* Gets the in-order successor of a node. Use with caution. */
int _olc_next(ol_splay_tree_node **node, ol_splay_tree_node *maximum);

/* Gets the in-order predecessor of a node. Use with caution. */
int _olc_prev(ol_splay_tree_node **node, ol_splay_tree_node *minimum);

/* Returns a bucket object from a cursor. */
ol_bucket *_olc_get_bucket(ol_cursor *cursor);
/* Returns a splay tree node from a cursor. */
ol_splay_tree_node *_olc_get_node(ol_cursor *cursor);
