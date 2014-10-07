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
    ol_database *db;
} ol_cursor;

/* --------------------------------------- */
/* CURSOR API */

/* Generic init for stepping through a tree. */
int olc_generic_init(ol_splay_tree *tree, ol_cursor *cursor);

/* TODO: Lock/unlock the DB for insertion when these happen. */
/* Creates a cursor for use. Returns 0 on failure, 1 on success.*/
int olc_init(ol_database *db, ol_cursor *cursor_ref);

/* Steps a cursor forward in the tree. */
int olc_step(ol_cursor *cursor);

/* Steps a cursor backward in the tree. */
int olc_step_back(ol_cursor *cursor);

/* Jumps the cursor to the specified key */
/* Returns 0 on success, 1 on failure */
int olc_jump(ol_cursor *cursor, const char *key, const size_t klen);

/* Fills out the passed in key array with the key of the bucket the cursor is
 * currently on.
 * Returns 0 on success, 1 on failure. */
int olc_get_key(const ol_cursor *c, char (*key)[KEY_SIZE]);

/* Fills out the val passed in with the value the cursor is currently on.
 * ***val MUST BE FREED ***
 * Returns 0 on success, 1 on failure. */
int olc_get_val(const ol_cursor *c, unsigned char **val, size_t *vsize);

/* Basically olc_get_key and olc_get_val all in one.
 * ***val MUST BE FREED ***
 * Returns 0 on success, 1 on failure. */
int olc_get(const ol_cursor *c, char (*key)[KEY_SIZE],
            unsigned char **val, size_t *vsize);

/* --------------------------------------- */
/* INTERNAL FUNCTIONS */
/* These are more internal functions. Use them if you want to. */

/* Gets the in-order successor of a node. Use with caution. */
int _olc_next(ol_splay_tree_node **node, ol_splay_tree_node *maximum);

/* Gets the in-order predecessor of a node. Use with caution. */
int _olc_prev(ol_splay_tree_node **node, ol_splay_tree_node *minimum);

/* Returns a bucket object from a cursor. */
const ol_bucket *_olc_get_bucket(const ol_cursor *cursor);

/* Returns a splay tree node from a cursor. */
const ol_splay_tree_node *_olc_get_node(const ol_cursor *cursor);

/* Returns a bucket deref'd from the current cursor's node */
const ol_bucket *_olc_get_bucket(const ol_cursor *cursor);
