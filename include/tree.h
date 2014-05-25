#pragma once
/* Datatypes and functions for the splay tree implementation. */

#include "defs.h"

/* Leaves of the splay tree */
typedef struct ol_splay_tree_node {
    struct  ol_splay_tree_node *left;
    struct  ol_splay_tree_node *right;
    struct  ol_splay_tree_node *parent;

    char    key[KEY_SIZE];
    size_t  klen;
    const void *ref_obj;
} ol_splay_tree_node;

/* The actual splay tree object */
typedef struct ol_splay_tree {
    ol_splay_tree_node *root;
    int rcrd_cnt;
} ol_splay_tree;

/* Insert and splay the tree. rej_obj is a pointer to an object somewhere in
 * memory that you want to reference from this node. */
ol_splay_tree_node *ols_insert(ol_splay_tree *tree, const char *key, const size_t klen, const void *ref_obj);
/* Used for iterating through a tree. This returns the closest neighbor or NULL. */
ol_splay_tree_node *ols_next_node(ol_splay_tree *tree, ol_splay_tree_node *cur);

/* Gets the right-most node. */
ol_splay_tree_node *ols_subtree_maximum(ol_splay_tree_node *node);
ol_splay_tree_node *ols_subtree_minimum(ol_splay_tree_node *node);

/* Finds and deletes a node via key */
int ols_find_and_delete(ol_splay_tree *tree, const char *key, const size_t klen);
/* Delete and splay the tree */
int ols_delete(ol_splay_tree *tree, ol_splay_tree_node *node);

/* Find an object in the tree. Returns NULL on failure to launch. */
ol_splay_tree_node *ols_find(ol_splay_tree *tree, const char *key, size_t klen);
/* Cleans up a tree (de-allocates memory, sets nulls, etc.) */
void ols_close(ol_splay_tree *tree);
