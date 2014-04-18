/* Splay tree implementation. */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "oleg.h"
#include "errhandle.h"
#include "tree.h"
#include "logging.h"

static inline void _ols_left_rotate(ol_splay_tree *tree, ol_splay_tree_node *node) {
    ol_splay_tree_node *right_child = node->right;
    node->right = right_child->left;

    if (right_child->left)
        right_child->left->parent = node;

    right_child->parent = node->parent;

    if (!node->parent)
        tree->root = right_child;
    else if (node == node->parent->left)
        node->parent->left = right_child;
    else
        node->parent->right = right_child;

    right_child->left = node;
    node->parent = right_child;
}

static inline void _ols_right_rotate(ol_splay_tree *tree, ol_splay_tree_node *node) {
    ol_splay_tree_node *left_child = node->left;
    node->left = left_child->right;

    if (left_child->right)
        left_child->right->parent = node;

    left_child->parent = node->parent;

    if (!node->parent)
        tree->root = left_child;
    else if (node == node->parent->right)
        node->parent->right = left_child;
    else
        node->parent->left = left_child;

    left_child->right = node;
    node->parent = left_child;
}

static inline void _ols_splay(ol_splay_tree *tree, ol_splay_tree_node *node) {
    /* Granny rotate */
    while (node->parent) {
        if (!node->parent->parent) {
            if (node->parent->left == node)
                _ols_right_rotate(tree, node->parent);
            else
                _ols_left_rotate(tree, node->parent);
        } else if (node->parent->left == node && node->parent->parent->left == node->parent) {
            _ols_right_rotate(tree, node->parent->parent);
            _ols_right_rotate(tree, node->parent);
        } else if (node->parent->right == node && node->parent->parent->right == node->parent) {
            _ols_left_rotate(tree, node->parent->parent);
            _ols_left_rotate(tree, node->parent);
        } else if(node->parent->left == node && node->parent->parent->right == node->parent) {
            _ols_right_rotate(tree, node->parent);
            _ols_left_rotate(tree, node->parent);
        } else {
            /* If node is a right node and node's parent is a left node */
            _ols_left_rotate(tree, node->parent);
            _ols_right_rotate(tree, node->parent);
        }
    }
}

static inline void _ols_replace(ol_splay_tree *tree,
    ol_splay_tree_node *node_a, ol_splay_tree_node *node_b) {

    if (!node_a->parent)
        tree->root = node_b;
    else if (node_a == node_a->parent->left)
        node_a->parent->left = node_b;
    else
        node_a->parent->right = node_b;

    if (node_b)
        node_a->parent = node_b->parent;
}

static inline ol_splay_tree_node *_ols_subtree_minimum(ol_splay_tree_node *node) {
    while (node->left != NULL) {
        node = node->left;
    }
    return node;
}

static inline ol_splay_tree_node *_ols_subtree_maximum(ol_splay_tree_node *node) {
    while (node->left != NULL) {
        node = node->right;
    }
    return node;
}

ol_splay_tree_node *ols_insert(ol_splay_tree *tree, const char *key, const size_t klen, const void *ref_obj) {
    check(klen < KEY_SIZE, "Key is too long.");
    check(key != NULL, "Key is null.");
    ol_splay_tree_node *current_node = NULL, *previous_node = NULL;
    current_node = tree->root;
    size_t larger_key = 0;

    while (current_node != NULL) {
        previous_node = current_node;
        larger_key = klen > current_node->klen ? klen : current_node->klen;
        if (strncmp(key, current_node->key, larger_key) >= 0)
            current_node = current_node->right;
        else
            current_node = current_node->left;
    }

    current_node = malloc(sizeof(ol_splay_tree_node));
    current_node->left = NULL;
    current_node->right = NULL;
    current_node->parent = NULL;
    if (strncpy(current_node->key, key, klen) != current_node->key)
        return NULL;
    current_node->klen = klen;
    current_node->ref_obj = ref_obj;
    /* Put that shit into the tree */
    current_node->parent = previous_node;

    /* Figure out how current_node relates to previous_node */
    if (previous_node == NULL) {
        tree->root = current_node;
    } else {
        larger_key = current_node->klen > previous_node->klen ?
            current_node->klen : previous_node->klen;
        if (strncmp(previous_node->key, current_node->key, larger_key) >= 0)
            previous_node->right = current_node;
        else
            previous_node->left = current_node;
    }

    /* Splay the node to the top. */
    _ols_splay(tree, current_node);
    tree->rcrd_cnt++;

    return current_node;

error:
    return NULL;
}
int ols_find_and_delete(ol_splay_tree *tree, const char *key, const size_t klen) {
    ol_splay_tree_node *node = ols_find(tree, key, klen);
    return ols_delete(tree, node);
}

int ols_delete(ol_splay_tree *tree, ol_splay_tree_node *node) {
    if (!node)
        return 1;

    _ols_splay(tree, node);

    if (!node->left)
        _ols_replace(tree, node, node->right);
    else if (!node->right)
        _ols_replace(tree, node, node->left);
    else {
        ol_splay_tree_node *found_node = _ols_subtree_minimum(node->right);
        if (found_node->parent != node) {
            _ols_replace(tree, found_node, found_node->right );
            found_node->right = node->right;
            found_node->right->parent = found_node;
        }
        _ols_replace(tree, node, found_node);
        found_node->left = node->left;
        found_node->left->parent = node;
    }

    free(node);
    tree->rcrd_cnt--;
    return 0;
}

ol_splay_tree_node *ols_find(ol_splay_tree *tree, const char *key, size_t klen) {
    return NULL;
}

static inline void _ols_free_node(ol_splay_tree_node *node) {
    check(node != NULL, "Node is null.");

    if (node->left != NULL) {
        _ols_free_node(node->left);
    }
    if (node->right != NULL) {
        _ols_free_node(node->right);
    }

    if (node->parent != NULL) {
        /* Dereference the parent's connection to us.
         * You're dead to me, ma. ;_; */
        if (node->parent->left && node->parent->left == node) {
            node->parent->left = NULL;
        } else if (node->parent->right && node->parent->right == node) {
            node->parent->right = NULL;
        }
    }
    free(node);

error:
    return;
}

void ols_close(ol_splay_tree *tree) {
    check(tree->root != NULL, "Tree root is NULL.");
    _ols_free_node(tree->root);
    tree->root = NULL;

error:
    return;
}

