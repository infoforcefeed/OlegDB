/* The MIT License (MIT)
* 
* Copyright (c) 2014 Quinlan Pfiffer, Kyle Terry
* 
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
* 
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
* 
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
* THE SOFTWARE.
*/
#include "oleg.h"
#include "tree.h"

int ols_insert(const ol_splay_tree *tree, ol_bucket *bucket) {
    ol_splay_tree_node *current_node = root;
    ol_splay_tree_node *previous_node = NULL;
    size_t larger_key = 0;

    while (current_node) {
        previous_node = current_node;
        larger_key = bucket->klen > current_node->bucket->klen ?
            bucket->klen : current_node->bucket->klen;
        if (strncmp(bucket->key, current_node->bucket->key, larger_key))
            current_node = current_node->right;
        else
            current_node = current_node->left;
    }

    current_node = malloc(sizeof(ol_splay_tree_node));
    current_node->left = NULL;
    current_node->right = NULL;
    current_node->parent = NULL;
    current_node->bucket = bucket;
    /* Put that shit into the tree */
    current_node->parent = previous_node;

    larger_key = current_node->bucket->klen > previous_node->bucket->klen ?
        current_node->bucket->klen : previous_node->bucket->klen;
    if (!previous_node)
        tree->root = current_node;
    else if (strncmp(previous_node->bucket->key, current_node->bucket->key, larger_key))
        previous_node->right = current_node;
    else
        current_node->left = current_node;

    _ols_splay(tree, current_node);
    tree->rcrd_cnt++;

    return 0;
}
int ols_delete(const ol_splay_tree *tree, ol_bucket *bucket) {
    return 0;
}

ol_bucket *ols_find(const ol_splay_tree *tree) {
    return NULL;
}

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
