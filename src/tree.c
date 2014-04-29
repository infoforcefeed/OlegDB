/* Splay tree implementation. */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "oleg.h"
#include "tree.h"
#include "logging.h"
#include "stack.h"
#include "errhandle.h"

static inline void _ols_left_rotate(ol_splay_tree *tree, ol_splay_tree_node *node) {
    ol_splay_tree_node *right_child = node->right;
    node->right = right_child->left;

    if (right_child && right_child->left)
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

    if (left_child && left_child->right)
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
    while (node->parent) {
        if (!node->parent->parent) {
            if (node->parent->left == node) _ols_right_rotate(tree, node->parent);
            else _ols_left_rotate(tree, node->parent);
        } else if (node->parent->left == node && node->parent->parent->left == node->parent) {
            _ols_right_rotate(tree, node->parent->parent);
            _ols_right_rotate(tree, node->parent);
        } else if (node->parent->right == node && node->parent->parent->right == node->parent) {
            _ols_left_rotate(tree, node->parent->parent);
            _ols_left_rotate(tree, node->parent);
        } else if (node->parent->left == node && node->parent->parent->right == node->parent) {
            _ols_right_rotate(tree, node->parent);
            _ols_left_rotate(tree, node->parent);
        } else {
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
        node_b->parent = node_a->parent;
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

    while (current_node != NULL) {
        size_t larger_key = 0;
        previous_node = current_node;
        larger_key = klen > current_node->klen ? klen : current_node->klen;
        if (strncmp(current_node->key, key, larger_key) >= 0)
            current_node = current_node->right;
        else
            current_node = current_node->left;
    }

    current_node = malloc(sizeof(ol_splay_tree_node));
    current_node->left = NULL;
    current_node->right = NULL;
    current_node->parent = NULL;
    current_node->klen = 0;
    memset(current_node->key, '\0', KEY_SIZE);
    if (strncpy(current_node->key, key, klen) != current_node->key) {
        free(current_node);
        return NULL;
    }
    current_node->klen = klen;
    current_node->ref_obj = ref_obj;
    /* Put that shit into the tree */
    current_node->parent = previous_node;

    /* Figure out how current_node relates to previous_node */
    if (previous_node == NULL)
        tree->root = current_node;
    else {
        size_t larger_key = 0;
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
        ol_splay_tree_node *minimum_node = _ols_subtree_minimum(node->right);
        if (minimum_node->parent != node) {
            _ols_replace(tree, minimum_node, minimum_node->right);
            minimum_node->right = node->right;
            minimum_node->right->parent = minimum_node;
        }
        _ols_replace(tree, node, minimum_node);
        minimum_node->left = node->left;
        minimum_node->left->parent = minimum_node;
    }

    node->parent = NULL;
    node->left = NULL;
    node->right = NULL;

    free(node);
    tree->rcrd_cnt--;
    return 0;
}

ol_splay_tree_node *ols_find(ol_splay_tree *tree, const char *key, size_t klen) {
    check(klen < KEY_SIZE, "Key is too long.");
    check(key != NULL, "Key is null.");
    ol_splay_tree_node *current_node = tree->root;

    while (current_node) {
        size_t larger_key = 0;
        larger_key = current_node->klen > klen ?
            current_node->klen : klen;
        const int result = strncmp(current_node->key, key, larger_key);
        if (result > 0)
            current_node = current_node->right;
        else if (result < 0)
            current_node = current_node->left;
        else
            return current_node;
    }

    return NULL;

error:
    return NULL;
}

static inline void _ols_free_node(ol_splay_tree_node *node) {
    struct ol_stack *stack = NULL;
    stack = malloc(sizeof(struct ol_stack));
    check_mem(stack);

    stack->next = NULL;
    stack->data = NULL;

    spush(&stack, (void *)node);

    int iters = 0;
    ol_log_msg(LOG_INFO, "Clearing tree.");
    while (stack->next != NULL) {
        iters++;
        ol_splay_tree_node *cur_node = (ol_splay_tree_node *)spop(&stack);

        if (cur_node->left != NULL) {
            spush(&stack, (void *)cur_node->left);
        }
        if (cur_node->right != NULL) {
            spush(&stack, (void *)cur_node->right);
        }
        free(cur_node);
    }
    ol_log_msg(LOG_INFO, "Tree cleared. Iterations: %i", iters);
    free(stack);

error:
    return;
}

void ols_close(ol_splay_tree *tree) {
    if (tree->root == NULL)
        return;
    _ols_free_node(tree->root);
    tree->root = NULL;
}

ol_splay_tree_node *ols_next_node(ol_splay_tree *tree, ol_splay_tree_node *cur) {
    if (!tree || !tree->root)
        return NULL;
    if (!cur)
        return NULL;

    if (cur->left != NULL) {
        debug("Left child: %s", cur->left->key);
        return cur->left;
    }
    else if (cur->right != NULL) {
        debug("Right child: %s", cur->right->key);
        return cur->right;
    } else {
        /* No parents, no children, no siblings, no god: */
        if (tree->root == cur)
            return NULL;

        /* Now it get's tricky. We need to walk up and to the right until we
         * find a node. Assume we have a parent. */
        ol_splay_tree_node *next_node = cur->parent;
        while (next_node->right == NULL) {
            if (next_node == tree->root)
                return NULL;
            next_node = next_node->parent->right;
        }
        return next_node;
    }

    return NULL;
}

/* Defined in oleg.h */
int ol_prefix_match(ol_database *db, const char *prefix, size_t plen, char **data) {
    if (!db->is_enabled(OL_F_SPLAYTREE, &db->feature_set))
        return 1;
    if (!prefix)
        return 1;
    if (data) /* We don't want to overwrite the *data ptr */
        return 1;

    ol_splay_tree *tree = db->tree;
    ol_splay_tree_node *current_node = db->tree->root;

    struct ol_stack *matches = malloc(sizeof(struct ol_stack));
    matches->data = NULL;
    matches->next = NULL;

    int imatches = 0;
    while (current_node != NULL) {
        if (strncmp(current_node->key, prefix, plen) == 0) {
            spush(&matches, current_node);
            imatches++;
        }
        current_node = ols_next_node(tree, current_node);
    }
    debug("Found %i matches.", imatches);

    /* Free all of the matches */
    while (matches->next != NULL) {
        spop(&matches);
    }
    return 0;
}
