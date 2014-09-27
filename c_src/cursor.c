#include <stdlib.h>
#include "cursor.h"
#include "errhandle.h"
#include "tree.h"

int olc_generic_init(ol_splay_tree *tree, ol_cursor *cursor) {
    /* Init the cursor */
    cursor->current_node = NULL;
    cursor->maximum = ols_subtree_maximum(tree->root);
    cursor->minimum = ols_subtree_minimum(tree->root);
    cursor->current_node = ols_subtree_minimum(tree->root);

    return 1;
}

int olc_init(ol_database *db, ol_cursor *cursor) {
    if (!db->is_enabled(OL_F_SPLAYTREE, &db->feature_set))
        return 0;

    return olc_generic_init(db->tree, cursor);
}

const ol_splay_tree_node *_olc_get_node(const ol_cursor *cursor) {
    return cursor->current_node;
}

const ol_bucket *_olc_get_bucket(const ol_cursor *cursor) {
    ol_bucket *bucket = (ol_bucket *)cursor->current_node->ref_obj;
    return bucket;
}

int olc_step(ol_cursor *cursor) {
    ol_splay_tree_node *max = cursor->maximum;

    return _olc_next(&(cursor->current_node), max);
}

int olc_step_back(ol_cursor *cursor) {
    ol_splay_tree_node *min = cursor->minimum;

    return _olc_prev(&(cursor->current_node), min);
}

int _olc_next(ol_splay_tree_node **node, ol_splay_tree_node *maximum) {
    check((*node) != NULL, "No nodes in tree.");

    if ((*node) == maximum)
        return 0;

    if ((*node)->right != NULL) {
        *node = ols_subtree_minimum((*node)->right);
        return 1;
    }

    ol_splay_tree_node *parent = (*node)->parent;
    while(parent != NULL && *node == parent->right) {
        *node = parent;
        parent = parent->parent;
    }

    *node = parent;
    return 1;

error:
    return 0;
}

int _olc_prev(ol_splay_tree_node **node, ol_splay_tree_node *minimum) {
    check((*node) != NULL, "No nodes in tree.");

    if ((*node) == minimum)
        return 0;

    if ((*node)->left != NULL) {
        *node = ols_subtree_maximum((*node)->left);
        return 1;
    }

    ol_splay_tree_node *parent = (*node)->parent;
    while(parent != NULL && *node == parent->left) {
        *node = parent;
        parent = parent->parent;
    }

    *node = parent;
    return 1;

error:
    return 0;
}
