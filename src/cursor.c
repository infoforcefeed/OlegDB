#include <stdlib.h>
#include "cursor.h"
#include "errhandle.h"
#include "tree.h"

void olc_init(ol_database *db, ol_cursor *cursor) {
    ol_stack *stack = NULL;

    /* Initialize the stack */
    stack = malloc(sizeof(ol_stack));
    check_mem(stack);

    stack->next = NULL;
    stack->data = NULL;

    /* Init the cursor */
    cursor->current_node = NULL;
    cursor->maximum = ols_subtree_maximum(db->tree->root);
    cursor->current_node = ols_subtree_minimum(db->tree->root);

    return;

error:
    if (stack != NULL) {
        while (stack->next != NULL) {
            spop(&stack);
        }
        free(stack);
    }
    return;
}

ol_splay_tree_node *_olc_get_node(ol_cursor *cursor) {
    return cursor->current_node;
}

ol_bucket *olc_get(ol_cursor *cursor) {
    ol_bucket *bucket = (ol_bucket *)cursor->current_node->ref_obj;
    return bucket;
}

/* TODO: Make this go backwards or forwards */
int olc_step(ol_cursor *cursor) {
    ol_splay_tree_node *node = cursor->current_node;

    if (node == cursor->maximum)
        return 0;

    if (node->right != NULL) {
        cursor->current_node = ols_subtree_minimum(node->right);
        return 1;
    }

    ol_splay_tree_node *parent = node->parent;
    while(parent != NULL && node == parent->right)
    {
        node = parent;
        parent = parent->parent;
    }

    cursor->current_node = parent;
    return 1;
}