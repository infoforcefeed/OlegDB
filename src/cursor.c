#include <stdlib.h>
#include "cursor.h"

ol_cursor *olc_init(ol_database *db) {
    ol_cursor *cursor = NULL;
    ol_stack *stack = NULL;

    /* Initialize the stack */
    stack = malloc(sizeof(ol_stack));
    check_mem(stack);

    stack->next = NULL;
    stack->data = NULL;

    /* Init the cursor */
    cursor = malloc(sizeof(ol_cursor));
    cursor->stack = stack;
    cursor->current_node = NULL;

    /* Push the stack-state down to the subtree minimum. */
    ol_splay_tree_node *node = db->tree->root;
    while (node != ols_subtree_minimum(db->tree->root) && node != NULL) {
        ol_splay_tree_node *next_node = NULL;
        if (node->left != NULL) {
            spush(&stack, (void *)node->left);
            next_node = node->left;
        }
        if (node->right != NULL) {
            spush(&stack, (void *)node->right);
            if (!next_node)
                next_node = node->right;
        }

        node = next_node;
    }
    cursor->current_node = node;
    return cursor;
error:
    if (stack != NULL) {
        spop(&stack);
        free(stack);
    }
    if (cursor != NULL)
        free(cursor);

    return NULL;
}

ol_splay_tree_node *olc_get_node(ol_cursor *cursor) {
    return cursor->current_node;
}

ol_bucket *olc_get(ol_cursor *cursor) {
    ol_bucket *bucket = (ol_bucket *)cursor->current_node->ref_obj;
    return bucket;
}
