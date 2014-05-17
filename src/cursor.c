#include <stdlib.h>
#include "cursor.h"
#include "errhandle.h"

void olc_init(ol_database *db, ol_cursor *cursor) {
    ol_stack *stack = NULL;

    /* Initialize the stack */
    stack = malloc(sizeof(ol_stack));
    check_mem(stack);

    stack->next = NULL;
    stack->data = NULL;

    /* Init the cursor */
    cursor->stack = NULL;
    cursor->current_node = NULL;
    cursor->last_node = NULL;

    /* Push the stack-state down to the subtree minimum. Cursors, for now
    * traverse the tree in-order. */
    ol_splay_tree_node *node = db->tree->root;
    ol_splay_tree_node *minimum = ols_subtree_minimum(db->tree->root);
    spush(&stack, (void *)node);

    while (node != minimum && node != NULL) {
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
    /* Point the reference to the stack at the current top. */
    cursor->stack = stack;
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

void olc_close(ol_cursor *cursor) {
    ol_stack *stack = cursor->stack;

    while (stack->next != NULL) {
        spop(&stack);
    }

    free(cursor->stack);
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
    ol_splay_tree_node *cur_node = cursor->current_node;
    ol_splay_tree_node *last_node = cursor->last_node;

    if (cur_node->left && last_node != cur_node->left) {
        cursor->last_node = cur_node;
        cursor->current_node = cur_node->left;
    } else if (cur_node->parent != NULL && last_node != cur_node->right) {
        cursor->last_node = cur_node;
        cursor->current_node = cur_node->parent;
    } else {
        cursor->last_node = cur_node;
        cursor->current_node = ols_subtree_minimum(cur_node);
    }

    return 0;
}
