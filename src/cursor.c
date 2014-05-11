#include <stdlib.h>
#include "cursor.h"

ol_cursor *ol_cursor_init(ol_database *db) {
    ol_cursor *cursor = NULL;
    ol_stack *stack = NULL;

    /* Initialize the stack */
    stack = malloc(sizeof(ol_stack));
    check_mem(stack);

    stack->next = NULL;
    stack->data = NULL;

    ol_splay_tree_node *node = db->tree->root;
    spush(&stack, (void *)node);

    /* Init the cursor */
    cursor = malloc(sizeof(ol_cursor));
    cursor->stack = stack;

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

