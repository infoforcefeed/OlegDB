#pragma once
/* Linked-list stack implementation. Used in a handful of places where some
 * bookkeeping is necessary. */
typedef struct ol_stack {
    const void *data;
    struct ol_stack *next;
} ol_stack;

const void *spop(ol_stack **stack);
void spush(ol_stack **stack, const void *data);
