#pragma once
/* Linked-list stack implementation. Used in a handful of places where some
 * bookkeeping is necessary. */
typedef struct ol_stack {
    void *data;
    struct ol_stack *next;
} ol_stack;

void *spop(ol_stack **stack);
void spush(ol_stack **stack, void *data);
