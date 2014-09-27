#pragma once
/* Linked-list stack implementation. Used in a handful of places where some
 * bookkeeping is necessary. */
typedef struct ol_stack {
    const void *data;
    struct ol_stack *next;
} ol_stack;

/* Muteable stack. Use only when ol_stack won't work. */
typedef struct ol_mstack {
    void *data;
    struct ol_mstack *next;
} ol_mstack;

const void *spop(ol_stack **stack);
void spush(ol_stack **stack, const void *data);

void *mspop(ol_mstack **stack);
void mspush(ol_mstack **stack, void *data);
