#pragma once
/* Linked-list stack implementation. Used in a handful of places where some
 * bookkeeping is necessary. */
struct ol_stack {
    void *data;
    struct ol_stack *next;
};

inline void *spop(struct ol_stack **stack);
inline void spush(struct ol_stack **stack, void *data);
