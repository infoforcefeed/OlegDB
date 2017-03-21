#include <stdlib.h>
#include "errhandle.h"
#include "stack.h"
/* This comment doesn't make any sense here but I'm keeping it anyway:
 * To avoid stack overflows, we have to recurse this tree iteratively to delete
 * it.
 * Oh, how I pine for better recursion.
 */
inline void spush(ol_stack **stack, const void *data) {
    check(stack != NULL, "Stack is null.");
    check(data != NULL, "Data is null.");

    ol_stack *to_push = NULL;
    to_push = malloc(sizeof(ol_stack));
    check_mem(to_push);

    to_push->data = data;
    to_push->next = *stack;

    *stack = to_push;

error:
    return;
}

inline const void *spop(ol_stack **stack) {
    check(stack != NULL, "Stack is null.");

    ol_stack *top = *stack;
    *stack = top->next;
    const void *data = top->data;

    free(top);
    return data;

error:
    return NULL;
}
