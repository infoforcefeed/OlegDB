#include <stdlib.h>
#include "errhandle.h"
#include "stack.h"
/* To avoid stack overflows, we have to recurse this tree iteratively to delete
 * it.
 * Oh, how I pine for better recursion.
 */
inline void spush(struct ol_stack **stack, void *data) {
    check(stack != NULL, "Stack is null.");
    check(data != NULL, "Stack is null.");

    struct ol_stack *to_push = NULL;
    to_push = malloc(sizeof(struct ol_stack));
    check_mem(to_push);

    to_push->data = data;
    to_push->next = *stack;

    *stack = to_push;

error:
    return;
}

inline void *spop(struct ol_stack **stack) {
    check(stack != NULL, "Stack is null.");

    struct ol_stack *top = *stack;
    *stack = top->next;
    void *data = top->data;

    free(top);
    return data;

error:
    return NULL;
}
