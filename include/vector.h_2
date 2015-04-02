#pragma once
#include <stdlib.h>

typedef struct vector {
    const size_t item_size;
    size_t max_size;
    size_t count;
    void *items;
} vector;

/* Create a new vector. */
vector *vector_new(const size_t item_size, const size_t initial_element_count);

/* Append a new element to the end of the vector. This will create a copy of
 * the item passed in.
 * Returns 1 on success.
 */
int vector_append(vector *vec, const void *item, const size_t item_size);

/* Similar to vector_append, but copies just the pointer value, not what it
 * points to.
 * Returns 1 on success.
 */
int vector_append_ptr(vector *vec, const void *item);

/* Gets an element at i. */
const void *vector_get(const vector *vec, const unsigned int i);
/* Gets an element at i, sans const. Use this when you intend to mutate
 * something. */
void *vector_get_danger(const vector *vec, const unsigned int i);

/* Free a vector and it's components. */
void vector_free(vector *to_free);

