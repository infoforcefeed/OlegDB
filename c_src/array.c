#include "array.h"
#include "errhandle.h"

ola_array *ola_create(const size_t slots) {
    int ret;

    check(!slots < 1, "Slots cannot be less than 1");

    ola_array *array = malloc(sizeof(ola_array));
    check_mem(array);

    array->max_slots = slots;
    array->last_slot = 0;

    array->elements = calloc(slots, sizeof(void *));
    check_mem(array->elements);

    return array;

error:
    if (array != NULL) {
        free(array);
    }
    return NULL;
}

void ola_destroy(ola_array *array) {
    if (array != NULL) {
        free(array->elements);
        free(array);
    }
}

int ola_grow(ola_array *array) {
    int old_max_slots = array->max_slots;
    array->max_slots = old_max_slots * OLA_DEFAULT_GROWTH_RATE;
    void *elements = realloc(array->elements, array->max_slots * sizeof(void *));
    check_mem(elements);
    array->elements = elements;
    return 0;
error:
    return 1;
}

int ola_reindex(ola_array *array) {
    return 1;
}

int ola_push(ola_array *array, void *item) {
    return 1;
}

void ola_pop(ola_array *array, void *item) {

}

void ola_get(ola_array *array, int index, void *item) {

}
