#pragma once

#include <stdlib.h>

/* xXx STRUCT=ola_array xXx
 * xXx DESCRIPTION=This is a dynamicly growing array for use inside the hash table, both in storing hash table elements and element values. xXx
 * xXx last_slot=The last slot in the array that will be used to push elements on. xXx
 * xXx max_slots=The current maximum available slots. This will change as the list grows. xXx
 * xXx elements=void* elements that will be pushed and popped from the array. xXx
 */
typedef struct ola_array {
    int last_slot;
    int max_slots;
    void **elements;
} ola_array;

/* xXx FUNCTION=ola_create xXx
 * xXx DESCRIPTION=Creates a new array. xXx
 * xXx RETURNS=A pinter to a new array object. xXx
 * xXx slots=Initializes to n slots. xXx
 */
ola_array *ola_create(const size_t slots);

/* xXx FUNCTION=ola_destroy xXx
 * xXx DESCRIPTION=Destroys the array and frees the memory. xXx
 * xXx RETURNS=0 on success. xXx
 * xXx array=Pointer to ola_array type. xXx
 */
int ola_destroy(ola_array *array);

/* xXx FUNCTION=ola_push xXx
 * xXx DESCRIPTION=Push an element onto the array. xXx
 * xXx RETURNS=0 on success. xXx
 * xXx array=Pointer to ola_array type. xXx
 * xXx item=Pointer to a void type item to push onto the array. xXx
 */
int ola_push(ola_array *array, void *item);

/* xXx FUNCTION=ola_pop xXx
 * xXx DESCRIPTION=Pop an element off the array. xXx
 * xXx RETURNS=void. No return. Check *item for null. xXx
 * xXx array=Pointer to ola_array type. xXx
 * xXx item=Pointer to a void type item to pop into. xXx
 */
void ola_pop(ola_array *array, void *item);

/* xXx FUNCTION=ola_get xXx
 * xXx DESCRIPTION=Get an element at index. xXx
 * xXx RETURNS=void pointer to element in index. xXx
 * xXx array=Pointer to ola_array type. xXx
 * xXx index=Index of slot to act on. xXx
 */
void *ola_get(ola_array *array, int index);

/* xXx FUNCTION=ola_set xXx
 * xXx DESCRIPTION=Set an element at index. xXx
 * xXx RETURNS=0 on success. xXx
 * xXx array=Pointer to ola_array type. xXx
 * xXx index=Index of slot to act on. xXx
 * xXx item=Pointer to a void type item to set at index. xXx
 */
int ola_set(ola_array *array, int index, void *item);

/* xXx FUNCTION=ola_delete xXx
 * xXx DESCRIPTION=Delete an element at index. xXx
 * xXx RETURNS=0 on success. xXx
 * xXx array=Pointer to ola_array type. xXx
 * xXx index=Index of slot to act on. xXx
 */
int ola_delete(ola_array *array, int index);
