#ifndef __TMPARRAY_H__
#define __TMPARRAY_H__

#include "internal.h"

#define TMPARRAY_DEFAULT_CAP (16)

typedef struct {
    void *data;
    int   elem_size;
    int   used;
    int   capacity;
    int   should_free;
} tmparray_t;

tmparray_t _tmparray_make(int elem_size);
tmparray_t _tmparray_make_with_cap(int elem_size, int initial_cap);
void _tmparray_free(tmparray_t *tmparray);
void * _tmparray_push(tmparray_t *tmparray, void *elem);
void * _tmparray_push_n(tmparray_t *tmparray, void *elems, int n);
void * _tmparray_next_elem(tmparray_t *tmparray);
void * _tmparray_insert(tmparray_t *tmparray, int idx, void *elem);
void _tmparray_delete(tmparray_t *tmparray, int idx);
void _tmparray_zero_term(tmparray_t *tmparray);
void _tmparray_grow_if_needed(tmparray_t *tmparray);
void _tmparray_copy(tmparray_t *dst, tmparray_t *src);

#define tmparray_make(T) \
    (_tmparray_make(sizeof(T)))

#define tmparray_make_with_cap(T, cap) \
    (_tmparray_make_with_cap(sizeof(T), (cap)))

#define tmparray_free(tmparray) \
    (_tmparray_free(&(tmparray)))

#define tmparray_len(tmparray) \
    ((tmparray).used)

#define tmparray_next_elem(tmparray) \
    (_tmparray_next_elem(&(tmparray)))

#define tmparray_push(tmparray, elem) \
    (_tmparray_push(&(tmparray), &(elem)))

#define tmparray_push_n(tmparray, elems, n) \
    (_tmparray_push_n(&(tmparray), (elems), (n)))

#define tmparray_insert(tmparray, idx, elem) \
    (_tmparray_insert(&(tmparray), idx, &(elem)))

#define tmparray_delete(tmparray, idx) \
    (_tmparray_delete(&(tmparray), idx))

#define tmparray_pop(tmparray) \
    (_tmparray_delete(&(tmparray), (tmparray).used - 1))

#define tmparray_clear(tmparray) \
    ((tmparray).used = 0)

#define tmparray_item(tmparray, idx) \
    ((tmparray).data + ((tmparray).elem_size * (idx)))

#define tmparray_last(tmparray) \
    ((tmparray).used ? ((tmparray).data + ((tmparray).elem_size * ((tmparray).used - 1))) : NULL)


#define tmparray_traverse(tmparray, it)                                                \
    for (it = (tmparray).data;                                                      \
         it < (__typeof(it))((tmparray).data + ((tmparray).used * (tmparray).elem_size)); \
         it += 1)

#define tmparray_traverse_from(tmparray, it, starting_idx)                             \
    for (it = (tmparray).data + ((starting_idx) * (tmparray).elem_size);               \
         it < (__typeof(it))((tmparray).data + ((tmparray).used * (tmparray).elem_size)); \
         it += 1)

#define tmparray_rtraverse(tmparray, it)                                     \
    for (it = (tmparray).data + (((tmparray).used - 1) * (tmparray).elem_size); \
         (tmparray).used && it >= (__typeof(it))((tmparray).data);           \
         it -= 1)

#define tmparray_data(tmparray) ((tmparray).data)

#define tmparray_zero_term(tmparray) (_tmparray_zero_term(&(tmparray)))

#define tmparray_grow_if_needed(tmparray) \
    (_tmparray_grow_if_needed(&(tmparray)))

#define tmparray_copy(dst, src) \
    (_tmparray_copy(&(dst), &(src)))

#endif
