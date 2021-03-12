#include "tmparray.h"
#include "memory.h"

tmparray_t _tmparray_make(int elem_size) {
    tmparray_t a;

    a.data        = NULL;
    a.elem_size   = elem_size;
    a.used        = 0;
    a.capacity    = TMPARRAY_DEFAULT_CAP;
    a.should_free = 1;

    return a;
}

tmparray_t _tmparray_make_with_cap(int elem_size, int initial_cap) {
    tmparray_t a;

    a.data        = NULL;
    a.elem_size   = elem_size;
    a.used        = 0;
    a.capacity    = initial_cap;
    a.should_free = 1;

    return a;
}

void _tmparray_free(tmparray_t *tmparray) {
    if (tmparray->data && tmparray->should_free) {
        tmp_mem_free(tmparray->data);
    }
    memset(tmparray, 0, sizeof(*tmparray));
}

void _tmparray_grow_if_needed(tmparray_t *tmparray) {
    void *data_save;
    int   grow;

    if (!tmparray->data) {
        tmparray->data        = tmp_mem_alloc(tmparray->capacity * tmparray->elem_size);
        tmparray->should_free = 1;
    } else {
        grow = 0;

        while (tmparray->used >= tmparray->capacity) {
            tmparray->capacity = next_power_of_2(tmparray->capacity + 1);
            grow = 1;
        }

        if (grow) {
            data_save   = tmparray->data;
            tmparray->data = tmp_mem_alloc(tmparray->capacity * tmparray->elem_size);
            memcpy(tmparray->data, data_save, tmparray->used * tmparray->elem_size);
            if (tmparray->should_free) {
                tmp_mem_free(data_save);
            }
            tmparray->should_free = 1;
        }
    }
}

void _tmparray_grow_if_needed_to(tmparray_t *tmparray, int new_cap) {
    void *data_save;
    int   grow;

    grow = 0;
    if (new_cap > tmparray->capacity) {
        grow = 1;
        tmparray->capacity = next_power_of_2(new_cap);
    }

    if (!tmparray->data) {
        tmparray->data        = tmp_mem_alloc(tmparray->capacity * tmparray->elem_size);
        tmparray->should_free = 1;
    } else {
        while (tmparray->used >= tmparray->capacity) {
            tmparray->capacity = next_power_of_2(tmparray->capacity + 1);
            grow = 1;
        }

        if (grow) {
            data_save   = tmparray->data;
            tmparray->data = tmp_mem_alloc(tmparray->capacity * tmparray->elem_size);
            memcpy(tmparray->data, data_save, tmparray->used * tmparray->elem_size);
            if (tmparray->should_free) {
                tmp_mem_free(data_save);
            }
            tmparray->should_free = 1;
        }
    }
}

void * _tmparray_next_elem(tmparray_t *tmparray) {
    void *elem_slot;

    _tmparray_grow_if_needed(tmparray);
    elem_slot = tmparray->data + (tmparray->elem_size * tmparray->used++);
    return elem_slot;
}

void * _tmparray_push(tmparray_t *tmparray, void *elem) {
    void *elem_slot;

    elem_slot = _tmparray_next_elem(tmparray);
    memcpy(elem_slot, elem, tmparray->elem_size);

    return elem_slot;
}

void * _tmparray_push_n(tmparray_t *tmparray, void *elems, int n) {
    void *elem_slot;

    if (unlikely(n == 0))    { return NULL; }

    _tmparray_grow_if_needed_to(tmparray, tmparray->used + n);

    elem_slot    = tmparray->data + (tmparray->elem_size * tmparray->used);
    tmparray->used += n;

    memcpy(elem_slot, elems, n * tmparray->elem_size);

    return elem_slot;
}

void * _tmparray_insert(tmparray_t *tmparray, int idx, void *elem) {
    void *elem_slot;

    if (idx == tmparray->used) {
        return _tmparray_push(tmparray, elem);
    }

    ASSERT(idx < tmparray->used, "can't insert into arbitrary place in tmparray");

    _tmparray_grow_if_needed(tmparray);

    elem_slot = tmparray->data + (tmparray->elem_size * idx);

    memmove(elem_slot + tmparray->elem_size,
            elem_slot,
            tmparray->elem_size * (tmparray->used - idx));

    memcpy(elem_slot, elem, tmparray->elem_size);

    tmparray->used += 1;

    return elem_slot;
}

void _tmparray_delete(tmparray_t *tmparray, int idx) {
    void *split;

    ASSERT(idx < tmparray->used, "can't delete from arbitrary place in tmparray");

    if (idx != tmparray->used - 1) {
        split = tmparray->data + (tmparray->elem_size * idx);
        memmove(split,
                split + tmparray->elem_size,
                tmparray->elem_size * (tmparray->used - idx - 1));
    }

    tmparray->used -= 1;
}

void _tmparray_zero_term(tmparray_t *tmparray) {
    _tmparray_grow_if_needed(tmparray);
    memset(tmparray->data + (tmparray->used * tmparray->elem_size),
           0,
           tmparray->elem_size);
}

void _tmparray_copy(tmparray_t *dst, tmparray_t *src) {
    _tmparray_grow_if_needed_to(dst, src->used);

    dst->used = src->used;
    memcpy(dst->data, src->data, src->used * src->elem_size);
}
