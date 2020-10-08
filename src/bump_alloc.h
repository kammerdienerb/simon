#ifndef __BUMP_ALLOC_H__
#define __BUMP_ALLOC_H__

#include "internal.h"

#define DEFAULT_BLOCK_SIZE (KB(128))

typedef struct bump_alloc_block {
    void                    *base;
    void                    *cursor;
    void                    *end;
    struct bump_alloc_block *prev;
} bump_alloc_block_t;

typedef struct {
    bump_alloc_block_t *head;
} bump_alloc_t;

void  bump_alloc_make(bump_alloc_t *ba);
void *bump_alloc(bump_alloc_t *ba, u64 size);

#endif
