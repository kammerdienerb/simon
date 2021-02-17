#ifndef __MEMORY_H__
#define __MEMORY_H__

#include "internal.h"

#include <stddef.h>


#define PAGE_SZ            (4096)
#define DEFAULT_BLOCK_SIZE (MB(1))
#define ALIGNMENT          (sizeof(void*))

typedef struct bump_alloc_block {
    void                    *base;
    void                    *cursor;
    void                    *end;
    struct bump_alloc_block *prev;
} bump_alloc_block_t;

#define MAX_BUMP_ALLOC_SIZE (DEFAULT_BLOCK_SIZE - ALIGN(sizeof(bump_alloc_block_t), ALIGNMENT))

typedef struct {
    bump_alloc_block_t *head;
} bump_alloc_t;

void init_mem(void);


void  bump_alloc_make(bump_alloc_t *ba);
void *bump_alloc(bump_alloc_t *ba, u64 size);

void * mem_alloc(size_t n_bytes);
void * mem_calloc(size_t count, size_t n_bytes);
void   mem_free(void *addr);

#ifndef USE_LIBC_MALLOC
#define malloc  mem_alloc
#define calloc  mem_calloc
#define free    mem_free
#endif

#define realloc (__we_dont_use_realloc__)

#endif
