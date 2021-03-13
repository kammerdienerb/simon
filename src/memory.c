#include "memory.h"
#include "tls.h"

void init_mem(void) {
    ASSERT(IS_ALIGNED(DEFAULT_BLOCK_SIZE, PAGE_SZ), "bad DEFAULT_BLOCK_SIZE -- must be multiple of PAGE_SZ");
}

#ifdef USE_LIBC_MALLOC

void   bump_alloc_make(bump_alloc_t *ba)        {                                }
void * bump_alloc(bump_alloc_t *ba, u64 size)   { return mem_alloc(size);        }
void * mem_alloc(size_t n_bytes)                { return calloc(1, n_bytes);     }
void * mem_calloc(size_t count, size_t n_bytes) { return calloc(count, n_bytes); }
void   mem_free(void *addr)                     { free(addr);                    }

#else

static bump_alloc_block_t * bump_alloc_make_block(void) {
    void               *mem;
    bump_alloc_block_t *block;

    mem = mmap(NULL,
               DEFAULT_BLOCK_SIZE,
               PROT_READ  | PROT_WRITE,
               MAP_SHARED | MAP_ANONYMOUS,
               -1, 0);
    ASSERT(mem != NULL && mem != MAP_FAILED, "could not map block for bump allocator");

    block = mem;

    block->base   = ALIGN(mem + sizeof(bump_alloc_block_t), ALIGNMENT);
    block->cursor = block->base;
    block->end    = ((void*)block) + DEFAULT_BLOCK_SIZE;
    block->prev   = NULL;

    ASSERT(IS_ALIGNED(block->cursor, ALIGNMENT), "alignment issue");
    ASSERT(*(char*)block->cursor == 0, "non-zero mem");

    return block;
}

void bump_alloc_make(bump_alloc_t *ba) {
    ba->head = bump_alloc_make_block();
}

static void * bump_big_alloc(bump_alloc_t *ba, u64 size) {
    void *mem;
    mem = mmap(NULL,
               ALIGN(size, PAGE_SZ),
               PROT_READ  | PROT_WRITE,
               MAP_SHARED | MAP_ANONYMOUS,
               -1, 0);
    ASSERT(mem != MAP_FAILED, "could not map block for big bump allocation");

    return mem;
}

void *bump_alloc(bump_alloc_t *ba, u64 size) {
    bump_alloc_block_t *block;
    void               *mem;

    ASSERT(ba != NULL, "bad bump allocator");

    block = ba->head;

    ASSERT(block != NULL, "bad bump block");

    if (unlikely(size > MAX_BUMP_ALLOC_SIZE)) {
        return bump_big_alloc(ba, size);
    }

    if (unlikely(block->cursor + size >= block->end)) {
        block       = bump_alloc_make_block();
        block->prev = ba->head;
        ba->head    = block;
    }

    ASSERT(block->cursor + size < block->end, "size larger than block");

    mem            = block->cursor;
    block->cursor += size;

    block->cursor = ALIGN(block->cursor, ALIGNMENT);

    ASSERT(*(char*)mem == 0, "non-zero mem");

    return mem;
}

void * mem_alloc(size_t n_bytes) {
    return bump_alloc(&(get_tls()->bump_alloc), n_bytes);
}

void * mem_calloc(size_t count, size_t n_bytes) {
    void *mem;

    mem = mem_alloc(n_bytes * count);

    /* bump_alloc bytes are zero initialized. */
/*     memset(mem, 0, n_bytes); */

    return mem;
}

void mem_free(void *addr) { }
#endif


#undef malloc
#undef calloc
#undef free
#undef realloc

void * tmp_mem_alloc(size_t n_bytes)                { return malloc(n_bytes);        }
void * tmp_mem_calloc(size_t count, size_t n_bytes) { return calloc(count, n_bytes); }
void   tmp_mem_free(void *addr)                     { free(addr);                    }
void * tmp_mem_realloc(void *ptr, size_t n_bytes)   { return realloc(ptr, n_bytes);  }
