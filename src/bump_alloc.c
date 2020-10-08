#include "bump_alloc.h"

static bump_alloc_block_t * bump_alloc_make_block(void) {
    void               *mem;
    bump_alloc_block_t *block;

    mem = mmap(NULL,
               DEFAULT_BLOCK_SIZE,
               PROT_READ  | PROT_WRITE,
               MAP_SHARED | MAP_ANONYMOUS,
               -1, 0);
    ASSERT(mem != MAP_FAILED, "could not map block for bump allocator");

    block = mem;

    block->base   = mem + sizeof(bump_alloc_block_t);
    block->cursor = block->base;
    block->end    = block->base + (DEFAULT_BLOCK_SIZE - sizeof(bump_alloc_block_t));
    block->prev   = NULL;

    return block;
}

void bump_alloc_make(bump_alloc_t *ba) {
    ba->head = bump_alloc_make_block();
}

void *bump_alloc(bump_alloc_t *ba, u64 size) {
    bump_alloc_block_t *block;
    void               *mem;

    block = ba->head;

    if (unlikely(block->cursor + size >= block->end)) {
        block       = bump_alloc_make_block();
        block->prev = ba->head;
        ba->head    = block;
    }

    ASSERT(block->cursor + size >= block->end, "size larger than block");

    mem            = block->cursor;
    block->cursor += size;

    return mem;
}
