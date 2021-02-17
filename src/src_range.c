#include "src_range.h"

void validate_range(src_range_t *range) {
    ASSERT(range->beg.path_id == range->end.path_id,
           "point paths don't match");
    ASSERT(range->beg.buff_ptr != NULL,
           "beg.buff_ptr is NULL");
    ASSERT(range->end.buff_ptr != NULL,
           "beg.buff_ptr is NULL");
    ASSERT(range->beg.buff_ptr <= range->end.buff_ptr,
           "point buff_ptrs are wrong");
}
