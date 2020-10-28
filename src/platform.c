#include "platform.h"

u32 platform_get_num_hw_threads(void) {
    u32 nprocs;

    nprocs = get_nprocs();
    ASSERT(nprocs > 0, "get_nprocs() failed");

    return nprocs;
}
