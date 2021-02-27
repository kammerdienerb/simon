#include "platform.h"

u32 platform_get_num_hw_threads(void) {
    u32 nprocs;

    nprocs = 0;

#ifdef __APPLE__
    int    nm[2];
    size_t len;

    nm[0] = CTL_HW;
    nm[1] = HW_AVAILCPU;
    len   = 4;
    sysctl(nm, 2, &nprocs, &len, NULL, 0);

    if (nprocs < 1) {
        nm[1] = HW_NCPU;
        sysctl(nm, 2, &nprocs, &len, NULL, 0);
    }
#else
    nprocs = get_nprocs();
#endif

    ASSERT(nprocs > 0, "failed to get the number of hw threads");

    return nprocs;
}
