#include "internal.h"

#ifdef SIMON_DO_ASSERTIONS
void simon_assert_fail(const char *msg, const char *fname, int line, const char *cond_str) {
    volatile int *trap;

    fprintf(stderr, "Assertion failed -- %s\n"
                    "at  %s :: line %d\n"
                    "    Condition: '%s'\n",
                    msg, fname, line, cond_str);

    trap = 0;
    (void)*trap;
}
#endif


u64 next_power_of_2(u64 x) {
    if (x == 0) {
        return 2;
    }

    x--;
    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;
    x |= x >> 32;
    x++;
    return x;
}

static __thread char pretty_bytes_buff[64];

char * pretty_bytes(u64 n_bytes) {
    u64         s;
    double      count;
    char       *r;
    const char *suffixes[]
        = { " B", " KB", " MB", " GB", " TB", " PB", " EB" };

    s     = 0;
    count = (double)n_bytes;

    while (count >= 1024 && s < 7) {
        s     += 1;
        count /= 1024;
    }

    r    = pretty_bytes_buff;
    r[0] = 0;

    if (count - floor(count) == 0.0) {
        sprintf(r, "%d", (int)count);
    } else {
        sprintf(r, "%.2f", count);
    }

    strcat(r, suffixes[s]);

    return r;
}

u64 measure_time_now_ms(void) {
    struct timeval tv;

    gettimeofday(&tv, NULL);

    return 1000ULL * tv.tv_sec + (tv.tv_usec / 1000ULL);
}

u64 measure_time_now_us(void) {
    struct timeval tv;

    gettimeofday(&tv, NULL);

    return 1000000ULL * tv.tv_sec + (tv.tv_usec);
}
