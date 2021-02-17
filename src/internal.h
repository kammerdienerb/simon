#ifndef __INTERNAL_H__
#define __INTERNAL_H__

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <math.h>
#include <errno.h>

#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/sysinfo.h>
#include <sys/time.h>
#include <unistd.h>
#include <pthread.h>
#include <alloca.h>

typedef struct {} empty_t;

#define likely(x)   (__builtin_expect(!!(x), 1))
#define unlikely(x) (__builtin_expect(!!(x), 0))

#define INTERNAL_ERROR(...)                           \
do {                                                  \
    fprintf(stderr, "INTERNAL ERROR: " __VA_ARGS__);  \
    exit(1);                                          \
} while (0)

#ifdef SIMON_DO_ASSERTIONS
void simon_assert_fail(const char *msg, const char *fname, int line, const char *cond_str);
#define ASSERT(cond, msg)                              \
do { if (unlikely(!(cond))) {                          \
    simon_assert_fail(msg, __FILE__, __LINE__, #cond); \
} } while (0)
#else
#define ASSERT(cond, mst) ((void)(cond));
#endif

#define _XSTR(x) #x
#define XSTR(x)  _XSTR(x)

#define MAX(a, b) ((a) >= (b) ? (a) : (b))
#define MIN(a, b) ((a) <= (b) ? (a) : (b))
#define LIMIT(x, lower, upper) do { \
    if ((x) < (lower)) {            \
        (x) = (lower);              \
    } else if ((x) > (upper)) {     \
        (x) = (upper);              \
    }                               \
} while (0)

/* @incomplete */
#define ERR(msg) ASSERT(0, #msg)


#define UINT(w) uint##w##_t
#define SINT(w) int##w##_t

#define u8  UINT(8 )
#define u16 UINT(16)
#define u32 UINT(32)
#define u64 UINT(64)

#define i8  SINT(8 )
#define i16 SINT(16)
#define i32 SINT(32)
#define i64 SINT(64)

#define ALIGN(x, align)      ((__typeof(x))((((u64)(x)) + (((u64)align) - 1ULL)) & ~(((u64)align) - 1ULL)))
#define IS_ALIGNED(x, align) (!(((u64)(x)) & (((u64)align) - 1ULL)))
#define IS_POWER_OF_TWO(x)   ((x) != 0 && IS_ALIGNED((x), (x)))

#define KB(x) ((x) * 1024ULL)
#define MB(x) ((x) * 1024ULL * KB(1ULL))
#define GB(x) ((x) * 1024ULL * MB(1ULL))
#define TB(x) ((x) * 1024ULL * GB(1ULL))

u64 next_power_of_2(u64 x);
char *pretty_bytes(u64 n_bytes);
u64 measure_time_now_ms(void);
u64 measure_time_now_us(void);

#endif
