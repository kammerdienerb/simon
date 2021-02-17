#ifndef __TLS_H__
#define __TLS_H__

#include "internal.h"
#include "memory.h"

typedef struct {
    int          is_initialized;
    bump_alloc_t bump_alloc;
} tls_t;

#define TLS_PER_HW_THREAD (1)
#define TLS_PER_OS_THREAD (2)

#ifndef TLS_METHOD
#define TLS_METHOD TLS_PER_HW_THREAD
#endif

#if TLS_METHOD == TLS_PER_HW_THREAD

#ifndef MAX_NUM_TLS
#define MAX_NUM_TLS (1024ULL)
#endif

typedef struct {
    int           is_valid;
    u64           id;
    u32           ref_count;
    pthread_key_t key;

    tls_t         tls;
} tls_entry_t;

#endif /* TLS_METHOD == TLS_PER_HW_THREAD */

#if TLS_METHOD == TLS_PER_OS_THREAD
    /* Nothing special needed. Will use __thread. */
#endif

void    init_tls(void);
tls_t * get_tls(void);

#endif
