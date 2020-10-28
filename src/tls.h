#ifndef __TLS_H__
#define __TLS_H__

#include "internal.h"
#include "bump_alloc.h"

#ifndef MAX_NUM_TLS
#define MAX_NUM_TLS (1024ULL)
#endif

typedef struct {
    bump_alloc_t bump_alloc;
} tls_t;

typedef struct {
    int           is_valid;
    u64           id;
    u32           ref_count;
    pthread_key_t key;

    tls_t         tls;
} tls_entry_t;

void    init_tls(void);
tls_t * get_tls(void);

#endif
