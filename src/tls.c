#include "tls.h"
#include "platform.h"
#include "ui.h"

static void init_tls_data(tls_t *tls) {
    bump_alloc_make(&tls->bump_alloc);
    tls->is_initialized = 1;
}

#if TLS_METHOD == TLS_PER_OS_THREAD

static __thread tls_t local_tls;

void init_tls(void) {
    verb_message("TLS_METHOD = TLS_PER_OS_THREAD\n");
}

tls_t * get_tls(void) {
    tls_t *local = &local_tls;

    if (unlikely(!local->is_initialized)) {
        init_tls_data(local);
    }

    return local;
}

#endif
#if TLS_METHOD == TLS_PER_HW_THREAD
static tls_entry_t           tls_entry_array[MAX_NUM_TLS];
static pthread_mutex_t       tls_entry_array_lock;
static u32                   num_threads;
static __thread tls_entry_t *local_tls_entry;

void init_tls(void) {
    verb_message("TLS_METHOD = TLS_PER_HW_THREAD\n");
    pthread_mutex_init(&tls_entry_array_lock, NULL);
    num_threads = platform_get_num_hw_threads() + 1;
}

static void dec_tls_entry_ref(void *_tls_entry) {
    tls_entry_t *tls_entry;

    tls_entry = _tls_entry;

    pthread_mutex_lock(&tls_entry_array_lock); {
        ASSERT(tls_entry->ref_count > 0, "invalid tls_entry_t ref_count");
        tls_entry->ref_count -= 1;
    } pthread_mutex_unlock(&tls_entry_array_lock);
}

static u32 get_next_thread_idx(void) {
    u32 i;
    int all_same;
    u32 min_ref;
    u32 min_ref_idx;

    /* Try to find the tls_entry_t with the lowest ref count. */
    all_same    = 1;
    min_ref     = tls_entry_array[0].ref_count;
    min_ref_idx = 0;
    for (i = 1; i < num_threads; i += 1) {
        if (tls_entry_array[i].ref_count != min_ref) {
            all_same = 0;
        }
        if (tls_entry_array[i].ref_count < min_ref) {
            min_ref     = tls_entry_array[i].ref_count;
            min_ref_idx = i;
        }
    }

    if (all_same) { return 0; }

    return min_ref_idx;
}

static tls_entry_t * find_tls_entry(void) {
    int          thread_idx;
    tls_entry_t *tls_entry;

    pthread_mutex_lock(&tls_entry_array_lock); {
        thread_idx = get_next_thread_idx();

        ASSERT(thread_idx != -1,         "failed to get thread index");
        ASSERT(thread_idx < MAX_NUM_TLS, "all tls locations taken");

        tls_entry = tls_entry_array + thread_idx;

        if (unlikely(!tls_entry->is_valid)) {
            tls_entry->is_valid  = 1;
            tls_entry->id        = thread_idx;
            tls_entry->ref_count = 0;

            init_tls_data(&tls_entry->tls);
        }

        tls_entry->ref_count += 1;
    } pthread_mutex_unlock(&tls_entry_array_lock);

    return tls_entry;
}

tls_t * get_tls(void) {
    if (unlikely(local_tls_entry == NULL)) {
        local_tls_entry = find_tls_entry();
        pthread_key_create(&local_tls_entry->key, dec_tls_entry_ref);
        pthread_setspecific(local_tls_entry->key, (void*)local_tls_entry);
    }

    return &local_tls_entry->tls;
}
#endif
