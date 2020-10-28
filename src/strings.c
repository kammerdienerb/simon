#include "strings.h"

#include "hash_table.h"
#include "hash_utilities.h"

typedef const char *string_t;
typedef struct {} empty_t;

use_hash_table(string_t, empty_t);

static hash_table(string_t, empty_t) string_table;
static pthread_rwlock_t              string_table_lock = PTHREAD_RWLOCK_INITIALIZER;

#define S_TO_ID(s)  ((string_id)(void*)(s))
#define ID_TO_S(id) ((string_t)(void*)(id))

void init_strings(void) {
    string_table = hash_table_make_e(string_t, empty_t, str_hash, str_equ);
}

string_id get_string_id(const char *string) {
    string_t *lookup;
    char     *copy;

    pthread_rwlock_rdlock(&string_table_lock); {
        if ((lookup = hash_table_get_key(string_table, string))) {
            pthread_rwlock_unlock(&string_table_lock);
            return S_TO_ID(*lookup);
        }
    } pthread_rwlock_unlock(&string_table_lock);

    /*
     * It's possible that two writers get past the first lookup
     * simultaneously.
     * In this case, one writer will win the race and insert the
     * string into the table.
     * If this happens, we need the second writer to check again
     * so that it doesn't insert the string again (overwriting the
     * old value).
     * It should check the table again for the string.
     */
    pthread_rwlock_wrlock(&string_table_lock); {
        if ((lookup = hash_table_get_key(string_table, string))) {
            pthread_rwlock_unlock(&string_table_lock);
            return S_TO_ID(*lookup);
        }
        copy = strdup(string);
        hash_table_insert(string_table, copy, (empty_t){});
    } pthread_rwlock_unlock(&string_table_lock);

    return S_TO_ID(copy);
}

string_id get_string_id_n(const char *string, u64 len) {
    char     *null_term_string;
    string_t *lookup;
    char     *copy;

    null_term_string = alloca(len + 1);
    strncpy(null_term_string, string, len);
    null_term_string[len] = 0;

    pthread_rwlock_rdlock(&string_table_lock); {
        if ((lookup = hash_table_get_key(string_table, null_term_string))) {
            pthread_rwlock_unlock(&string_table_lock);
            return S_TO_ID(*lookup);
        }
    } pthread_rwlock_unlock(&string_table_lock);

    /*
     * It's possible that two writers get past the first lookup
     * simultaneously.
     * In this case, one writer will win the race and insert the
     * string into the table.
     * If this happens, we need the second writer to check again
     * so that it doesn't insert the string again (overwriting the
     * old value).
     * It should check the table again for the string.
     */
    pthread_rwlock_wrlock(&string_table_lock); {
        if ((lookup = hash_table_get_key(string_table, null_term_string))) {
            pthread_rwlock_unlock(&string_table_lock);
            return S_TO_ID(*lookup);
        }
        copy = strdup(null_term_string);
        hash_table_insert(string_table, copy, (empty_t){});
    } pthread_rwlock_unlock(&string_table_lock);

    return S_TO_ID(copy);
}

const char *get_string(string_id id) {
    string_t  string;
#ifdef SIMON_DO_ASSERTIONS
    string_t *lookup;
#endif

    string = ID_TO_S(id);

    ASSERT((lookup = hash_table_get_key(string_table, string)),
           "string_id did not correspond to a valid string in the table");

    return string;
}
