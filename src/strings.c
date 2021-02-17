#include "strings.h"
#include "memory.h"
#include "tls.h"
#include "array.h"
#include "globals.h"
#include "ui.h"


static char *cstr_dup(const char *str) {
    u64   len;
    char *dup;

    len = strlen(str);
    dup = mem_alloc(len + 1);
    memcpy(dup, str, len + 1);

    return dup;
}

/* @todo replace strcmp? */



#if STRING_INTERN_STRUCTURE == STRING_HASH_TABLE
    #include "hash_table.h"
    #include "hash_utilities.h"
    use_hash_table(string_t, empty_t);
    static hash_table(string_t, empty_t) string_table;
#elif STRING_INTERN_STRUCTURE == STRING_RB_TREE
    #include "tree.h"
    use_tree_c(string_t, empty_t, strcmp);
    static tree(string_t, empty_t) string_table;
#else
    #error "invalid STRING_INTERN_STRUCTURE"
#endif

static pthread_rwlock_t string_table_lock = PTHREAD_RWLOCK_INITIALIZER;
static array_t          kwd_ids;

#define S_TO_ID(s)  ((string_id)(void*)(s))
#define ID_TO_S(id) ((string_t)(void*)(id))

#define RLOCK()                                    \
do {                                               \
    if (tp != NULL) {                              \
        pthread_rwlock_rdlock(&string_table_lock); \
    }                                              \
} while (0)

#define WLOCK()                                    \
do {                                               \
    if (tp != NULL) {                              \
        pthread_rwlock_wrlock(&string_table_lock); \
    }                                              \
} while (0)

#define UNLOCK()                                   \
do {                                               \
    if (tp != NULL) {                              \
        pthread_rwlock_unlock(&string_table_lock); \
    }                                              \
} while (0)

void init_strings(void) {
    int         i;
    const char *kwds[] = {
        "or",
        "and",   "not",
        "proc",
        "macro", "module", "struct",
        "do",    "if",     "else",   "loop", "break", "defer", "return", "continue",
    };
    string_id   kwd_id;

#if STRING_INTERN_STRUCTURE == STRING_HASH_TABLE
    verb_message("using a hash table for string interning\n");
    string_table = hash_table_make_e(string_t, empty_t, str_hash, str_equ);
#elif STRING_INTERN_STRUCTURE == STRING_RB_TREE
    verb_message("using a red/black tree for string interning\n");
    string_table = tree_make(string_t, empty_t);
#endif
    kwd_ids = array_make(string_id);

    for (i = 0; i < sizeof(kwds) / sizeof(kwds[0]); i += 1) {
        kwd_id = get_string_id(kwds[i]);
        array_push(kwd_ids, kwd_id);
    }
}

string_id get_string_id(const char *string) {
#if STRING_INTERN_STRUCTURE == STRING_HASH_TABLE
    string_t                   *lookup;
#elif STRING_INTERN_STRUCTURE == STRING_RB_TREE
    tree_it(string_t, empty_t)  it;
    string_t                    lookup;
#endif
    char                       *copy;

    RLOCK(); {
#if STRING_INTERN_STRUCTURE == STRING_HASH_TABLE
        lookup = hash_table_get_key(string_table, string);
#elif STRING_INTERN_STRUCTURE == STRING_RB_TREE
        it     = tree_lookup(string_table, string);
        lookup = tree_it_good(it) ? tree_it_key(it) : NULL;
#endif
        if (lookup != NULL) {
            UNLOCK();
#if STRING_INTERN_STRUCTURE == STRING_HASH_TABLE
            return S_TO_ID(*lookup);
#elif STRING_INTERN_STRUCTURE == STRING_RB_TREE
            return S_TO_ID(lookup);
#endif
        }
    } UNLOCK();

    /*
    ** It's possible that two writers get past the first lookup
    ** simultaneously.
    ** In this case, one writer will win the race and insert the
    ** string into the table.
    ** If this happens, we need the second writer to check again
    ** so that it doesn't insert the string again (overwriting the
    ** old value).
    ** It should check the table again for the string.
    */
    WLOCK(); {
#if STRING_INTERN_STRUCTURE == STRING_HASH_TABLE
        lookup = hash_table_get_key(string_table, string);
#elif STRING_INTERN_STRUCTURE == STRING_RB_TREE
        it     = tree_lookup(string_table, string);
        lookup = tree_it_good(it) ? tree_it_key(it) : NULL;
#endif
        if (lookup != NULL) {
            UNLOCK();
#if STRING_INTERN_STRUCTURE == STRING_HASH_TABLE
            return S_TO_ID(*lookup);
#elif STRING_INTERN_STRUCTURE == STRING_RB_TREE
            return S_TO_ID(lookup);
#endif
        }
        copy = cstr_dup(string);
#if STRING_INTERN_STRUCTURE == STRING_HASH_TABLE
        hash_table_insert(string_table, copy, (empty_t){});
#elif STRING_INTERN_STRUCTURE == STRING_RB_TREE
        tree_insert(string_table, copy, (empty_t){});
#endif
    } UNLOCK();

    return S_TO_ID(copy);
}

string_id get_string_id_n(const char *string, u64 len) {
    char *null_term_string;

    null_term_string = alloca(len + 1);
    memcpy(null_term_string, string, len);
    null_term_string[len] = 0;

    return get_string_id(null_term_string);
}

const char *get_string(string_id id) {
    string_t                    string;
#ifdef SIMON_DO_ASSERTIONS
#if STRING_INTERN_STRUCTURE == STRING_HASH_TABLE
    string_t                   *lookup;
#elif STRING_INTERN_STRUCTURE == STRING_RB_TREE
    tree_it(string_t, empty_t)  it;
    string_t                    lookup;
#endif
#endif

    string = ID_TO_S(id);

#ifdef SIMON_DO_ASSERTIONS
    RLOCK(); {
#if STRING_INTERN_STRUCTURE == STRING_HASH_TABLE
        lookup = hash_table_get_key(string_table, string);
#elif STRING_INTERN_STRUCTURE == STRING_RB_TREE
        it     = tree_lookup(string_table, string);
        lookup = tree_it_good(it) ? tree_it_key(it) : NULL;
#endif
        ASSERT(lookup != NULL, "string_id did not correspond to a valid string in the table");
    } UNLOCK();
#endif

    return string;
}

int is_kwd(string_id id) {
    string_id *it;

    array_traverse(kwd_ids, it) {
        if (id == *it) { return 1; }
    }

    return 0;
}

void print_all_strings(void) {
#if STRING_INTERN_STRUCTURE == STRING_RB_TREE
    tree_it(string_t, empty_t)  it;
#endif
    const char                 *key;
    empty_t                    *val;

    (void)val;
    (void)key;

#if STRING_INTERN_STRUCTURE == STRING_HASH_TABLE
    hash_table_traverse(string_table, key, val) {
        (void)val;
        printf("%s\n", key);
    }
#elif STRING_INTERN_STRUCTURE == STRING_RB_TREE
    tree_traverse(string_table, it) {
        printf("%s\n", tree_it_key(it));
    }
#endif
}
