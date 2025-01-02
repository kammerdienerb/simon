#include "strings.h"
#include "memory.h"
#include "tls.h"
#include "array.h"
#include "globals.h"
#include "ui.h"


string_id ELLIPSIS_ID;
string_id _BUILTIN_PRINTS_ID;
string_id _BUILTIN_PRINTP_ID;
string_id _BUILTIN_PRINTI_ID;
string_id _BUILTIN_STACK_ALLOC_ID;
string_id _BUILTIN_VARG_ID;
string_id _BUILTIN_SLICE_FROM_ID;
string_id CAST_ID;
string_id EXTERN_ID;
string_id PROGRAM_ENTRY_ID;
string_id MACRO_PUBLIC_ID;
string_id BITFIELD_STRUCT_ID;
string_id BITFIELD_ID;
string_id SPECIALIZATION_ID;
string_id COMPILE_ERROR_ID;
string_id REQUIRE_ID;
string_id STATIC_IF_ID;
string_id VARGS_ID;
string_id CODE_TO_STRING_ID;

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
        "struct",
        "do",    "if",     "else",   "loop", "break", "defer", "return", "continue",
    };
    string_id   kwd_id;

#if STRING_INTERN_STRUCTURE == STRING_HASH_TABLE
/*     verb_message("using a hash table for string interning\n"); */
    string_table = hash_table_make_e(string_t, empty_t, str_hash, str_equ);
#elif STRING_INTERN_STRUCTURE == STRING_RB_TREE
/*     verb_message("using a red/black tree for string interning\n"); */
    string_table = tree_make(string_t, empty_t);
#endif
    kwd_ids = array_make(string_id);

    for (i = 0; i < sizeof(kwds) / sizeof(kwds[0]); i += 1) {
        kwd_id = get_string_id(kwds[i]);
        array_push(kwd_ids, kwd_id);
    }

    ELLIPSIS_ID             = get_string_id("...");
    _BUILTIN_PRINTS_ID      = get_string_id("_builtin_prints");
    _BUILTIN_PRINTP_ID      = get_string_id("_builtin_printp");
    _BUILTIN_PRINTI_ID      = get_string_id("_builtin_printi");
    _BUILTIN_STACK_ALLOC_ID = get_string_id("_builtin_stack_alloc");
    _BUILTIN_VARG_ID        = get_string_id("_builtin_varg");
    _BUILTIN_SLICE_FROM_ID  = get_string_id("_builtin_slice_from");
    CAST_ID                 = get_string_id("cast");
    EXTERN_ID               = get_string_id("extern");
    PROGRAM_ENTRY_ID        = get_string_id("program_entry");
    MACRO_PUBLIC_ID         = get_string_id("macro_public");
    BITFIELD_STRUCT_ID      = get_string_id("bitfield_struct");
    BITFIELD_ID             = get_string_id("bitfield");
    SPECIALIZATION_ID       = get_string_id("specialization");
    COMPILE_ERROR_ID        = get_string_id("compile_error");
    REQUIRE_ID              = get_string_id("require");
    STATIC_IF_ID            = get_string_id("static_if");
    VARGS_ID                = get_string_id("vargs");
    CODE_TO_STRING_ID       = get_string_id("code_to_string");
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

    ASSERT(id != 0, "NULL ID!");

    string = ID_TO_S(id);

    /* Is this actually in the table? */
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


array_t sh_split(char *s) {
    array_t  r;
    char    *copy,
            *sub,
            *sub_p;
    char     c, prev;
    int      len,
             start,
             end,
             q,
             sub_len,
             i;

    r     = array_make(char*);
    copy  = strdup(s);
    len   = strlen(copy);
    start = 0;
    end   = 0;
    prev  = 0;

    while (start < len && isspace(copy[start])) { start += 1; }

    while (start < len) {
        c   = copy[start];
        q   = 0;
        end = start;

        if (c == '#' && prev != '\\') {
            break;
        } else if (c == '"') {
            start += 1;
            prev   = copy[end];
            while (end + 1 < len
            &&    (copy[end + 1] != '"' || prev == '\\')) {
                end += 1;
                prev = copy[end];
            }
            q = 1;
        } else if (c == '\'') {
            start += 1;
            prev   = copy[end];
            while (end + 1 < len
            &&    (copy[end + 1] != '\'' || prev == '\\')) {
                end += 1;
                prev = copy[end];
            }
            q = 1;
        } else {
            while (end + 1 < len
            &&     !isspace(copy[end + 1])) {
                end += 1;
            }
        }

        sub_len = end - start + 1;
        if (q && sub_len == 0 && start == len) {
            sub    = malloc(2);
            sub[0] = copy[end];
            sub[1] = 0;
        } else {
            sub   = malloc(sub_len + 1);
            sub_p = sub;
            for (i = 0; i < sub_len; i += 1) {
                c = copy[start + i];
                if (c == '\\'
                &&  i < sub_len - 1
                &&  (copy[start + i + 1] == '"'
                || copy[start + i + 1] == '\''
                || copy[start + i + 1] == '#')) {
                    continue;
                }
                *sub_p = c;
                sub_p += 1;
            }
            *sub_p = 0;
        }

        array_push(r, sub);

        end  += q;
        start = end + 1;

        while (start < len && isspace(copy[start])) { start += 1; }
    }

    free(copy);

    return r;
}
