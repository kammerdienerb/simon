#ifndef __GLOBALS_H__
#define __GLOBALS_H__

#include "internal.h"
#include "threadpool.h"
#include "array.h"
#include "options.h"
#include "scope.h"
#include "ast.h"

extern tp_t            *tp;
extern pthread_mutex_t  roots_mtx;
extern array_t          roots;
extern pthread_mutex_t  macro_calls_mtx;
extern array_t          macro_calls;
extern ast_decl_t      *program_entry;
extern array_t          all_types;
extern array_t          all_procs;
extern array_t          all_vars;

#define ROOTS_LOCK()                             \
do {                                             \
    if (tp != NULL) {                            \
        pthread_mutex_lock(&roots_mtx);          \
    }                                            \
} while (0)

#define ROOTS_UNLOCK()                           \
do {                                             \
    if (tp != NULL) {                            \
        pthread_mutex_unlock(&roots_mtx);        \
    }                                            \
} while (0)

#define MACRO_CALLS_LOCK()                       \
do {                                             \
    if (tp != NULL) {                            \
        pthread_mutex_lock(&macro_calls_mtx);    \
    }                                            \
} while (0)

#define MACRO_CALLS_UNLOCK()                     \
do {                                             \
    if (tp != NULL) {                            \
        pthread_mutex_unlock(&macro_calls_mtx);  \
    }                                            \
} while (0)

extern pthread_mutex_t  global_scope_mtx;
extern scope_t         *global_scope;

#define GS_LOCK()                                \
do {                                             \
    if (tp != NULL) {                            \
        pthread_mutex_lock(&global_scope_mtx);   \
    }                                            \
} while (0)

#define GS_UNLOCK()                              \
do {                                             \
    if (tp != NULL) {                            \
        pthread_mutex_unlock(&global_scope_mtx); \
    }                                            \
} while (0)

extern pthread_mutex_t  lines_mtx;
extern u64              n_lines;
extern u64              n_blank_lines;

#define LINES_LOCK()                             \
do {                                             \
    if (tp != NULL) {                            \
        pthread_mutex_lock(&lines_mtx);          \
    }                                            \
} while (0)

#define LINES_UNLOCK()                           \
do {                                             \
    if (tp != NULL) {                            \
        pthread_mutex_unlock(&lines_mtx);        \
    }                                            \
} while (0)


extern FILE *output_file;

#endif
