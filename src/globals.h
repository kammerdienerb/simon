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

extern pthread_mutex_t  program_entry_mtx;
extern ast_assign_t    *program_entry;

#define PROGRAM_ENTRY_LOCK()                     \
do {                                             \
    if (tp != NULL) {                            \
        pthread_mutex_lock(&program_entry_mtx);  \
    }                                            \
} while (0)

#define PROGRAM_ENTRY_UNLOCK()                   \
do {                                             \
    if (tp != NULL) {                            \
        pthread_mutex_unlock(&program_entry_mtx);\
    }                                            \
} while (0)


#endif
