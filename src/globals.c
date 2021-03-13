#include "globals.h"

tp_t            *tp;
pthread_mutex_t  roots_mtx = PTHREAD_MUTEX_INITIALIZER;
array_t          roots;
pthread_mutex_t  global_scope_mtx = PTHREAD_MUTEX_INITIALIZER;
scope_t         *global_scope;
pthread_mutex_t  lines_mtx = PTHREAD_MUTEX_INITIALIZER;
u64              n_lines;
u64              n_blank_lines;
ast_assign_t    *program_entry;
pthread_mutex_t  program_entry_mtx = PTHREAD_MUTEX_INITIALIZER;
