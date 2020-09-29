#ifndef __FILE_H__
#define __FILE_H__

#include "internal.h"

#define FILE_PATH_MAX (4096)

typedef struct {
    char  path[FILE_PATH_MAX];
    char *buff;
    char *cursor;
    u64   len;
    int   free_buff;
} file_t;


int checked_open_FILE(const char *path, const char *mode, FILE **f, u64 *file_size);
int copy_file_into_memory(const char *path, file_t *file);
int map_file_into_readonly_memory(const char *path, file_t *file);

#endif
