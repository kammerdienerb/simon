#ifndef __FILE_H__
#define __FILE_H__

#include "internal.h"
#include "strings.h"

typedef struct {
    string_id  path_id;
    char      *buff;
    char      *end;
    char      *cursor;
    u64        len;
    int        free_buff;
} file_t;

#define FILE_NO_ERR  (0)
#define FILE_ERR_NOF (1)
#define FILE_ERR_DIR (2)
#define FILE_ERR_PER (3)
#define FILE_ERR_MAP (4)
#define FILE_ERR_UNK (5)

int checked_open_FILE(const char *path, const char *mode, FILE **f, u64 *file_size);
int copy_file_into_memory(const char *path, file_t *file);
int map_file_into_readonly_memory(const char *path, file_t *file);

#endif
