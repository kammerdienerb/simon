#ifndef __FILE_H__
#define __FILE_H__

#include "internal.h"
#include "strings.h"

typedef struct {
    string_id  path_id;
    char      *buff;
    char      *end;
    u64        len;
    int        free_buff;
} ifile_t;

#define FILE_NO_ERR  (0)
#define FILE_ERR_NOF (1)
#define FILE_ERR_DIR (2)
#define FILE_ERR_PER (3)
#define FILE_ERR_MAP (4)
#define FILE_ERR_UNK (5)

void init_ifile_table(void);
ifile_t * add_ifile_readonly(string_id path_id);
ifile_t * add_ifile_rw(string_id path_id);
ifile_t * get_ifile(string_id path_id);
int num_ifiles(void);
int checked_open_FILE(const char *path, const char *mode, FILE **f, u64 *file_size);

#endif
