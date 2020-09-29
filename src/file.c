#include "file.h"

#define FILE_NO_ERR  (0)
#define FILE_ERR_NOF (1)
#define FILE_ERR_DIR (2)
#define FILE_ERR_PER (3)
#define FILE_ERR_MAP (4)
#define FILE_ERR_UNK (5)

int checked_open_FILE(const char *path, const char *mode, FILE **f, u64 *file_size) {
    struct stat fs;
    int         fd;
    int         status;

    status = FILE_NO_ERR;
    errno  = 0;
    *f     = fopen(path, mode);
    if (*f) {
        fd = fileno(*f);
        if (fstat(fd, &fs) != 0) {
            errno  = 0;
            status = FILE_ERR_NOF;
            goto out;
        } else if (S_ISDIR(fs.st_mode)) {
            errno  = 0;
            status = FILE_ERR_DIR;
            goto out;
        }

        *file_size = fs.st_size;
    }

    if (errno) {
        switch (errno) {
            case ENOENT: status = FILE_ERR_NOF; break;
            case EISDIR: status = FILE_ERR_DIR; break;
            case EACCES: status = FILE_ERR_PER; break;
            default:     status = FILE_ERR_UNK; break;
        }
        errno = 0;
    }

out:
    return status;
}

int copy_file_into_memory(const char *path, file_t *file) {
    int   status;
    FILE *f;
    int   fd;
    void *file_data;

    status = checked_open_FILE(path, "r", &f, &file->len);
    if (status) { goto out; }

    fd        = fileno(f);
    file_data = mmap(NULL, file->len, PROT_READ, MAP_SHARED, fd, 0);

    if (file_data == MAP_FAILED) {
        status = FILE_ERR_MAP;
        goto out;
    }

    file->buff      = malloc(file->len);
    file->free_buff = 1;
    file->cursor    = file->buff;

    memcpy(file->buff, file_data, file->len);

    munmap(file_data, file->len);
    fclose(f);

    strcpy(file->path, path);

out:
    return status;
}

int map_file_into_readonly_memory(const char *path, file_t *file) {
    int   status;
    FILE *f;
    int   fd;

    status = checked_open_FILE(path, "r", &f, &file->len);
    if (status) { goto out; }

    fd         = fileno(f);
    file->buff = mmap(NULL, file->len, PROT_READ, MAP_SHARED, fd, 0);

    if (file->buff == MAP_FAILED) {
        status = FILE_ERR_MAP;
        goto out;
    }

    file->free_buff = 0;
    file->cursor    = file->buff;

    fclose(f);

    strcpy(file->path, path);

out:
    return status;
}
