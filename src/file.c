#include "file.h"

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
    int   i;

    file->path_id = get_string_id(path);

    status = checked_open_FILE(path, "r", &f, &file->len);
    if (status) { goto out; }

    if (file->len == 0) {
        file->buff = file->end = file->cursor = NULL;
        file->free_buff = 0;
        goto out_fclose;
    }

    fd        = fileno(f);
    file_data = mmap(NULL, file->len, PROT_READ, MAP_SHARED, fd, 0);

    if (file_data == MAP_FAILED) {
        status = FILE_ERR_MAP;
        goto out;
    }

    /*
     * About the ALIGN:
     * It can be beneficial for our file buffers to be aligned of 4 byte
     * boundaries for some parsing optimizations (like checking for 4
     * spaces at a time).
     * So, we just allocate a few extra bytes if needed and fill them with
     * zeros.
     * In the map_file_into_readonly_memory() version of this function,
     * this isn't necessary because mappings are page-aligned and zero-filled.
     */
    file->buff      = malloc(ALIGN(file->len, 4));
    file->free_buff = 1;
    file->end       = file->buff + file->len;
    file->cursor    = file->buff;

    for (i = 0; i < ALIGN(file->len, 4) - file->len; i += 1) {
        *(file->end + i) = 0;
    }

    memcpy(file->buff, file_data, file->len);

    munmap(file_data, file->len);

out_fclose:
    fclose(f);

out:
    return status;
}

int map_file_into_readonly_memory(const char *path, file_t *file) {
    int   status;
    FILE *f;
    int   fd;

    file->path_id = get_string_id(path);

    status = checked_open_FILE(path, "r", &f, &file->len);
    if (status) { goto out; }

    if (file->len == 0) {
        file->buff = file->end = file->cursor = NULL;
        file->free_buff = 0;
        goto out_fclose;
    }

    fd         = fileno(f);
    file->buff = mmap(NULL, file->len, PROT_READ, MAP_SHARED, fd, 0);

    if (file->buff == MAP_FAILED) {
        status = FILE_ERR_MAP;
        goto out;
    }

    file->free_buff = 0;
    file->end       = file->buff + file->len;
    file->cursor    = file->buff;

out_fclose:
    fclose(f);

out:
    return status;
}
