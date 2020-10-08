#include "ui.h"
#include "strings.h"

static pthread_mutex_t output_mtx = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_OUTPUT()   (pthread_mutex_lock(&output_mtx))
#define UNLOCK_OUTPUT() (pthread_mutex_unlock(&output_mtx))

static void common_exit(int status) {
    exit(status);
}

void report_vague_err(const char *msg) {
    LOCK_OUTPUT();
    printf("error: %s\n", msg);
    common_exit(1);
    UNLOCK_OUTPUT();
}

void report_file_err(file_t *file, int err) {
    const char *path;
    const char *err_str;

    LOCK_OUTPUT();

    path = get_string(file->path_id);

    switch (err) {
        case FILE_NO_ERR:  err_str = "no error???";               break;
        case FILE_ERR_NOF: err_str = "file not found";            break;
        case FILE_ERR_DIR: err_str = "file is a directory";       break;
        case FILE_ERR_PER: err_str = "permission to read denied"; break;
        case FILE_ERR_MAP: err_str = "file mapping failed";       break;
        default:           err_str = "unknown error";             break;
    }

    printf("error: file '%s': %s\n", path, err_str);

    common_exit(err);

    UNLOCK_OUTPUT();
}
