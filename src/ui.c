#include "ui.h"
#include "strings.h"
#include "src_range.h"
#include "options.h"

static int output_is_tty;
static pthread_mutex_t output_mtx = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_OUTPUT()   (pthread_mutex_lock(&output_mtx))
#define UNLOCK_OUTPUT() (pthread_mutex_unlock(&output_mtx))

#define TERM_RESET   "\e[0m"
#define TERM_RED     "\e[31m"
#define TERM_GREEN   "\e[32m"
#define TERM_YELLOW  "\e[33m"
#define TERM_BLUE    "\e[34m"
#define TERM_MAGENTA "\e[35m"
#define TERM_CYAN    "\e[36m"


#define ERR_COLOR   TERM_RED
#define INFO_COLOR  TERM_YELLOW
#define LOC_COLOR   TERM_BLUE
#define RANGE_COLOR TERM_RESET

#define N_CONTEXT_LINES (2)

#define C(c)                          \
do {                                  \
    if (output_is_tty && c != NULL) { \
        printf("%s", c);              \
    }                                 \
} while (0)

void init_ui(void) {
    output_is_tty = isatty(1);
}

void verb_message(const char *fmt, ...) {
    va_list va;

    if (options.verbose) {
        LOCK_OUTPUT();
        va_start(va, fmt);
        vprintf(fmt, va);
        va_end(va);
        UNLOCK_OUTPUT();
    }
}

static void common_exit(int status) {
    exit(status);
}

void _report_simple_err(int should_exit, const char *fmt, ...) {
    va_list va;

    LOCK_OUTPUT();
    C(ERR_COLOR);
    printf("error");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n\n");

    if (should_exit) {
        common_exit(1);
    }
    UNLOCK_OUTPUT();
}

void _report_simple_info(int should_exit, const char *fmt, ...) {
    va_list va;

    LOCK_OUTPUT();
    C(INFO_COLOR);
    printf("info");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n\n");

    if (should_exit) {
        common_exit(1);
    }
    UNLOCK_OUTPUT();
}

static int n_digits(u64 num) {
    int digits;

    digits = 1;
    while (num > 9) {
        num /= 10;
        digits += 1;
    }

    return digits;
}

static void print_range(src_range_t *range, const char *all_color, const char *range_color, const char *loc_color, int n_context_lines) {
    file_t *f;
    int     n_nl;
    char   *context_start;
    char   *context_end;
    char   *p;
    char   *line_start;
    int     underline;
    int     line_nr;
    int     line_nr_digits;
    int     i;


    f = get_file(range->beg.path_id);

    n_nl          = 0;
    context_start = range->beg.buff_ptr;
    while (context_start > f->buff) {
        if (*(context_start - 1) == '\n') {
            n_nl += 1;
            if (n_nl == n_context_lines + 1) { break; }
        }
        context_start -= 1;
    }

    line_nr = range->beg.line - n_nl + (context_start != f->buff);

    n_nl        = 0;
    context_end = range->end.buff_ptr;
    while (context_end < f->end) {
        if (*(context_end) == '\n') {
            n_nl += 1;
            if (n_nl == n_context_lines + 1) { break; }
        }
        context_end += 1;
    }

    line_nr_digits = n_digits(range->end.line + n_nl);

    C(all_color);

    p         = line_start = context_start;
    underline = 0;
    while (p < context_end) {
        if (p == line_start) {
            if (line_nr >= range->beg.line && line_nr <= range->end.line) {
                C(range_color);
                printf("%*d", line_nr_digits, line_nr);
                C(all_color);
            } else {
                C(loc_color);
                printf("%*d", line_nr_digits, line_nr);
                C(all_color);
            }
            C(loc_color);
            printf(" │ ");
            C(all_color);
        }

        if (p == range->beg.buff_ptr) {
            underline = 1;// !output_is_tty;
            C(range_color);
        } else if (p == range->end.buff_ptr) {
            underline = -underline;
            C(all_color);
        }
        putchar(*p);
        if (*p == '\n') {
            if (underline != 0) {
                if (p > line_start) {
                    for (i = 0; i < line_nr_digits + 3; i += 1) { putchar(' '); }
                    while (line_start < p) {
                        if (line_start >= range->beg.buff_ptr
                        &&  line_start  < range->end.buff_ptr) {
                            C(range_color);
                            putchar('^');
                            C(all_color);
                        } else if (*line_start == '\t') {
                            putchar('\t');
                        } else {
                            putchar(' ');
                        }
                        line_start += 1;
                    }
                    if (line_start == range->beg.buff_ptr) {
                        putchar('^');
                    } else {
                        putchar('\n');
                    }
                }
            }

            line_start = p + 1;

            if (underline == -1) { underline = 0; }

            line_nr += 1;
        }
        p += 1;
    }

    if (context_end != f->end || underline) {
        if (*line_start != '\n') { putchar('\n'); }

        if (n_context_lines == 0) {
            for (i = 0; i < line_nr_digits + 3; i += 1) { putchar(' '); }
            C(range_color);
            while (line_start < range->end.buff_ptr) {
                if (line_start >= range->beg.buff_ptr
                &&  line_start  < range->end.buff_ptr) {
                    C(range_color);
                    putchar('^');
                    C(all_color);
                } else if (*line_start == '\t') {
                    putchar('\t');
                } else {
                    putchar(' ');
                }
                line_start += 1;
            }
            C(all_color);
            putchar('\n');
        } else if (line_nr == range->beg.line
               &&  line_nr == range->end.line
               &&  range->beg.col == 1
               &&  range->end.col == 2) {

            for (i = 0; i < line_nr_digits + 3; i += 1) { putchar(' '); }
            C(range_color);
            putchar('^');
            C(all_color);
            putchar('\n');
        }
    }

    C(TERM_RESET);
}

void _report_loc_err(int should_exit, src_point_t pt, const char *fmt, ...) {
    src_range_t range;
    va_list     va;

    range.beg = range.end  = pt;
    range.end.col         += 1;
    range.end.buff_ptr    += 1;

    LOCK_OUTPUT();
    C(LOC_COLOR);
    printf("%s:%d:%d", get_string(pt.path_id), pt.line, pt.col);
    C(TERM_RESET);
    printf(": ");
    C(ERR_COLOR);
    printf("error");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n");

    print_range(&range, RANGE_COLOR, ERR_COLOR, LOC_COLOR, N_CONTEXT_LINES);
    printf("\n");

    if (should_exit) {
        common_exit(1);
    }

    UNLOCK_OUTPUT();
}

void _report_loc_info(int should_exit, src_point_t pt, const char *fmt, ...) {
    src_range_t range;
    va_list     va;

    range.beg = range.end  = pt;
    range.end.col         += 1;
    range.end.buff_ptr    += 1;


    LOCK_OUTPUT();
    C(LOC_COLOR);
    printf("%s:%d:%d", get_string(pt.path_id), pt.line, pt.col);
    C(TERM_RESET);
    printf(": ");
    C(INFO_COLOR);
    printf("info");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n");

    print_range(&range, RANGE_COLOR, INFO_COLOR, LOC_COLOR, N_CONTEXT_LINES);
    printf("\n");

    if (should_exit) {
        common_exit(1);
    }

    UNLOCK_OUTPUT();
}

void _report_range_err(int should_exit, src_range_t *range, const char *fmt, ...) {
    va_list va;

    validate_range(range);

    LOCK_OUTPUT();
    C(LOC_COLOR);
    printf("%s:%d:%d", get_string(range->beg.path_id), range->beg.line, range->beg.col);
    C(TERM_RESET);
    printf(": ");
    C(ERR_COLOR);
    printf("error");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n");

    print_range(range, RANGE_COLOR, ERR_COLOR, LOC_COLOR, N_CONTEXT_LINES);
    printf("\n");

    if (should_exit) {
        common_exit(1);
    }

    UNLOCK_OUTPUT();
}

void _report_range_info(int should_exit, src_range_t *range, const char *fmt, ...) {
    va_list va;

    validate_range(range);

    LOCK_OUTPUT();
    C(LOC_COLOR);
    printf("%s:%d:%d", get_string(range->beg.path_id), range->beg.line, range->beg.col);
    C(TERM_RESET);
    printf(": ");
    C(INFO_COLOR);
    printf("info");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n");

    print_range(range, RANGE_COLOR, INFO_COLOR, LOC_COLOR, N_CONTEXT_LINES);
    printf("\n");

    if (should_exit) {
        common_exit(1);
    }

    UNLOCK_OUTPUT();
}

void _report_loc_err_no_context(int should_exit, src_point_t pt, const char *fmt, ...) {
    src_range_t range;
    va_list     va;

    range.beg = range.end  = pt;
    range.end.col         += 1;
    range.end.buff_ptr    += 1;

    LOCK_OUTPUT();
    C(LOC_COLOR);
    printf("%s:%d:%d", get_string(pt.path_id), pt.line, pt.col);
    C(TERM_RESET);
    printf(": ");
    C(ERR_COLOR);
    printf("error");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n");

    print_range(&range, RANGE_COLOR, ERR_COLOR, LOC_COLOR, 0);
    printf("\n");

    if (should_exit) {
        common_exit(1);
    }

    UNLOCK_OUTPUT();
}

void _report_loc_info_no_context(int should_exit, src_point_t pt, const char *fmt, ...) {
    src_range_t range;
    va_list     va;

    range.beg = range.end  = pt;
    range.end.col         += 1;
    range.end.buff_ptr    += 1;


    LOCK_OUTPUT();
    C(LOC_COLOR);
    printf("%s:%d:%d", get_string(pt.path_id), pt.line, pt.col);
    C(TERM_RESET);
    printf(": ");
    C(INFO_COLOR);
    printf("info");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n");

    print_range(&range, RANGE_COLOR, INFO_COLOR, LOC_COLOR, 0);
    printf("\n");

    if (should_exit) {
        common_exit(1);
    }

    UNLOCK_OUTPUT();
}

void _report_range_err_no_context(int should_exit, src_range_t *range, const char *fmt, ...) {
    va_list va;

    validate_range(range);

    LOCK_OUTPUT();
    C(LOC_COLOR);
    printf("%s:%d:%d", get_string(range->beg.path_id), range->beg.line, range->beg.col);
    C(TERM_RESET);
    printf(": ");
    C(ERR_COLOR);
    printf("error");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n");

    print_range(range, RANGE_COLOR, ERR_COLOR, LOC_COLOR, 0);
    printf("\n");

    if (should_exit) {
        common_exit(1);
    }

    UNLOCK_OUTPUT();
}

void _report_range_info_no_context(int should_exit, src_range_t *range, const char *fmt, ...) {
    va_list va;

    validate_range(range);

    LOCK_OUTPUT();
    C(LOC_COLOR);
    printf("%s:%d:%d", get_string(range->beg.path_id), range->beg.line, range->beg.col);
    C(TERM_RESET);
    printf(": ");
    C(INFO_COLOR);
    printf("info");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n");

    print_range(range, RANGE_COLOR, INFO_COLOR, LOC_COLOR, 0);
    printf("\n");

    if (should_exit) {
        common_exit(1);
    }

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

    C(ERR_COLOR);
    printf("error");
    C(TERM_RESET);
    printf(": file '%s': %s\n", path, err_str);

    common_exit(err);

    UNLOCK_OUTPUT();
}
