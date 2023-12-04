#include "ui.h"
#include "strings.h"
#include "src_range.h"
#include "options.h"
#include "array.h"
#include "type.h"
#include "globals.h"

static int output_is_tty;
static pthread_mutex_t output_mtx = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_OUTPUT()   (pthread_mutex_lock(&output_mtx))
#define UNLOCK_OUTPUT() (pthread_mutex_unlock(&output_mtx))

static array_t breadcrumbs;

#define TERM_RESET   "\e[0m"
#define TERM_RED     "\e[31m"
#define TERM_GREEN   "\e[32m"
#define TERM_YELLOW  "\e[33m"
#define TERM_BLUE    "\e[34m"
#define TERM_MAGENTA "\e[35m"
#define TERM_CYAN    "\e[36m"
#define TERM_WHITE   "\e[37m"
#define TERM_BOLD    "\e[1m"

#define ERR_ATTR   TERM_BOLD
#define INFO_ATTR  ""
#define FIXIT_ATTR ""
#define LOC_ATTR   ""
#define RANGE_ATTR TERM_RESET

#define ERR_COLOR   TERM_RED
#define INFO_COLOR  TERM_YELLOW
#define FIXIT_COLOR TERM_GREEN
#define LOC_COLOR   TERM_BLUE
#define RANGE_COLOR TERM_CYAN

#define ERR_STYLE   ERR_ATTR ERR_COLOR
#define INFO_STYLE  INFO_ATTR INFO_COLOR
#define FIXIT_STYLE FIXIT_ATTR FIXIT_COLOR
#define LOC_STYLE   LOC_ATTR LOC_COLOR
#define RANGE_STYLE RANGE_ATTR RANGE_COLOR

#define N_CONTEXT_LINES (2)

#define C(c)                          \
do {                                  \
    if (output_is_tty && c != NULL) { \
        printf("%s", c);              \
    }                                 \
} while (0)

void init_ui(void) {
    breadcrumbs = array_make(breadcrumb_t);
}

void set_output_is_tty(void) {
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
        fflush(stdout);
    }
}

static void print_breadcrumbs(void) {
    int           seen_macro;
    breadcrumb_t *it;
    string_id     name;
    src_range_t   loc;

    if (array_len(breadcrumbs) == 0) { return; }

    UNLOCK_OUTPUT();

    seen_macro = 0;
    array_rtraverse(breadcrumbs, it) {
        if (it->kind == BREADCRUMB_RANGE) {
            report_range_info_no_context_no_exit(&it->range, "%s", it->buff);
        } else if (it->kind == BREADCRUMB_POINT) {
            report_loc_info_no_context_no_exit(it->range.beg, "%s", it->buff);
        } else if (it->kind == BREADCRUMB_MACRO) {
            name = ((ast_decl_t*)it->node->macro_decl)->full_name;
            if (!seen_macro) {
                report_range_info_no_exit(&it->node->loc, "during expanion of macro '%s':", get_string(name));
                loc.beg = it->node->macro_decl->loc.beg;
                loc.end = ((ast_decl_t*)it->node->macro_decl)->val_expr->loc.end;
                report_range_info_no_context_no_exit(&loc, "macro '%s', defined here:", get_string(name));
                seen_macro = 1;
            } else {
                report_range_info_no_exit(&it->node->loc, "during expanion of macro '%s':", get_string(name));
            }
        }
    }
}

void common_exit(int status) {
    print_breadcrumbs();

    fflush(output_file);

/*     ASSERT(status == 0, "error on exit"); */
    exit(status);
}

void _report_simple_err(int should_exit, const char *fmt, ...) {
    va_list va;

    LOCK_OUTPUT();
    C(ERR_STYLE);
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
    C(INFO_STYLE);
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

static void print_range(src_range_t *range, const char *all_color, const char *range_color, const char *loc_color, int n_context_lines, int color_source, int trailing_newline) {
    ifile_t *f;
    int      n_nl;
    char    *context_start;
    char    *context_end;
    char    *p;
    char    *line_start;
    int      underline;
    int      line_nr;
    int      line_nr_digits;
    int      i;


    f = get_ifile(range->beg.path_id);

    n_nl          = 0;
    context_start = range->beg.buff_ptr;
    while (context_start > f->buff) {
        if (*(context_start - 1) == '\n' && context_start) {
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

            if (underline && color_source) { C(range_color); }
        }

        if (p == range->beg.buff_ptr) {
            underline = 1;// !output_is_tty;
            if (color_source) {
                C(range_color);
            }
        }
        if (p == range->end.buff_ptr) {
            underline = -underline;
            C(all_color);
        }
        putchar(*p);
        if (*p == '\n') {
            if (underline != 0) {
                if (p > line_start) {
                    for (i = 0; i < line_nr_digits + 3; i += 1) { putchar(' '); }
                    while (line_start < p) {
                        if ((line_start >= range->beg.buff_ptr && line_start < range->end.buff_ptr)
                        ||  (range->beg.buff_ptr == range->end.buff_ptr && line_start == range->beg.buff_ptr)) {
                            if (color_source) { C(range_color); }
                            putchar('^');
                            if (color_source) { C(all_color); }
                        } else if (*line_start == '\t') {
                            putchar('\t');
                        } else {
                            putchar(' ');
                        }
                        line_start += 1;
                    }
                    if (line_start == range->beg.buff_ptr) {
                        if (color_source) { C(range_color); }
                        putchar('^');
                        if (color_source) { C(all_color); }
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
            if (trailing_newline) {
                putchar('\n');
            }
        } else if (line_nr == range->end.line) {
            for (i = 0; i < line_nr_digits + 3; i += 1) { putchar(' '); }
            C(range_color);
            if (range->beg.line == range->end.line) {
                for (i = 1; i < range->beg.col; i += 1) {
                    putchar(' ');
                }
                for (i = range->beg.col; i < range->end.col; i += 1) {
                    putchar('^');
                }
            } else {
                for (i = 1; i < range->end.col; i += 1) {
                    putchar('^');
                }
            }
            C(all_color);
            if (trailing_newline) {
                putchar('\n');
            }
        }
    } else if (context_end == f->end) {
        putchar('\n');
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
    C(LOC_STYLE);
    printf("%s:%d:%d", get_string(pt.path_id), pt.line, pt.col);
    C(TERM_RESET);
    printf(": ");
    C(ERR_STYLE);
    printf("error");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n");

    print_range(&range, RANGE_STYLE, ERR_STYLE, LOC_STYLE, N_CONTEXT_LINES, 1, 1);
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
    C(LOC_STYLE);
    printf("%s:%d:%d", get_string(pt.path_id), pt.line, pt.col);
    C(TERM_RESET);
    printf(": ");
    C(INFO_STYLE);
    printf("info");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n");

    print_range(&range, RANGE_STYLE, INFO_STYLE, LOC_STYLE, N_CONTEXT_LINES, 1, 1);
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
    C(LOC_STYLE);
    printf("%s:%d:%d", get_string(range->beg.path_id), range->beg.line, range->beg.col);
    C(TERM_RESET);
    printf(": ");
    C(ERR_STYLE);
    printf("error");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n");

    print_range(range, RANGE_STYLE, ERR_STYLE, LOC_STYLE, N_CONTEXT_LINES, 1, 1);
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
    C(LOC_STYLE);
    printf("%s:%d:%d", get_string(range->beg.path_id), range->beg.line, range->beg.col);
    C(TERM_RESET);
    printf(": ");
    C(INFO_STYLE);
    printf("info");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n");

    print_range(range, RANGE_STYLE, INFO_STYLE, LOC_STYLE, N_CONTEXT_LINES, 1, 1);
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
    C(LOC_STYLE);
    printf("%s:%d:%d", get_string(pt.path_id), pt.line, pt.col);
    C(TERM_RESET);
    printf(": ");
    C(ERR_STYLE);
    printf("error");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n");

    print_range(&range, RANGE_STYLE, ERR_STYLE, LOC_STYLE, 0, 1, 1);
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
    C(LOC_STYLE);
    printf("%s:%d:%d", get_string(pt.path_id), pt.line, pt.col);
    C(TERM_RESET);
    printf(": ");
    C(INFO_STYLE);
    printf("info");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n");

    print_range(&range, RANGE_STYLE, INFO_STYLE, LOC_STYLE, 0, 1, 1);
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
    C(LOC_STYLE);
    printf("%s:%d:%d", get_string(range->beg.path_id), range->beg.line, range->beg.col);
    C(TERM_RESET);
    printf(": ");
    C(ERR_STYLE);
    printf("error");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n");

    print_range(range, RANGE_STYLE, ERR_STYLE, LOC_STYLE, 0, 1, 1);
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
    C(LOC_STYLE);
    printf("%s:%d:%d", get_string(range->beg.path_id), range->beg.line, range->beg.col);
    C(TERM_RESET);
    printf(": ");
    C(INFO_STYLE);
    printf("info");
    C(TERM_RESET);

    printf(": ");
    va_start(va, fmt);
    vprintf(fmt, va);
    va_end(va);
    printf("\n");

    print_range(range, RANGE_STYLE, INFO_STYLE, LOC_STYLE, 0, 1, 1);
    printf("\n");

    if (should_exit) {
        common_exit(1);
    }

    UNLOCK_OUTPUT();
}

void _report_fixit(int should_exit, src_point_t pt, const char *fmt, ...) {
    char         buff[4096];
    va_list      va;
    char        *brk;
    char         sav_brk;
    src_range_t  range;

    va_start(va, fmt);
    memset(buff, 0, sizeof(buff));
    vsnprintf(buff, sizeof(buff) - 1, fmt, va);
    va_end(va);

    for (brk = buff; *brk && *brk != '\a'; brk += 1);
    sav_brk = *brk;
    if (sav_brk) { *brk = 0; }

    range.beg = range.end  = pt;
    range.end.col         += 1;
    range.end.buff_ptr    += 1;


    LOCK_OUTPUT();
    C(LOC_STYLE);
    printf("%s:%d:%d", get_string(pt.path_id), pt.line, pt.col);
    C(TERM_RESET);
    printf(": ");
    C(FIXIT_STYLE);
    printf("fix-it");
    C(TERM_RESET);
    printf(": ");
    printf("%s\n", buff);
    print_range(&range, RANGE_STYLE, FIXIT_STYLE, LOC_STYLE, 0, 0, 0);
    C(FIXIT_STYLE);
    printf(" %s\n\n", brk + 1);
    C(TERM_RESET);

    *brk = sav_brk;

    if (should_exit) {
        common_exit(1);
    }

    UNLOCK_OUTPUT();
}

void report_file_err(const char *path, int err) {
    const char *err_str;

    LOCK_OUTPUT();

    switch (err) {
        case FILE_NO_ERR:  err_str = "no error???";               break;
        case FILE_ERR_NOF: err_str = "file not found";            break;
        case FILE_ERR_DIR: err_str = "file is a directory";       break;
        case FILE_ERR_PER: err_str = "permission to read denied"; break;
        case FILE_ERR_MAP: err_str = "file mapping failed";       break;
        default:           err_str = "unknown error";             break;
    }

    C(ERR_STYLE);
    printf("error");
    C(TERM_RESET);
    printf(": file '%s': %s\n", path, err_str);

    common_exit(err);

    UNLOCK_OUTPUT();
}

void push_loc_breadcrumb(src_point_t pt, const char *fmt, ...) {
    breadcrumb_t b;
    va_list      va;

    b.kind      = BREADCRUMB_POINT;
    b.range.beg = pt;

    va_start(va, fmt);
    memset(b.buff, 0, sizeof(b.buff));
    vsnprintf(b.buff, sizeof(b.buff) - 1, fmt, va);
    va_end(va);

    array_push(breadcrumbs, b);
}

void push_range_breadcrumb(src_range_t *range, const char *fmt, ...) {
    breadcrumb_t b;
    va_list      va;

    b.kind = BREADCRUMB_RANGE;
    memcpy(&b.range, range, sizeof(b.range));

    va_start(va, fmt);
    memset(b.buff, 0, sizeof(b.buff));
    vsnprintf(b.buff, sizeof(b.buff) - 1, fmt, va);
    va_end(va);

    array_push(breadcrumbs, b);
}

void push_macro_breadcrumb(ast_t *node) {
    breadcrumb_t b;

    ASSERT(node->macro_decl != NULL, "no macro decl!");

    b.kind = BREADCRUMB_MACRO;
    b.node = node;

    array_push(breadcrumbs, b);
}

void pop_breadcrumb(void) {
    if (array_len(breadcrumbs) > 0) {
        array_pop(breadcrumbs);
    }
}

void print_node(ast_t *node) {
    if (node->kind != AST_BUILTIN) {
        C(LOC_STYLE);
        printf("%s:%d:%d", get_string(node->loc.beg.path_id), node->loc.beg.line, node->loc.beg.col);
        C(TERM_RESET);
        printf(":");
    }
    printf("%s @ %p\n", ast_get_kind_str(node->kind), node);
    if (node->kind != AST_BUILTIN) {
        print_range(&node->loc, RANGE_STYLE, INFO_STYLE, LOC_STYLE, 0, 1, 1);
        C(TERM_RESET);
    }
    printf("type  = %s\n", get_string(get_type_string_id(node->type)));
    printf("value = %s\n", get_string(value_to_string_id(node->value, node->type)));
}

void print_type(u32 ty) {
    printf("%s\n", get_string(get_type_string_id(ty)));
}
