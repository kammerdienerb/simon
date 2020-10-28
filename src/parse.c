#include "parse.h"
#include "globals.h"
#include "threadpool.h"
#include "file.h"
#include "strings.h"
#include "ui.h"
#include "src_range.h"
#include "ast.h"
#include "tls.h"

typedef struct {
    tls_t       *tls;
    file_t       file;
    src_range_t  cur_range;
    src_range_t  just_cleaned_range;
    u64          n_lines;
    u64          n_blank_lines;
} parse_context_t;

#define IS_SPACE(c)      (((c) >= 9 && (c) <= 13) || (c) == 32)
#define IS_ALPHA(c)      (((c) >= 'a' && (c) <= 'z') || ((c) >= 'A' && (c) <= 'Z'))
#define IS_NUM(c)        ((c) >= '0' && (c) <= '9')
#define IS_IDENT_CHAR(c) ((c) == '_' || IS_ALPHA(c) || IS_NUM(c))

#define BEGIN_RANGE(_cxt, r) \
    ((r).beg = (_cxt)->cur_range.end)

#define END_CLEAN_RANGE(_cxt) \
    (end_range_before(&((_cxt)->just_cleaned_range), &((_cxt)->cur_range), NULL))

static void consume_comment(parse_context_t *cxt) {
    /* Consume ';' */
    cxt->file.cursor       += 1;
    cxt->cur_range.end.col += 1;

    /* Consume rest */
    while (cxt->file.cursor < cxt->file.end
    &&     *cxt->file.cursor != '\n') {
        cxt->file.cursor       += 1;
        cxt->cur_range.end.col += 1;
    }
    cxt->cur_range.end.buff_ptr = cxt->file.cursor;
}

static int clean(parse_context_t *cxt, int passalong_len) {
    src_point_t save_cur_end;
    int         altered;
    int         seen_first_nl;
    char        c;

    if (cxt->file.cursor >= cxt->file.end) {
        return passalong_len;
    }

    save_cur_end  = cxt->cur_range.end;
    altered       = 0;
    seen_first_nl = 0;

    while (cxt->file.cursor < cxt->file.end) {
        c = *cxt->file.cursor;

        if (*((unsigned int*)cxt->file.cursor) == 0x20202020) {
            cxt->cur_range.end.col += 4;
            cxt->file.cursor       += 4;
        } else if (IS_SPACE(c)) {
            if (c == '\n') {
                cxt->n_lines            += 1;
                cxt->cur_range.end.line += 1;
                cxt->cur_range.end.col   = 1;

                if (seen_first_nl || cxt->file.cursor == cxt->file.buff) {
                    cxt->n_blank_lines += 1;
                } else {
                    seen_first_nl = 1;
                }
            } else {
                cxt->cur_range.end.col += 1;
            }

            cxt->file.cursor += 1;
        } else if (c == ';') {
            consume_comment(cxt);
            /* If the last line is a comment, we have another blank. */
            if (cxt->file.cursor >= cxt->file.end) {
                cxt->n_blank_lines += 1;
            }
        } else {
            break;
        }

        altered = 1;
    }

    if (altered) {
        cxt->just_cleaned_range.beg = save_cur_end;
        END_CLEAN_RANGE(cxt);
    }

    cxt->cur_range.end.buff_ptr = cxt->file.cursor;

    return passalong_len;
}

static int eat(parse_context_t *cxt, int len) {
    if (len) {
        cxt->file.cursor += len;
        cxt->cur_range.end.buff_ptr = cxt->file.cursor;
    }
    return len;
}

#define EXPECT(cxt, call, msg) \
do {                           \
    if (!eat((cxt), (call))) { \
        report_vague_err(msg); \
    }                          \
    clean((cxt), 0);           \
} while (0)

#define _OPTIONAL_COMMON(call)              (call)
#define OPTIONAL(cxt, call)                 (clean((cxt), eat((cxt), _OPTIONAL_COMMON(call))))
#define OPTIONAL_NO_CLEAN(cxt, call)        (eat((cxt), _OPTIONAL_COMMON(call)))
#define OPTIONAL_NO_EAT(cxt, call)          (clean((cxt), _OPTIONAL_COMMON(call)))
#define OPTIONAL_NO_CLEAN_OR_EAT(cxt, call) (_OPTIONAL_COMMON(call))

static int parse_identifier(parse_context_t *cxt, string_id *string_out) {
    int  has_alnum;
    int  len;
    char c;

    has_alnum = 0;
    len       = 0;

    if (cxt->file.cursor < cxt->file.end
    &&  ((c = *(cxt->file.cursor)), (c == '_' || IS_ALPHA(c)))) {
        if (c != '_') {
            has_alnum = 1;
        }
        len += 1;
    } else {
        return 0;
    }

    while (cxt->file.cursor < cxt->file.end
    &&     IS_IDENT_CHAR((c = *(cxt->file.cursor + len)))) {
        if (c != '_') {
            has_alnum = 1;
        }
        len += 1;
    }

    if (!has_alnum) { return 0; }

    if (string_out) {
        *string_out = get_string_id_n(cxt->file.cursor, len);
    }

    return len;
}

static int parse_char(parse_context_t *cxt, char c) {
    if (cxt->file.cursor < cxt->file.end
    &&  *cxt->file.cursor == c) {
        cxt->file.cursor += 1;
        return 1;
    }
    return 0;
}

#define EXPECT_IDENT(cxt, id_ptr, msg) \
    EXPECT((cxt), parse_identifier((cxt), (id_ptr)), (msg))
#define OPTIONAL_IDENT(cxt, id_ptr) \
    OPTIONAL((cxt), parse_identifier((cxt), (id_ptr)))

#define EXPECT_CHAR(cxt, c, msg) \
    EXPECT((cxt), parse_char((cxt), (c)), (msg))
#define OPTIONAL_CHAR(cxt, c) \
    OPTIONAL((cxt), parse_char((cxt), (c)))

static void remove_trailing_whitespace(parse_context_t *cxt) {
    while (cxt->file.end != cxt->file.buff
    &&     IS_SPACE(*(cxt->file.end - 1))) {
        if (*(cxt->file.end - 1) == '\n') {
            cxt->n_lines += 1;
            if (cxt->file.end - 2 >= cxt->file.buff
            &&  *(cxt->file.end - 2) == '\n') {
                cxt->n_blank_lines += 1;
            }
        }
        cxt->file.end -= 1;
    }
}

#define AST_ALLOC(cxt, t) \
    (bump_alloc(&(cxt)->tls->bump_alloc, sizeof(t)))

static ast_t * parse_assign(parse_context_t *cxt) {
    src_range_t   loc;
    string_id     name;
    ast_assign_t *result;

    BEGIN_RANGE(cxt, loc);

    if (!OPTIONAL_IDENT(cxt, &name)) { return NULL; }
    if (!OPTIONAL_CHAR(cxt, '='))    { return NULL; }

    result          = AST_ALLOC(cxt, ast_assign_t);
    result->ast.loc = loc;
    result->name    = name;

    return &(result->ast);
}

static void setup_cxt(parse_context_t *cxt) {
    cxt->tls = get_tls();

    cxt->cur_range.path_id      = cxt->file.path_id;
    cxt->cur_range.beg.line     = 1;
    cxt->cur_range.beg.col      = 1;
    cxt->cur_range.beg.buff_ptr = cxt->file.cursor;
    cxt->cur_range.end.line     = 1;
    cxt->cur_range.end.col      = 1;
    cxt->cur_range.end.buff_ptr = cxt->file.cursor;

    cxt->just_cleaned_range = cxt->cur_range;

    cxt->n_lines = cxt->n_blank_lines = 0;
}

static void parse(parse_context_t *cxt) {
    setup_cxt(cxt);

    remove_trailing_whitespace(cxt);
    clean(cxt, 0);

    while (cxt->file.cursor < cxt->file.end) {
        if (!parse_assign(cxt)) {
            report_vague_err("expected assignment");
        }
        clean(cxt, 0);
    }
}

static void parse_file_thread(void *_path) {
    parse_context_t  cxt;
    const char      *path;
    int              err;

    path = (const char*)_path;
    err  = map_file_into_readonly_memory(path, &(cxt.file));

    if (err) { report_file_err(&(cxt.file), err); }

    parse(&cxt);
}

void start_parsing_file(const char *path) {
    tp_add_task(tp, parse_file_thread, (void*)path);
}

void wait_for_parsing(void) { tp_wait(tp); }
