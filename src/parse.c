#include "parse.h"
#include "globals.h"
#include "threadpool.h"
#include "file.h"
#include "strings.h"
#include "ui.h"
#include "src_range.h"
#include "ast.h"
#include "tls.h"
#include "array.h"
#include "scope.h"
#include "type.h"

typedef struct {
    tls_t        *tls;
    ifile_t      *file;
    char         *end;
    char         *cursor;
    src_point_t   cur_point;
    src_point_t   pre_clean_point;
    u64           n_lines;
    u64           n_blank_lines;
    array_t       top_level_nodes;
    scope_t      *global_scope;
    array_t       scope_stack;
    int           allow_poly_idents;
    int           poly_expr_pattern;
    int           in_struct;
    int           in_macro_def;
    ast_macro_t  *current_macro_def;
    array_t       macro_calls;
} parse_context_t;


u32 op_prec_table[] = {
#define X(_op, _prec, _assoc, _arity, _match, _str) _prec,
    X_OPS
#undef X
};
int op_assoc_table[] = {
#define X(_op, _prec, _assoc, _arity, _match, _str) _assoc,
    X_OPS
#undef X
};
int op_arity_table[] = {
#define X(_op, _prec, _assoc, _arity, _match, _str) _arity,
    X_OPS
#undef X
};
const char * op_match_table[] = {
#define X(_op, _prec, _assoc, _arity, _match, _str) _match,
    X_OPS
#undef X
};
const char * op_str_table[] = {
#define X(_op, _prec, _assoc, _arity, _match, _str) _str,
    X_OPS
#undef X
};





#define IS_SPACE(c)               (((c) >= 9 && (c) <= 13) || (c) == 32)
#define IS_ALPHA(c)               (((c) >= 'a' && (c) <= 'z') || ((c) >= 'A' && (c) <= 'Z'))
#define IS_NUM(c)                 ((c) >= '0' && (c) <= '9')
#define IS_IDENT_CHAR(c)          ((c) == '_' || IS_ALPHA(c) || IS_NUM(c))
#define IS_IN_RANGE(c, low, high) ((c) >= (low) && (c) <= (high))

#define GET_BEG_POINT(_cxt) (((_cxt)->cur_point.buff_ptr = (_cxt)->cursor), ((_cxt)->cur_point))
#define GET_END_POINT(_cxt) ((_cxt)->pre_clean_point)

static void consume_to_eol(parse_context_t *cxt) {
    while (cxt->cursor < cxt->end
    &&     *cxt->cursor != '\n') {
        cxt->cursor        += 1;
        cxt->cur_point.col += 1;
    }
}

static void consume_comment(parse_context_t *cxt) {
    /* Consume '#' */
    cxt->cursor        += 1;
    cxt->cur_point.col += 1;

    consume_to_eol(cxt);
}

static int clean(parse_context_t *cxt, int passalong_len) {
    int  seen_first_nl;
    char c;

    if (unlikely(cxt->cursor >= cxt->end)) { return passalong_len; }

    cxt->pre_clean_point = GET_BEG_POINT(cxt);

    seen_first_nl = 0;

    while (cxt->cursor < cxt->end) {
        c = *cxt->cursor;

        if (*((unsigned int*)cxt->cursor) == 0x20202020) {
            cxt->cur_point.col += 4;
            cxt->cursor        += 4;
        } else if (IS_SPACE(c)) {
            if (c == '\n') {
                cxt->n_lines        += 1;
                cxt->cur_point.line += 1;
                cxt->cur_point.col   = 1;

                if (seen_first_nl || cxt->cursor == cxt->file->buff) {
                    cxt->n_blank_lines += 1;
                } else {
                    seen_first_nl = 1;
                }
            } else {
                cxt->cur_point.col += 1;
            }

            cxt->cursor += 1;
        } else if (c == '#') {
            consume_comment(cxt);
            /* If the last line is a comment, we have another blank. */
            if (cxt->cursor >= cxt->end) {
                cxt->n_blank_lines += 1;
            }
        } else {
            break;
        }
    }

    return passalong_len;
}

static int eat(parse_context_t *cxt, int len) {
    if (len) {
        cxt->cursor        += len;
        cxt->cur_point.col += len;
    }
    return len;
}

#define EXPECT(cxt, call, fmt, ...)                               \
do {                                                              \
    if (!eat((cxt), (call))) {                                    \
        report_loc_err(GET_BEG_POINT(cxt), (fmt), ##__VA_ARGS__); \
    }                                                             \
    clean((cxt), 0);                                              \
} while (0)

#define _OPTIONAL_COMMON(call)              (call)
#define OPTIONAL(cxt, call)                 (clean((cxt), eat((cxt), _OPTIONAL_COMMON(call))))
#define OPTIONAL_NO_CLEAN(cxt, call)        (eat((cxt), _OPTIONAL_COMMON(call)))
#define OPTIONAL_NO_EAT(cxt, call)          (clean((cxt), _OPTIONAL_COMMON(call)))
#define OPTIONAL_NO_CLEAN_OR_EAT(cxt, call) (_OPTIONAL_COMMON(call))

static int parse_identifier(parse_context_t *cxt, string_id *string_out) {
    int       has_alnum;
    int       len;
    char      c;
    string_id id;

    has_alnum = 0;
    len       = 0;

    if (cxt->cursor < cxt->end
    &&  ((c = *(cxt->cursor)), (c == '_' || IS_ALPHA(c)))) {
        if (c != '_') {
            has_alnum = 1;
        }
        len += 1;
    } else {
        return 0;
    }

    while ((cxt->cursor + len) < cxt->end
    &&     IS_IDENT_CHAR((c = *(cxt->cursor + len)))) {
        if (c != '_') {
            has_alnum = 1;
        }
        len += 1;
    }

    if (!has_alnum) {
        if (len != 1 || *cxt->cursor != '_') {
            return 0;
        }
    }

    id = get_string_id_n(cxt->cursor, len);

    if (is_kwd(id)) { return 0; }

    if (string_out) {
        *string_out = id;
    }

    return len;
}

static int parse_declaration_begin(parse_context_t *cxt) {
    int       has_alnum;
    int       len;
    char      c;
    int       id_len;
    string_id id;

    has_alnum = 0;
    len       = 0;

    if (cxt->cursor < cxt->end
    &&  ((c = *(cxt->cursor)), (c == '_' || IS_ALPHA(c)))) {
        if (c != '_') {
            has_alnum = 1;
        }
        len += 1;
    } else {
        return 0;
    }

    while ((cxt->cursor + len) < cxt->end
    &&     IS_IDENT_CHAR((c = *(cxt->cursor + len)))) {
        if (c != '_') {
            has_alnum = 1;
        }
        len += 1;
    }

    if (!has_alnum) {
        if (len != 1 || *cxt->cursor != '_') {
            return 0;
        }
    }

    id_len = len;
    while ((cxt->cursor + len) < cxt->end && IS_SPACE(*(cxt->cursor + len))) {
        len += 1;
    }

    if ((cxt->cursor + len) >= cxt->end || *(cxt->cursor + len) != ':') {
        return 0;
    }

    len += 1;

    id = get_string_id_n(cxt->cursor, id_len);

    if (is_kwd(id)) { return 0; }

    return len;
}

static int parse_macro_call_begin(parse_context_t *cxt) {
    int       has_alnum;
    int       len;
    char      c;
    int       id_len;
    string_id id;

    has_alnum = 0;
    len       = 0;

    if (cxt->cursor < cxt->end
    &&  ((c = *(cxt->cursor)), (c == '_' || IS_ALPHA(c)))) {
        if (c != '_') {
            has_alnum = 1;
        }
        len += 1;
    } else {
        return 0;
    }

    while ((cxt->cursor + len) < cxt->end
    &&     IS_IDENT_CHAR((c = *(cxt->cursor + len)))) {
        if (c != '_') {
            has_alnum = 1;
        }
        len += 1;
    }

    if (!has_alnum) {
        if (len != 1 || *cxt->cursor != '_') {
            return 0;
        }
    }

    id_len = len;

    if ((cxt->cursor + len) >= cxt->end || *(cxt->cursor + len) != '!') {
        return 0;
    }

    len += 1;

    id = get_string_id_n(cxt->cursor, id_len);

    if (is_kwd(id)) { return 0; }

    return len;
}

static int parse_char(parse_context_t *cxt, char c) {
    return (cxt->cursor < cxt->end && *cxt->cursor == c);
}

static int parse_literal(parse_context_t *cxt, const char *lit) {
    int len;

    len = 0;
    while (cxt->cursor + len < cxt->end && *lit) {
        if (*(cxt->cursor + len) == *lit) {
            lit += 1;
            len += 1;
        } else {
            return 0;
        }
    }

    if (*lit) { return 0; }

    return len;
}

static int parse_word(parse_context_t *cxt, const char *word) {
    int len;

    len = 0;
    while (cxt->cursor + len < cxt->end && *word) {
        if (*(cxt->cursor + len) == *word) {
            word += 1;
            len  += 1;
        } else {
            return 0;
        }
    }

    if (*word) { return 0; }

    if (cxt->cursor + len < cxt->end) {
        if (IS_IDENT_CHAR(*(cxt->cursor + len))) {
            return 0;
        }
    }

    return len;
}

static int parse_int(parse_context_t *cxt, string_id *string_out) {
    char *curs;
    int   len;
    int   non_underscore;

    curs           = cxt->cursor;
    len            = 0;
    non_underscore = 0;

    if (curs >= cxt->end) { return 0; }

    if (*curs == '0') {
        curs += 1;
        if (curs < cxt->end) {
            if (*curs == 'x') {
                curs += 1;

                if (curs < cxt->end
                &&  (IS_NUM(*curs)
                ||  IS_IN_RANGE(*curs, 'a', 'f')
                ||  IS_IN_RANGE(*curs, 'A', 'F')
                ||  *curs == '_')) {

                    non_underscore |= (*curs != '_');

                    curs += 1;

                    while (curs < cxt->end) {
                        if (!IS_NUM(*curs)
                        &&  !IS_IN_RANGE(*curs, 'a', 'f')
                        &&  !IS_IN_RANGE(*curs, 'A', 'F')
                        &&  *curs != '_') {

                            if (IS_ALPHA(*curs)) {
                                return 0;
                            }
                            break;
                        }

                        non_underscore |= (*curs != '_');

                        curs += 1;
                    }
                } else {
                    return 0;
                }
            } else if (IS_IDENT_CHAR(*curs)) {
                return 0;
            } else {
                non_underscore = 1;
            }
        }
    } else if (*curs == '-') {
        curs += 1;

        if (curs < cxt->end && IS_NUM(*curs)) {
            non_underscore = 1;
            curs += 1;
        } else {
            return 0;
        }

        goto get_digits;
    } else {
get_digits:;
        while (curs < cxt->end) {
            if (!IS_NUM(*curs) && *curs != '_') {
                if (IS_ALPHA(*curs)) {
                    return 0;
                }
                break;
            }

            non_underscore |= (*curs != '_');
            curs += 1;
        }
    }

    if (non_underscore == 0) { return 0; }

    len = curs - cxt->cursor;

    if (len > 0 && string_out) {
        *string_out = get_string_id_n(cxt->cursor, len);
    }

    return len;
}

static int parse_float(parse_context_t *cxt, string_id *string_out) {
    char *curs;
    int   len;
    int   non_underscore;

    curs           = cxt->cursor;
    len            = 0;
    non_underscore = 0;

    if (*curs == '-') {
        curs += 1;

        if (curs < cxt->end && IS_NUM(*curs)) {
            non_underscore = 1;
            curs += 1;
        } else {
            return 0;
        }

        goto get_digits;
    } else {
get_digits:;
        while (curs < cxt->end) {
            if (!IS_NUM(*curs) && *curs != '_') {
                if (*curs != '.' || !non_underscore) {
                    return 0;
                }
                break;
            }

            non_underscore |= (*curs != '_');
            curs += 1;
        }

        curs += 1;

        non_underscore = 0;

        while (curs < cxt->end) {
            if (!IS_NUM(*curs) && *curs != '_') {
                if (IS_ALPHA(*curs)) {
                    return 0;
                }
                break;
            }

            non_underscore |= (*curs != '_');
            curs += 1;
        }
    }

    if (non_underscore == 0) { return 0; }

    len = curs - cxt->cursor;

    if (len > 0 && string_out) {
        *string_out = get_string_id_n(cxt->cursor, len);
    }

    return len;
}

static int parse_string_literal(parse_context_t *cxt, string_id *string_out) {
    char *curs;
    char  last;
    int   len;

    len  = 0;
    curs = cxt->cursor;

    if (curs >= cxt->end) { return 0; }

    if (*curs != '\"') { return 0; }

    curs += 1;

    last = 0;
    while (curs < cxt->end) {
        if (*curs == '\"') {
            if (last != '\\') {
                curs += 1;
                break;
            }
        }

        if (last == '\\' && *curs == '\\') {
            last = 0;
        } else {
            last = *curs;
        }
        curs += 1;
    }

    len = curs - cxt->cursor;

    if (len > 0 && string_out) {
        *string_out = get_string_id_n(cxt->cursor, len);
    }

    return len;
}

static int parse_char_literal(parse_context_t *cxt, string_id *char_out) {
    char *curs;
    int   len;

    len  = 0;
    curs = cxt->cursor;

    if (curs >= cxt->end) { return 0; }

    if (*curs != '\'') { return 0; }

    curs += 1;

    if (*curs == '\\') {
        curs += 1;
    }

    curs += 1;

    if (*curs != '\'') { return 0; }

    curs += 1;

    len = curs - cxt->cursor;

    if (len > 0 && char_out) {
        *char_out = get_string_id_n(cxt->cursor, len);
    }

    return len;
}


#define EXPECT_IDENT(cxt, id_ptr, fmt, ...) \
    EXPECT((cxt), parse_identifier((cxt), (id_ptr)), (fmt), ##__VA_ARGS__)
#define OPTIONAL_IDENT(cxt, id_ptr) \
    OPTIONAL((cxt), parse_identifier((cxt), (id_ptr)))

#define OPTIONAL_NO_EAT_DECLARATION(cxt) \
    OPTIONAL_NO_EAT((cxt), parse_declaration_begin((cxt)))

#define OPTIONAL_NO_EAT_MACRO_CALL_BEGIN(cxt) \
    OPTIONAL_NO_EAT((cxt), parse_macro_call_begin((cxt)))

#define EXPECT_CHAR(cxt, c, fmt, ...) \
    EXPECT((cxt), parse_char((cxt), (c)), (fmt), ##__VA_ARGS__)
#define OPTIONAL_CHAR(cxt, c) \
    OPTIONAL((cxt), parse_char((cxt), (c)))
#define OPTIONAL_NO_EAT_CHAR(cxt, c) \
    OPTIONAL_NO_EAT((cxt), parse_char((cxt), (c)))

#define EXPECT_LIT(cxt, lit, fmt, ...) \
    EXPECT((cxt), parse_literal((cxt), (lit)), (fmt), ##__VA_ARGS__)
#define OPTIONAL_LIT(cxt, lit) \
    OPTIONAL((cxt), parse_literal((cxt), (lit)))
#define OPTIONAL_NO_EAT_LIT(cxt, lit) \
    OPTIONAL_NO_EAT((cxt), parse_literal((cxt), (lit)))

#define EXPECT_WORD(cxt, word, fmt, ...) \
    EXPECT((cxt), parse_word((cxt), (word)), (fmt), ##__VA_ARGS__)
#define OPTIONAL_WORD(cxt, word) \
    OPTIONAL((cxt), parse_word((cxt), (word)))
#define OPTIONAL_NO_EAT_WORD(cxt, word) \
    OPTIONAL_NO_EAT((cxt), parse_word((cxt), (word)))

#define EXPECT_INT(cxt, id_ptr, fmt, ...) \
    EXPECT((cxt), parse_int((cxt), (id_ptr)), (fmt), ##__VA_ARGS__)
#define OPTIONAL_INT(cxt, id_ptr) \
    OPTIONAL((cxt), parse_int((cxt), (id_ptr)))
#define OPTIONAL_NO_EAT_INT(cxt, id_ptr) \
    OPTIONAL_NO_EAT((cxt), parse_int((cxt), (id_ptr)))

#define EXPECT_FLOAT(cxt, id_ptr, fmt, ...) \
    EXPECT((cxt), parse_float((cxt), (id_ptr)), (fmt), ##__VA_ARGS__)
#define OPTIONAL_FLOAT(cxt, id_ptr) \
    OPTIONAL((cxt), parse_float((cxt), (id_ptr)))

#define EXPECT_STR_LIT(cxt, id_ptr, fmt, ...) \
    EXPECT((cxt), parse_string_literal((cxt), (id_ptr)), (fmt), ##__VA_ARGS__)
#define OPTIONAL_STR_LIT(cxt, id_ptr) \
    OPTIONAL((cxt), parse_string_literal((cxt), (id_ptr)))

#define EXPECT_CHAR_LIT(cxt, id_ptr, fmt, ...) \
    EXPECT((cxt), parse_char_literal((cxt), (id_ptr)), (fmt), ##__VA_ARGS__)
#define OPTIONAL_CHAR_LIT(cxt, id_ptr) \
    OPTIONAL((cxt), parse_char_literal((cxt), (id_ptr)))


#define AST_ALLOC(cxt, t) \
    (bump_alloc(&(cxt)->tls->bump_alloc, sizeof(t)))



#define SCOPE(_cxt) (*(scope_t**)array_last((_cxt)->scope_stack))

#define SCOPE_POP(_cxt)                                                           \
do {                                                                              \
    array_pop((_cxt)->scope_stack);                                               \
    ASSERT(array_len((_cxt)->scope_stack), "scope mismatch");                     \
} while (0)

#define SCOPE_PUSH(_cxt, _kind, _node)                                            \
do {                                                                              \
    scope_t *_new_scope;                                                          \
    _new_scope = add_subscope(SCOPE((_cxt)), (_kind), (_node));                   \
    array_push((_cxt)->scope_stack, _new_scope);                                  \
} while (0)

#define SCOPE_PUSH_NAMED(_cxt, _kind, _node, _name_id)                            \
do {                                                                              \
    scope_t *_new_scope;                                                          \
    _new_scope = add_named_subscope(SCOPE((_cxt)), (_kind), (_node), (_name_id)); \
    array_push((_cxt)->scope_stack, _new_scope);                                  \
} while (0)

#define INSTALL(_cxt, _name_id, _node)                                            \
do {                                                                              \
    add_symbol(SCOPE((_cxt)), (_name_id), (_node));                               \
} while (0)





static ast_t * parse_expr(parse_context_t *cxt);
static ast_t * parse_block(parse_context_t *cxt);

static ast_t * parse_arg_list(parse_context_t *cxt, char close_c) {
    ast_arg_list_t *result;
    arg_t           arg;

    result                = AST_ALLOC(cxt, ast_arg_list_t);
    ASTP(result)->kind    = AST_ARG_LIST;
    ASTP(result)->loc.beg = GET_BEG_POINT(cxt);
    result->args          = array_make(arg_t);

    while (!OPTIONAL_NO_EAT_CHAR(cxt, close_c)) {
        arg.name = STRING_ID_NULL;
        if (OPTIONAL_NO_EAT_DECLARATION(cxt)) {
            ASSERT(OPTIONAL_IDENT(cxt, &arg.name), "parse_declaration_begin must be wrong");
            ASSERT(OPTIONAL_CHAR(cxt, ':'),        "parse_declaration_begin must be wrong");
        }

        arg.expr = parse_expr(cxt);
        if (arg.expr == NULL) {
            if (arg.name == STRING_ID_NULL) {
                report_loc_err(GET_BEG_POINT(cxt), "expected valid expression in argument list");
            } else {
                report_loc_err(GET_BEG_POINT(cxt), "expected valid expression for named argument '%s'", get_string(arg.name));
            }
            return NULL;
        }

        array_push(result->args, arg);

        if (!OPTIONAL_CHAR(cxt, ',')) { break; }
    }

    if (array_len(result->args) > MAX_PARAMS_OR_ARGS) {
        report_range_err(&((arg_t*)array_item(result->args, MAX_PARAMS_OR_ARGS))->expr->loc,
                         "procedure call has exceeded the maximum number of arguments (%d)",
                         MAX_PARAMS_OR_ARGS);
        return NULL;
    }

    ASTP(result)->loc.end = GET_END_POINT(cxt);

    return ASTP(result);
}

static ast_t *parse_macro_call(parse_context_t *cxt, int expected_kind) {
    ast_macro_call_t *result;
    ast_t            *ident;
    char              style;
    char              match;

    if (!OPTIONAL_NO_EAT_MACRO_CALL_BEGIN(cxt)) { return NULL; }

    result                = AST_ALLOC(cxt, ast_macro_call_t);
    ASTP(result)->loc.beg = GET_BEG_POINT(cxt);
    ASTP(result)->kind    = AST_MACRO_CALL;
    result->block         = NULL;
    result->scope         = SCOPE(cxt);
    result->expected_kind = expected_kind;

    ident                                = AST_ALLOC(cxt, ast_ident_t);
    ident->kind                          = AST_IDENT;
    ((ast_ident_t*)ident)->resolved_node = NULL;
    ((ast_ident_t*)ident)->mono_idx      = -1;
    ((ast_ident_t*)ident)->varg_idx      = -1;

    ident->loc.beg = GET_BEG_POINT(cxt);
    ASSERT(OPTIONAL_IDENT(cxt, &((ast_ident_t*)ident)->str_rep), "parse_macro_call_begin must be wrong");
    ident->loc.end = GET_END_POINT(cxt);

    result->ident = ident;

    ASSERT(OPTIONAL_CHAR(cxt, '!'), "parse_macro_call_begin must be wrong");

    if (OPTIONAL_CHAR(cxt, '(')) {
        style = '('; match = ')';
    } else if (OPTIONAL_CHAR(cxt, '[')) {
        style = '['; match = ']';
    } else {
        report_loc_err(GET_BEG_POINT(cxt), "expected either '(' or '[' to open a macro argument list");
        return NULL;
    }

    result->arg_list = parse_arg_list(cxt, match);

    EXPECT_CHAR(cxt, match, "expected '%c'", match);

    ASTP(result)->loc.end = GET_END_POINT(cxt);

    if (style == '[') {
        if ((result->block = parse_block(cxt)) == NULL) {
            report_loc_err(GET_BEG_POINT(cxt), "expected '{' to open '[]'-style macro block");
            return NULL;
        }
        result->block->flags |= AST_FLAG_SYNTHETIC_BLOCK;
    }

    if (!cxt->in_macro_def) {
        array_push(cxt->macro_calls, result);
    }

    return ASTP(result);
}

static int lookahead_binary_op(parse_context_t *cxt) {
    int op;

    /*
    ** Simple check so that the beginning of a assignment tag doesn't
    ** parse as a subscript operator.
    */
    if (OPTIONAL_NO_EAT_LIT(cxt, "[[")) { return OP_INVALID; }


    op = OP_INVALID;

    /*
    ** The order of these is sensative to parsing.
    ** e.g.
    **    << will parse as < if < is parsed first.
    ** So you have to check for << explicitly before <.
    ** (Obviously applies to other operators too.)
    */

    if      (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_CALL)))         { op = OP_CALL;         }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_DOT)))          { op = OP_DOT;          }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_SUBSCRIPT)))    { op = OP_SUBSCRIPT;    }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_EQU)))          { op = OP_EQU;          }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_NEQ)))          { op = OP_NEQ;          }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_ASSIGN)))       { op = OP_ASSIGN;       }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_PLUS_ASSIGN)))  { op = OP_PLUS_ASSIGN;  }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_MINUS_ASSIGN))) { op = OP_MINUS_ASSIGN; }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_MULT_ASSIGN)))  { op = OP_MULT_ASSIGN;  }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_DIV_ASSIGN)))   { op = OP_DIV_ASSIGN;   }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_MOD_ASSIGN)))   { op = OP_MOD_ASSIGN;   }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_PLUS)))         { op = OP_PLUS;         }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_MINUS)))        { op = OP_MINUS;        }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_MULT)))         { op = OP_MULT;         }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_DIV)))          { op = OP_DIV;          }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_MOD)))          { op = OP_MOD;          }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_BSHL)))         { op = OP_BSHL;         }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_BSHR)))         { op = OP_BSHR;         }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_LEQ)))          { op = OP_LEQ;          }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_GEQ)))          { op = OP_GEQ;          }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_LSS)))          { op = OP_LSS;          }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_GTR)))          { op = OP_GTR;          }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_AND)))          { op = OP_AND;          }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_OR)))           { op = OP_OR;           }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_BXOR)))         { op = OP_BXOR;         }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_BAND)))         { op = OP_BAND;         }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_BOR)))          { op = OP_BOR;          }

    return op;
}

static int lookahead_unary_prefix_op(parse_context_t *cxt) {
    int op;

    op = OP_INVALID;

         if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_NOT)))      { op = OP_NOT;      }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_SLICE)))    { op = OP_SLICE;    }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_ADDR)))     { op = OP_ADDR;     }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_DEREF)))    { op = OP_DEREF;    }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_NEG)))      { op = OP_NEG;      }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_BNEG)))     { op = OP_BNEG;     }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_SIZEOF)))   { op = OP_SIZEOF;   }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_TYPEOF)))   { op = OP_TYPEOF;   }
    else if (OPTIONAL_NO_EAT_LIT(cxt, OP_MATCH(OP_LENOF)))    { op = OP_LENOF;    }

    return op;
}

static ast_t * parse_expr_prec(parse_context_t *cxt, int min_prec);
static ast_t * parse_block_with_scope(parse_context_t *cxt);
static ast_t * parse_block(parse_context_t *cxt);

static ast_t * parse_leaf_expr(parse_context_t *cxt) {
    src_range_t  loc;
    ast_t       *result;
    ast_t       *type_expr;
    string_id    str_rep;

    loc.beg = GET_BEG_POINT(cxt);

    result = NULL;

    if (OPTIONAL_NO_EAT_WORD(cxt, "proc")) {
        result                                       = AST_ALLOC(cxt, ast_proc_type_t);
        result->kind                                 = AST_PROC_TYPE;
        ((ast_proc_type_t*)result)->param_type_exprs = array_make(ast_t*);
        ((ast_proc_type_t*)result)->ret_type_expr    = NULL;

        ASSERT(OPTIONAL_WORD(cxt, "proc"), "eat");

        EXPECT_CHAR(cxt, '(', "expected '(' to open the parameter type list for a procedure type");

        while (!OPTIONAL_CHAR(cxt, ')')) {
            type_expr = parse_expr_prec(cxt, ASSIGNMENT_PREC + 1);

            if (type_expr == NULL) {
                report_loc_err(GET_BEG_POINT(cxt), "expected valid type expression");
                return NULL;
            }

            array_push(((ast_proc_type_t*)result)->param_type_exprs, type_expr);

            if (!OPTIONAL_CHAR(cxt, ',')) {
                EXPECT_CHAR(cxt, ')', "expected ')' to close the parameter type list for procedure type or ',' to add more");
                break;
            }
        }

        if (OPTIONAL_CHAR(cxt, ':')) {
            type_expr = parse_expr_prec(cxt, ASSIGNMENT_PREC + 1);

            if (type_expr == NULL) {
                report_loc_err(GET_BEG_POINT(cxt), "expected valid type expression");
                return NULL;
            }

            ((ast_proc_type_t*)result)->ret_type_expr = type_expr;
        }


    /* @performance
     * Is this the best thing to do? We're double checking the identifier pattern, which
     * is the most likely path... */
    } else if ((result = parse_macro_call(cxt, MACRO_EXPR)) != NULL) {
        /* It's all set up. */
    } else if (OPTIONAL_IDENT(cxt, &str_rep)) {
        result                                = AST_ALLOC(cxt, ast_ident_t);
        result->kind                          = AST_IDENT;
        ((ast_ident_t*)result)->str_rep       = str_rep;
        ((ast_ident_t*)result)->resolved_node = NULL;
        ((ast_ident_t*)result)->mono_idx      = -1;
        ((ast_ident_t*)result)->varg_idx      = -1;

        if (cxt->in_macro_def) {
            result->flags |= AST_FLAG_NAME_IN_MACRO;
        }
    } else if (OPTIONAL_FLOAT(cxt, &str_rep)) {
        result                           = AST_ALLOC(cxt, ast_float_t);
        result->kind                     = AST_FLOAT;
        ((ast_float_t*)result)->str_rep  = str_rep;
        result->flags                   |= AST_FLAG_CONSTANT;
    } else if (OPTIONAL_INT(cxt, &str_rep)) {
        result                           = AST_ALLOC(cxt, ast_int_t);
        result->kind                     = AST_INT;
        ((ast_int_t*)result)->str_rep    = str_rep;
        result->flags                   |= AST_FLAG_CONSTANT;
    } else if (OPTIONAL_STR_LIT(cxt, &str_rep)) {
        result                           = AST_ALLOC(cxt, ast_string_t);
        result->kind                     = AST_STRING;
        ((ast_string_t*)result)->str_rep = str_rep;
        result->flags                   |= AST_FLAG_CONSTANT;
    } else if (OPTIONAL_CHAR_LIT(cxt, &str_rep)) {
        result                           = AST_ALLOC(cxt, ast_char_t);
        result->kind                     = AST_CHAR;
        ((ast_char_t*)result)->str_rep   = str_rep;
        result->flags                   |= AST_FLAG_CONSTANT;
    } else if (OPTIONAL_NO_EAT_CHAR(cxt, '(')) {
        ASSERT(OPTIONAL_CHAR(cxt, '('), "eat");
        result = parse_expr(cxt);
        if (result == NULL) {
            report_loc_err(GET_BEG_POINT(cxt), "expected valid expression after opening '('");
            return NULL;
        }
/*         result->flags |= AST_FLAG_PAREN_EXPR; */
        EXPECT_CHAR(cxt, ')', "expected closing ')'");
    } else if (OPTIONAL_CHAR(cxt, '%')) {
        EXPECT_IDENT(cxt, &str_rep, "expected valid identifier after '%%', which indicates the declaration of a polymorphic parameter");
        if (!cxt->allow_poly_idents) {
            loc.end = GET_END_POINT(cxt);
            report_range_err(&loc, "polymorphic parameter declaration is not allowed outside of a struct or procedure parameter list");
            return NULL;
        }

        result                                = AST_ALLOC(cxt, ast_ident_t);
        result->kind                          = AST_IDENT;
        ((ast_ident_t*)result)->str_rep       = str_rep;
        ((ast_ident_t*)result)->resolved_node = NULL;

        result->flags           |= AST_FLAG_POLYMORPH | AST_FLAG_POLY_IDENT;
        SCOPE(cxt)->node->flags |= AST_FLAG_POLYMORPH;
        cxt->poly_expr_pattern   = 1;

        loc.end     = GET_END_POINT(cxt);
        result->loc = loc;

        INSTALL(cxt, str_rep, result);
    } else if (OPTIONAL_NO_EAT_CHAR(cxt, '$')) {
        eat(cxt, 1);
        EXPECT_IDENT(cxt, &str_rep, "expected identifier in macro argument expansion");

        result                                  = AST_ALLOC(cxt, ast_macro_arg_expand_t);
        result->kind                            = AST_MACRO_ARG_EXPAND;
        ((ast_macro_arg_expand_t*)result)->name = str_rep;

        if (!cxt->in_macro_def) {
            loc.end = GET_END_POINT(cxt);
            report_range_err(&loc, "macro argument expansion is only valid within a macro");
        }
    } else if ((result = parse_block_with_scope(cxt))) {}

    if (result != NULL) {
        loc.end     = GET_END_POINT(cxt);
        result->loc = loc;
    }

    return result;
}

static ast_t * parse_expr_more(parse_context_t *cxt, ast_t *left, int min_prec);

static ast_t * parse_operand(parse_context_t *cxt) {
    int               op;
    ast_t            *leaf;
    ast_unary_expr_t *unary;

    op = lookahead_unary_prefix_op(cxt);

    if (op == OP_INVALID
    || (op == OP_NEG && OPTIONAL_NO_EAT_INT(cxt, NULL))) {
        leaf = parse_leaf_expr(cxt);
        return leaf;
    }

    unary                = AST_ALLOC(cxt, ast_unary_expr_t);
    unary->op            = op;
    ASTP(unary)->kind    = AST_UNARY_EXPR;
    ASTP(unary)->loc.beg = GET_BEG_POINT(cxt);

    ASSERT(OPTIONAL_LIT(cxt, OP_MATCH(op)), "eat");

    unary->child = parse_operand(cxt);

    if (unary->child == NULL) {
        report_loc_err(GET_BEG_POINT(cxt), "missing inner operand to '%s' expression", OP_STR(op));
        return NULL;
    }

    unary->child = parse_expr_more(cxt, unary->child, HIGHEST_BIN_PREC);

    ASSERT(unary->child != NULL, "parse_expr_more() failed");

    ASTP(unary)->loc.end = unary->child->loc.end;

    if (op == OP_SLICE) {
        EXPECT_CHAR(cxt, ']', "expected ']'");
        ASTP(unary)->loc.end = GET_END_POINT(cxt);
    }

    return ASTP(unary);
}

static ast_t * parse_expr_more(parse_context_t *cxt, ast_t *left, int min_prec) {
    /*
    ** based on algorithm found here:
    ** https://en.wikipedia.org/wiki/Operator-precedence_parser#Pseudo-code
    */

    ast_t          *result;
    ast_t          *right;
    int             op;
    src_point_t     op_loc;
    int             split;
    src_point_t     split_end;
    int             op_prec;
    int             lookahead_op;
    ast_bin_expr_t *bin_result;

    result = right = NULL;

    /*
    ** lookahead := peek next token
    ** while lookahead is a binary operator whose precedence is >= min_precedence
    **     op := lookahead
    */

    op = OP_INVALID;

    while ((op = lookahead_binary_op(cxt)) != OP_INVALID
    &&     OP_PREC(op) >= min_prec) {
        /*
        ** advance to next token
        ** rhs := parse_primary()
        */

        op_loc = GET_BEG_POINT(cxt);

        split = 0;

        if (op == OP_CALL) {
            ASSERT(OPTIONAL_CHAR(cxt, '('), "eat");
            right = parse_arg_list(cxt, ')');

            if (right == NULL) {
                return NULL;
            } else {
                EXPECT_CHAR(cxt, ')', "expected ')'");
                split     = 1;
                split_end = GET_END_POINT(cxt);
            }
        } else if (op == OP_SUBSCRIPT) {
            ASSERT(OPTIONAL_CHAR(cxt, '['), "eat");
            right = parse_expr(cxt);

            if (right == NULL) {
                report_loc_err(GET_BEG_POINT(cxt), "missing inner operand to '[]' expression");
                return NULL;
            } else {
                EXPECT_CHAR(cxt, ']', "expected ']'");
                split     = 1;
                split_end = GET_END_POINT(cxt);
            }
        } else {
            ASSERT(OPTIONAL_LIT(cxt, OP_MATCH(op)), "eat");

            if (OP_IS_BINARY(op)) {
                right = parse_operand(cxt);
                if (right == NULL) {
                    report_loc_err(GET_BEG_POINT(cxt), "missing operand to binary '%s' expression", OP_STR(op));
                    return NULL;
                }
            }
        }

        op_prec = OP_PREC(op);

        /*
        ** lookahead := peek next token
        ** while lookahead is a binary operator whose
        ** precedence is greater than op's, or a
        ** right-associative operator whose precedence is
        ** equal to op's
        */

        lookahead_op = OP_INVALID;

        while ((((lookahead_op = lookahead_binary_op(cxt)) != OP_INVALID)
            && (OP_PREC(lookahead_op) > op_prec))
        ||     ((OP_ASSOC(lookahead_op) == OP_ASSOC_RIGHT)
            && (OP_PREC(lookahead_op) == op_prec))) {

            right = parse_expr_more(cxt, right, OP_PREC(lookahead_op));
            if (right == NULL) {
                report_loc_err(GET_BEG_POINT(cxt), "missing operand to binary '%s' expression", OP_STR(lookahead_op));
                return NULL;
            }
        }

        /* lhs := the result of applying op with operands lhs and rhs */

        ASSERT(left  != NULL, "binary expr has NULL left");
        ASSERT(right != NULL, "binary expr has NULL right");

        result = AST_ALLOC(cxt, ast_bin_expr_t);
        ASSERT(result != NULL, "new_bin_expr_from_op() failed");

        bin_result                = (ast_bin_expr_t*)result;
        bin_result->op            = op;
        bin_result->op_loc        = op_loc;
        bin_result->left          = left;
        bin_result->right         = right;
        ASTP(bin_result)->kind    = AST_BIN_EXPR;
        ASTP(bin_result)->loc.beg = left->loc.beg;
        ASTP(bin_result)->loc.end = (split ? split_end : right->loc.end);

        left = result;
    }

    result = left;

    return result;
}

static ast_t * parse_expr_prec(parse_context_t *cxt, int min_prec) {
    ast_t *operand;
    ast_t *top;

    operand = parse_operand(cxt);
    if (operand == NULL) { return NULL; }

    top = parse_expr_more(cxt, operand, min_prec);

    if (top != NULL) { top->flags |= AST_FLAG_EXPR_TOP; }

    return top;
}

static ast_t * parse_expr(parse_context_t *cxt) {
    return parse_expr_prec(cxt, 0);
}

static int parse_tags(parse_context_t *cxt, array_t *tags) {
    int    n_tags;
    ast_t *tag_expr;

    n_tags = 0;

    while (OPTIONAL_LIT(cxt, "[[")) {
        do {
            tag_expr = parse_expr(cxt);
            if (tag_expr == NULL) {
                report_loc_err(GET_END_POINT(cxt), "expected valid tag expression");
                return 0;
            }

            array_push(*tags, tag_expr);

            n_tags += 1;
        } while (OPTIONAL_CHAR(cxt, ','));

        EXPECT_LIT(cxt, "]]", "expected ']]'");
    }

    return n_tags;
}

static ast_t * parse_declaration(parse_context_t *cxt);

static ast_t * parse_struct_body(parse_context_t *cxt, string_id name) {
    ast_struct_t *result;
    ast_param_t  *param;
    ast_t        *decl;

    result                = AST_ALLOC(cxt, ast_struct_t);
    ASTP(result)->kind    = AST_STRUCT;
    ASTP(result)->loc.beg = GET_BEG_POINT(cxt);
    result->params        = array_make(ast_t*);
    result->monomorphs    = array_make(monomorphed_t);
    result->fields        = array_make(ast_t*);
    result->children      = array_make(ast_t*);
    result->mono_idx      = -1;

    ASTP(result)->flags |= AST_FLAG_CONSTANT;

    SCOPE_PUSH_NAMED(cxt, AST_STRUCT, ASTP(result), name);
    result->scope = SCOPE(cxt);

    cxt->in_struct         = 1;
    cxt->allow_poly_idents = 1;

    if (OPTIONAL_NO_EAT_CHAR(cxt, '(')) {
        eat(cxt, 1); /* ) */

        result->params_loc.beg = GET_BEG_POINT(cxt);

        while (!OPTIONAL_NO_EAT_CHAR(cxt, ')')
        ||     (((result->params_loc.end = GET_END_POINT(cxt)), 1) && (eat(cxt, 1) /* ) */, clean(cxt, 1), 0))) {

            cxt->poly_expr_pattern = 0;

            param                   = AST_ALLOC(cxt, ast_param_t);
            ASTP(param)->kind       = AST_PARAM;
            ASTP(param)->loc.beg    = GET_BEG_POINT(cxt);
            param->name             = STRING_ID_NULL;
            param->containing_scope = SCOPE(cxt);

            if (OPTIONAL_CHAR(cxt, '%')) {
                ASTP(param)->flags  |= AST_FLAG_POLYMORPH | AST_FLAG_POLY_IDENT;
                ASTP(result)->flags |= AST_FLAG_POLYMORPH | AST_FLAG_POLY_IDENT;
            }

            EXPECT_IDENT(cxt, &param->name, "expected parameter name");
            param->name_end = GET_END_POINT(cxt);
            ASTP(param)->loc.end = GET_END_POINT(cxt);

            INSTALL(cxt, param->name, ASTP(param));

            if (!(ASTP(param)->flags & AST_FLAG_POLYMORPH)) {
                report_range_err_no_exit(&ASTP(param)->loc, "struct parameters must be polymorphic");
                report_simple_info("to fix, change '%s' to '%%%s'", get_string(param->name), get_string(param->name));
            }

            EXPECT_CHAR(cxt, ':', "expected ':'");

            param->type_expr = parse_expr_prec(cxt, ASSIGNMENT_PREC + 1);

            if (cxt->poly_expr_pattern) {
                param->type_expr->flags |= AST_FLAG_POLYMORPH;
            }

            if (param->type_expr == NULL) {
                report_loc_err(GET_BEG_POINT(cxt),
                            "expected valid type expression for parameter '%s'",
                            get_string(param->name));
                return NULL;
            }

            if (OPTIONAL_CHAR(cxt, '=')) {
                param->val = parse_expr(cxt);
                if (param->val == NULL) {
                    report_loc_err(GET_BEG_POINT(cxt),
                                "expected valid expression as default value for parameter '%s'",
                                get_string(param->name));
                    return NULL;
                }
            }

            array_push(result->params, param);

            if (!OPTIONAL_CHAR(cxt, ',')) {
                result->params_loc.end = GET_END_POINT(cxt);
                EXPECT_CHAR(cxt, ')', "expected ')' to close the parameter list for struct '%s' or ',' to add more", get_string(name));
                break;
            }
        }
    }

    cxt->allow_poly_idents = 0;

    EXPECT_CHAR(cxt, '{', "expected '{' to open struct '%s'", get_string(name));

    while (!OPTIONAL_CHAR(cxt, '}')) {
        decl = parse_declaration(cxt);
        if (decl == NULL) {
            report_loc_err(GET_BEG_POINT(cxt), "expected valid declaration inside struct '%s'", get_string(name));
            return NULL;
        }
        if (decl->kind == AST_DECL_STRUCT_FIELD) {
            array_push(result->fields, decl);
        } else {
            array_push(result->children, decl);
        }
    }

    SCOPE_POP(cxt);

    ASTP(result)->loc.end = GET_END_POINT(cxt);

    cxt->in_struct = 0;

    return ASTP(result);
}

static ast_t * parse_macro_body(parse_context_t *cxt, string_id name) {
    ast_macro_t *result;
    string_id    param_name;
    src_range_t  loc;
    string_id   *it;

    result                 = AST_ALLOC(cxt, ast_macro_t);
    ASTP(result)->kind     = AST_MACRO;
    ASTP(result)->loc.beg  = GET_BEG_POINT(cxt);
    result->param_names    = array_make(string_id);
    result->is_block_macro = 0;

    cxt->in_macro_def      = 1;
    cxt->current_macro_def = result;


    ASTP(result)->flags |= AST_FLAG_CONSTANT;

    if (OPTIONAL_CHAR(cxt, '(')) {
    } else if (OPTIONAL_CHAR(cxt, '[')) {
        result->is_block_macro = 1;
    } else {
        EXPECT_CHAR(cxt, '(', "expected '(' or '[' to open the parameter list for macro '%s'", get_string(name));
    }

    while (!OPTIONAL_CHAR(cxt, result->is_block_macro ? ']' : ')')) {
        loc.beg = GET_BEG_POINT(cxt);
        EXPECT_IDENT(cxt, &param_name, "expected parameter name");
        loc.end = GET_END_POINT(cxt);

        array_traverse(result->param_names, it) {
            if (*it == param_name) {
                report_range_err(&loc, "reuse of macro parameter name '%s'", get_string(param_name));
                cxt->in_macro_def = 0;
                return NULL;
            }
        }

        array_push(result->param_names, param_name);

        if (!OPTIONAL_CHAR(cxt, ',')) {
            EXPECT_CHAR(cxt, result->is_block_macro ? ']' : ')', "expected '%c' to close the parameter list for macro '%s'", result->is_block_macro ? ']' : ')', get_string(name));
            break;
        }
    }

    result->block = parse_block(cxt);
    if (result->block == NULL) {
        report_loc_err(GET_BEG_POINT(cxt), "expected '{' to open macro '%s'", get_string(name));
        return NULL;
    }
    ASTP(result)->loc.end = result->block->loc.end;

    ASTP(result)->value.a = ASTP(result);

    cxt->current_macro_def = NULL;
    cxt->in_macro_def      = 0;

    return ASTP(result);
}

static ast_t * parse_module_body(parse_context_t *cxt, string_id name) {
    ast_module_t *result;
    ast_t        *decl;

    result                = AST_ALLOC(cxt, ast_module_t);
    ASTP(result)->kind    = AST_MODULE;
    ASTP(result)->loc.beg = GET_BEG_POINT(cxt);
    result->children      = array_make(ast_t*);

    ASTP(result)->flags |= AST_FLAG_CONSTANT;

    EXPECT_CHAR(cxt, '{', "expected '{' to open module '%s'", get_string(name));

    SCOPE_PUSH_NAMED(cxt, AST_MODULE, ASTP(result), name);
    result->scope = SCOPE(cxt);

    while (!OPTIONAL_CHAR(cxt, '}')) {
        decl = parse_declaration(cxt);
        if (decl == NULL) {
            report_loc_err(GET_BEG_POINT(cxt), "expected valid assigment inside module '%s'", get_string(name));
            return NULL;
        }
        array_push(result->children, decl);
    }

    SCOPE_POP(cxt);

    ASTP(result)->loc.end = GET_END_POINT(cxt);
    ASTP(result)->value.a = ASTP(result);

    return ASTP(result);
}

static ast_t * parse_stmt(parse_context_t *cxt);

static ast_t * _parse_block(parse_context_t *cxt, int do_scope) {
    ast_block_t *result;
    ast_t       *stmt;

    if (!OPTIONAL_NO_EAT_CHAR(cxt, '{')) { return NULL; }

    result                = AST_ALLOC(cxt, ast_block_t);
    ASTP(result)->kind    = AST_BLOCK;
    ASTP(result)->loc.beg = GET_BEG_POINT(cxt);
    result->stmts         = array_make(ast_t*);

    ASSERT(OPTIONAL_CHAR(cxt, '{'), "eat");

    if (do_scope) {
        SCOPE_PUSH(cxt, ASTP(result)->kind, ASTP(result));
    }
    result->scope = SCOPE(cxt);

    while (!OPTIONAL_NO_EAT_CHAR(cxt, '}')) {
        stmt = parse_stmt(cxt);
        if (stmt == NULL) {
            report_loc_err(GET_BEG_POINT(cxt), "expected valid statement");
            return NULL;
        }
        array_push(result->stmts, stmt);
    }
    result->end_brace_loc = GET_BEG_POINT(cxt);
    ASSERT(OPTIONAL_CHAR(cxt, '}'), "eat");

    if (do_scope) {
        SCOPE_POP(cxt);
    }

    ASTP(result)->loc.end = GET_END_POINT(cxt);

    return ASTP(result);
}


static ast_t * parse_block_with_scope(parse_context_t *cxt)       { return _parse_block(cxt, 1); }
static ast_t * parse_block(parse_context_t *cxt)                  { return _parse_block(cxt, 0); }

static ast_t * parse_if(parse_context_t *cxt) {
    ast_if_t *result;

    if (!OPTIONAL_NO_EAT_WORD(cxt, "if")) { return NULL; }

    result                = AST_ALLOC(cxt, ast_if_t);
    ASTP(result)->kind    = AST_IF;
    ASTP(result)->loc.beg = GET_BEG_POINT(cxt);

    ASSERT(OPTIONAL_WORD(cxt, "if"), "eat");

    ASTP(result)->loc.end = GET_END_POINT(cxt);

    SCOPE_PUSH(cxt, AST_IF, ASTP(result));
    result->scope = SCOPE(cxt);

    result->expr = parse_expr(cxt);
    if (result->expr == NULL) {
        report_loc_err(GET_BEG_POINT(cxt), "expected valid conditional expression after 'if'");
        return NULL;
    }

    result->then_block = parse_block(cxt);
    if (result->then_block == NULL) {
        report_loc_err(GET_BEG_POINT(cxt), "expected '{' to open 'if' statement");
        return NULL;
    }

    SCOPE_POP(cxt);

    if (OPTIONAL_WORD(cxt, "else")) {
        result->els = parse_if(cxt);

        if (result->els == NULL) {
            SCOPE_PUSH(cxt, AST_IF, ASTP(result));
            result->els = parse_block(cxt);
            ((ast_block_t*)result->els)->scope = SCOPE(cxt);

            if (result->els == NULL) {
                report_loc_err(GET_BEG_POINT(cxt), "expected '{' to open 'else' block");
                return NULL;
            }

            SCOPE_POP(cxt);
        }
    }

    return ASTP(result);
}

static ast_t * parse_loop(parse_context_t *cxt) {
    ast_loop_t *result;

    if (!OPTIONAL_NO_EAT_WORD(cxt, "loop")) { return NULL; }

    result                = AST_ALLOC(cxt, ast_loop_t);
    ASTP(result)->kind    = AST_LOOP;
    ASTP(result)->loc.beg = GET_BEG_POINT(cxt);

    ASSERT(OPTIONAL_WORD(cxt, "loop"), "eat");

    ASTP(result)->loc.end = GET_END_POINT(cxt);

    SCOPE_PUSH(cxt, AST_LOOP, ASTP(result));
    result->scope = SCOPE(cxt);

    result->init = result->cond = result->post = NULL;

    result->init = parse_declaration(cxt);
    if (result->init == NULL) {
        if (OPTIONAL_CHAR(cxt, ';')) { goto cond; }
        report_loc_err(GET_BEG_POINT(cxt), "expected valid declaration expression for 'loop'");
        return NULL;
    }
    if (result->init->kind != AST_DECL_VAR) {
        EXPECT_CHAR(cxt, ';', "expected ';'");
    }

cond:;
    result->cond = parse_expr(cxt);
    if (result->cond == NULL) {
        if (OPTIONAL_CHAR(cxt, ';')) { goto post; }
        report_loc_err(GET_BEG_POINT(cxt), "expected valid conditional expression for 'loop'");
        return NULL;
    }
    EXPECT_CHAR(cxt, ';', "expected ';'");

post:;
    result->post = parse_expr(cxt);
    if (result->post == NULL) {
        if (OPTIONAL_NO_EAT_CHAR(cxt, '{')) { goto block; }
        report_loc_err(GET_BEG_POINT(cxt), "expected valid post-expression for 'loop'");
        return NULL;
    }

block:;
    result->block = parse_block(cxt);
    if (result->block == NULL) {
        report_loc_err(GET_BEG_POINT(cxt), "expected '{' to open 'loop'");
        return NULL;
    }

    SCOPE_POP(cxt);

    return ASTP(result);
}

static ast_t *parse_return(parse_context_t *cxt) {
    ast_return_t *result;

    if (!OPTIONAL_NO_EAT_WORD(cxt, "return")) { return NULL; }

    result                = AST_ALLOC(cxt, ast_return_t);
    ASTP(result)->kind    = AST_RETURN;
    ASTP(result)->loc.beg = GET_BEG_POINT(cxt);

    ASSERT(OPTIONAL_WORD(cxt, "return"), "eat");

    result->expr = NULL;
    if (!OPTIONAL_NO_EAT_CHAR(cxt, ';')) {
        result->expr = parse_expr(cxt);
        if (result->expr == NULL) {
            report_loc_err(GET_BEG_POINT(cxt), "expected ';' or a valid expression in 'return' statement");
            return NULL;
        }
    }

    ASTP(result)->loc.end = GET_END_POINT(cxt);

    return ASTP(result);
}

static ast_t *parse_defer(parse_context_t *cxt) {
    ast_defer_t *result;

    if (!OPTIONAL_NO_EAT_WORD(cxt, "defer")) { return NULL; }

    result                = AST_ALLOC(cxt, ast_defer_t);
    ASTP(result)->kind    = AST_DEFER;
    ASTP(result)->loc.beg = GET_BEG_POINT(cxt);

    ASSERT(OPTIONAL_WORD(cxt, "defer"), "eat");

    ASTP(result)->loc.end = GET_END_POINT(cxt);

    result->block = parse_block(cxt);
    if (result->block == NULL) {
        report_loc_err(GET_BEG_POINT(cxt), "expected '{' to open 'defer' statement");
        return NULL;
    }

    return ASTP(result);
}

static ast_t *parse_break(parse_context_t *cxt) {
    ast_break_t *result;

    if (!OPTIONAL_NO_EAT_WORD(cxt, "break")) { return NULL; }

    result                = AST_ALLOC(cxt, ast_break_t);
    ASTP(result)->kind    = AST_BREAK;
    ASTP(result)->loc.beg = GET_BEG_POINT(cxt);

    ASSERT(OPTIONAL_WORD(cxt, "break"), "eat");

    ASTP(result)->loc.end = GET_END_POINT(cxt);

    return ASTP(result);
}

static ast_t *parse_continue(parse_context_t *cxt) {
    ast_continue_t *result;

    if (!OPTIONAL_NO_EAT_WORD(cxt, "continue")) { return NULL; }

    result                = AST_ALLOC(cxt, ast_continue_t);
    ASTP(result)->kind    = AST_CONTINUE;
    ASTP(result)->loc.beg = GET_BEG_POINT(cxt);

    ASSERT(OPTIONAL_WORD(cxt, "continue"), "eat");

    ASTP(result)->loc.end = GET_END_POINT(cxt);

    return ASTP(result);
}

static ast_t *parse_stmt(parse_context_t *cxt) {
    ast_t *result;

    if ((result = parse_macro_call(cxt, MACRO_STMT))) {
        if (((ast_macro_call_t*)result)->block == NULL) {
            EXPECT_CHAR(cxt, ';', "expected ';'");
        }
        goto out;
    }

    if ((result = parse_declaration(cxt)))            { /* Parses own semicolons. */           goto out; }
    if ((result = parse_if(cxt)))                     {                                        goto out; }
    if ((result = parse_loop(cxt)))                   {                                        goto out; }
    if ((result = parse_return(cxt)))                 { EXPECT_CHAR(cxt, ';', "expected ';'"); goto out; }
    if ((result = parse_defer(cxt)))                  {                                        goto out; }
    if ((result = parse_break(cxt)))                  { EXPECT_CHAR(cxt, ';', "expected ';'"); goto out; }
    if ((result = parse_continue(cxt)))               { EXPECT_CHAR(cxt, ';', "expected ';'"); goto out; }

    if (OPTIONAL_NO_EAT_LIT(cxt, "${}")) {
        result                                  = AST_ALLOC(cxt, ast_macro_arg_expand_t);
        result->kind                            = AST_MACRO_BLOCK_ARG_EXPAND;
        result->loc.beg                         = GET_BEG_POINT(cxt);
        ((ast_macro_arg_expand_t*)result)->name = 0;

        ASSERT(OPTIONAL_LIT(cxt, "${}"), "eat");

        result->loc.end = GET_END_POINT(cxt);

        if (!cxt->in_macro_def) {
            report_range_err(&result->loc, "macro argument expansion is only valid within a macro");
        }
        goto out;
    }

    if ((result = parse_block_with_scope(cxt)))       {                                        goto out; }
    if ((result = parse_expr(cxt)))                   { EXPECT_CHAR(cxt, ';', "expected ';'"); goto out; }

out:;
    return result;
}

static ast_t * parse_proc_body(parse_context_t *cxt, string_id name, int do_parse_block) {
    ast_proc_t  *result;
    int          seen_vargs;
    ast_param_t *param;

    result                = AST_ALLOC(cxt, ast_proc_t);
    ASTP(result)->kind    = AST_PROC;
    ASTP(result)->loc.beg = GET_BEG_POINT(cxt);
    result->params        = array_make(ast_t*);
    result->monomorphs    = array_make(monomorphed_t);
    result->mono_idx      = -1;

    ASTP(result)->flags |= AST_FLAG_CONSTANT;

    EXPECT_CHAR(cxt, '(', "expected '(' to open the parameter list for procedure '%s'", get_string(name));

    result->params_loc.beg = GET_BEG_POINT(cxt);

    SCOPE_PUSH_NAMED(cxt, AST_PROC, ASTP(result), name);
    result->scope = SCOPE(cxt);

    cxt->allow_poly_idents = 1;

    seen_vargs = 0;
    while (!OPTIONAL_NO_EAT_CHAR(cxt, ')')
    ||     (((result->params_loc.end = GET_END_POINT(cxt)), 1) && (eat(cxt, 1) /* ) */, clean(cxt, 1), 0))) {

        cxt->poly_expr_pattern = 0;

        if (seen_vargs) {
            report_loc_err(GET_BEG_POINT(cxt),
                           "parameters following a variadic argument list are not allowed");
        }

        param                   = AST_ALLOC(cxt, ast_param_t);
        ASTP(param)->kind       = AST_PARAM;
        ASTP(param)->loc.beg    = GET_BEG_POINT(cxt);
        param->name             = STRING_ID_NULL;
        param->containing_scope = SCOPE(cxt);

        if (OPTIONAL_CHAR(cxt, '%')) {
            ASTP(param)->flags  |= AST_FLAG_POLYMORPH | AST_FLAG_POLY_IDENT;
            ASTP(result)->flags |= AST_FLAG_POLYMORPH | AST_FLAG_POLY_IDENT;
        }

        EXPECT_IDENT(cxt, &param->name, "expected parameter name");
        param->name_end = GET_END_POINT(cxt);
        ASTP(param)->loc.end = GET_END_POINT(cxt);

        INSTALL(cxt, param->name, ASTP(param));

        EXPECT_CHAR(cxt, ':', "expected ':'");

        if (OPTIONAL_LIT(cxt, "%...")) {
            seen_vargs            = 1;
            ASTP(param)->flags   |= AST_FLAG_POLY_VARARGS;
            ASTP(result)->flags  |= AST_FLAG_POLYMORPH | AST_FLAG_POLY_VARARGS;
        } else {
            param->type_expr = parse_expr_prec(cxt, ASSIGNMENT_PREC + 1);

            if (cxt->poly_expr_pattern) {
                param->type_expr->flags |= AST_FLAG_POLYMORPH;
            }

            if (param->type_expr == NULL) {
                report_loc_err(GET_BEG_POINT(cxt),
                            "expected valid type expression for parameter '%s'",
                            get_string(param->name));
                return NULL;
            }

            if (OPTIONAL_CHAR(cxt, '=')) {
                param->val = parse_expr(cxt);
                if (param->val == NULL) {
                    report_loc_err(GET_BEG_POINT(cxt),
                                "expected valid expression as default value for parameter '%s'",
                                get_string(param->name));
                    return NULL;
                }
            }
        }

        array_push(result->params, param);

        if (!OPTIONAL_CHAR(cxt, ',')) {
            result->params_loc.end = GET_END_POINT(cxt);
            EXPECT_CHAR(cxt, ')', "expected ')' to close the parameter list for procedure '%s' or ',' to add more", get_string(name));
            break;
        }
    }

    cxt->allow_poly_idents = 0;

    if (array_len(result->params) > MAX_PARAMS_OR_ARGS) {
        report_range_err(&(*(ast_t**)array_item(result->params, MAX_PARAMS_OR_ARGS))->loc,
                         "procedure '%s' has exceeded the maximum number of parameters (%d)",
                         get_string(name),
                         MAX_PARAMS_OR_ARGS);
        return NULL;
    }

    if (OPTIONAL_CHAR(cxt, ':')) {
        result->ret_type_expr = parse_expr(cxt);
        if (result->ret_type_expr == NULL) {
            report_loc_err(GET_BEG_POINT(cxt),
                        "expected valid return type expression for procedure '%s'",
                        get_string(name));
            return NULL;
        }
    }

    if (do_parse_block) {
        result->block = parse_block(cxt);
        if (result->block == NULL) {
            report_loc_err(GET_BEG_POINT(cxt), "expected '{' to open procedure '%s', or ':' to specify its return type", get_string(name));
            return NULL;
        }
        ASTP(result)->loc.end = result->block->loc.end;
    } else {
        result->block = NULL;
        ASTP(result)->loc.end = GET_END_POINT(cxt);
    }

    SCOPE_POP(cxt);

    ASTP(result)->value.a = ASTP(result);

    return ASTP(result);
}

static string_id get_full_name(string_id name_id, scope_t *scope) {
    string_id   full_name;
    const char *scope_name;
    const char *name;
    char        buff[SCOPE_NAME_BUFF_SIZE];

    if (scope->kind == AST_GLOBAL_SCOPE) {
        full_name = name_id;
    } else {
        scope_name = get_string(scope->name_id);
        name       = get_string(name_id);

        if (strlen(scope_name) + strlen(name) + 2 > SCOPE_NAME_BUFF_SIZE) {
            report_simple_err("INTERNAL ERROR: name too long");
            ASSERT(0, "name too long");
        }

        strncpy(buff, scope_name, SCOPE_NAME_BUFF_SIZE - strlen(buff) - 1);
        strncat(buff, ".", SCOPE_NAME_BUFF_SIZE - strlen(buff) - 1);
        strncat(buff, name, SCOPE_NAME_BUFF_SIZE - strlen(buff) - 1);

        full_name = get_string_id(buff);
    }

    return full_name;
}

static ast_t * parse_declaration(parse_context_t *cxt) {
    ast_t       *macro_call;
    array_t      tags;
    ast_t      **tag_it;
    ast_t       *tag_expr;
    int          has_tags;
    src_range_t  loc;
    int          is_extern;
    string_id    name;
    int          kind;
    ast_decl_t  *result;
    int          value;
    int          var_shape_kind;
    int          must_be_const;
    const char  *s;

    if ((macro_call = parse_macro_call(cxt, MACRO_DECL)) != NULL) {
        if (((ast_macro_call_t*)macro_call)->block == NULL) {
            EXPECT_CHAR(cxt, ';', "expected ';'");
        }
        return macro_call;
    }

    tags             = array_make(ast_t*);
    has_tags         = parse_tags(cxt, &tags);
    is_extern        = 0;

    array_traverse(tags, tag_it) {
        tag_expr = *tag_it;

        if (tag_is_string(tag_expr, EXTERN_ID)) { is_extern = 1; }
    }

    loc.beg = GET_BEG_POINT(cxt);

    if (!OPTIONAL_NO_EAT_DECLARATION(cxt)) {
        if (has_tags) {
            EXPECT_IDENT(cxt, &name, "expected identifier after a declaration tag");
            EXPECT_CHAR(cxt, ':', "expected ':'");

            ASSERT(0, "should never get here");
        }
        return NULL;
    }

    result = AST_ALLOC(cxt, ast_decl_t);

    ASSERT(OPTIONAL_IDENT(cxt, &name), "parse_declaration_begin must be wrong");
    result->name_end = GET_END_POINT(cxt);
    ASSERT(OPTIONAL_CHAR(cxt, ':'), "parse_declaration_begin must be wrong");

    if (cxt->in_macro_def) {
        ASTP(result)->flags |= AST_FLAG_NAME_IN_MACRO;
    }

    result->containing_scope = SCOPE(cxt);
    if (result->containing_scope == cxt->global_scope) {
        result->containing_scope = global_scope;
    }

    if (has_tags) {
        result->tags = tags;
    }

    value = 0;

    if (OPTIONAL_CHAR(cxt, ':')) {
        ASTP(result)->flags |= AST_FLAG_CONSTANT;
        value = 1;
    } else if (OPTIONAL_CHAR(cxt, '=')) {
        value = 1;
    } else {
        if ((result->type_expr = parse_expr_prec(cxt, LOWEST_TYPE_EXPR_PREC)) == NULL) {
            report_loc_err(GET_BEG_POINT(cxt),
                        "expected a valid type expression in declaration of '%s'",
                        get_string(name));
            return NULL;
        }

        if (OPTIONAL_CHAR(cxt, '=')) {
            value = 1;
        }
    }

    /*
     * Declarations of these forms
     *
     *   name: type;    name: type = val;    name := val;
     *
     * should be considered struct fields when we're parsing a struct.
     * Notably,
     *
     *   name :: val;
     *
     * should remain a "variable" and be put in the structs list of other
     * (non-field) declarations.
     *
     * parse_struct_body() checks each declaration to decide if it's stored
     * as a field or general declaration.
     */
    var_shape_kind = !SCOPE(cxt)->in_proc && cxt->in_struct && !(ASTP(result)->flags & AST_FLAG_CONSTANT)
                        ? AST_DECL_STRUCT_FIELD
                        : AST_DECL_VAR;


    if (value) {
               if (OPTIONAL_WORD(cxt, "proc"))            { kind = AST_DECL_PROC;   loc.end = GET_END_POINT(cxt);
        } else if (OPTIONAL_WORD(cxt, "struct"))          { kind = AST_DECL_STRUCT; loc.end = GET_END_POINT(cxt);
        } else if (OPTIONAL_WORD(cxt, "macro"))           { kind = AST_DECL_MACRO;  loc.end = GET_END_POINT(cxt);
        } else if (OPTIONAL_WORD(cxt, "module"))          { kind = AST_DECL_MODULE; loc.end = GET_END_POINT(cxt);
        } else if ((result->val_expr = parse_expr(cxt))) { kind = var_shape_kind;  loc.end = result->val_expr->loc.end;
        } else {
            report_loc_err(GET_BEG_POINT(cxt),
                        "expected 'module', 'proc', 'type', 'macro', or a valid expression in initialization of '%s'",
                        get_string(name));
            return NULL;
        }
    } else {
        kind    = var_shape_kind;
        loc.end = GET_END_POINT(cxt);
        EXPECT_CHAR(cxt, ';', "expected ';' or '=' followed by an initialization expression");
    }

    ASTP(result)->kind = kind;
    ASTP(result)->loc  = loc;
    result->name       = name;
    result->full_name  = get_full_name(name, SCOPE(cxt));

    if (var_shape_kind != AST_DECL_STRUCT_FIELD
    &&  !SCOPE(cxt)->in_proc
    &&  !cxt->in_macro_def) {

        INSTALL(cxt, name, ASTP(result));
    }

    if (value) {
        if (!(ASTP(result)->flags & AST_FLAG_CONSTANT)) {
            must_be_const = 0;
            switch (kind) {
                case AST_DECL_PROC:
                    must_be_const = 1;
                    s             = "procedures";
                    break;
                case AST_DECL_STRUCT:
                    must_be_const = 1;
                    s             = "structs";
                    break;
                case AST_DECL_MACRO:
                    must_be_const = 1;
                    s             = "macros";
                    break;
                case AST_DECL_MODULE:
                    must_be_const = 1;
                    s             = "modules";
                    break;
            }
            if (must_be_const) {
                report_range_err_no_exit(&ASTP(result)->loc,
                                         "%s must be declared as constants", s);
                report_fixit(ASTP(result)->loc.beg,
                             "declare '%s' as a constant\a%s ::",
                             get_string(result->name),
                             get_string(result->name));
            }
        }

        switch (kind) {
            case AST_DECL_PROC:
                result->val_expr = parse_proc_body(cxt, name, !is_extern);
                OPTIONAL_CHAR(cxt, ';');
                ((ast_proc_t*)result->val_expr)->parent_decl = result;
                break;
            case AST_DECL_STRUCT:
                result->val_expr = parse_struct_body(cxt, name);
                OPTIONAL_CHAR(cxt, ';');
                ((ast_struct_t*)result->val_expr)->parent_decl = result;
                break;
            case AST_DECL_MACRO:
                if (cxt->in_macro_def) {
                    report_range_err(&ASTP(result)->loc, "macros may not be defined within other macros");
                }
                result->val_expr = parse_macro_body(cxt, name);
                OPTIONAL_CHAR(cxt, ';');
                ((ast_macro_t*)result->val_expr)->parent_decl = result;
                break;
            case AST_DECL_MODULE:
                result->val_expr = parse_module_body(cxt, name);
                OPTIONAL_CHAR(cxt, ';');
                ((ast_module_t*)result->val_expr)->parent_decl = result;
                break;
            case AST_DECL_VAR:
            case AST_DECL_STRUCT_FIELD:
                EXPECT_CHAR(cxt, ';', "expected ';'");
                break;
            default:
                ASSERT(0, "bad kind");
                break;
        }
    }

    if (result->val_expr != NULL) {
        ASTP(result)->value = result->val_expr->value;
        if (result->val_expr->flags & AST_FLAG_POLYMORPH) {
            ASTP(result)->flags |= AST_FLAG_POLYMORPH;
        }
    }

    return ASTP(result);
}

static void setup_cxt(parse_context_t *cxt) {
    cxt->tls = get_tls();

    cxt->end                = cxt->file->end;
    cxt->cursor             = cxt->file->buff;
    cxt->cur_point.path_id  = cxt->file->path_id;
    cxt->cur_point.line     = 1;
    cxt->cur_point.col      = 1;
    cxt->cur_point.buff_ptr = cxt->cursor;
    cxt->pre_clean_point    = cxt->cur_point;
    cxt->n_lines            = cxt->n_blank_lines = 0;
    cxt->top_level_nodes    = array_make(ast_t*);
    cxt->global_scope       = create_named_scope(global_scope, AST_GLOBAL_SCOPE, NULL, get_string_id("<global scope>"));
    cxt->scope_stack        = array_make(scope_t*);
    cxt->allow_poly_idents  = 0;
    cxt->macro_calls        = array_make(ast_macro_call_t*);

    array_push(cxt->scope_stack, cxt->global_scope);
}

static void parse(parse_context_t *cxt) {
    ast_t             *node;
    string_id          unexpected_identifier;
    src_range_t        unexpected_loc;
    ast_t            **node_it;
    int                i;
    string_id         *name_it;
    scope_t          **subscope_it;
    ast_macro_call_t **macro_it;

    setup_cxt(cxt);

    clean(cxt, 0);

    while (cxt->cursor < cxt->end) {
        node = parse_declaration(cxt);
        if (node == NULL) {
            unexpected_loc.beg = GET_BEG_POINT(cxt);
            if (OPTIONAL_IDENT(cxt, &unexpected_identifier)) {
                unexpected_loc.end = GET_END_POINT(cxt);
                report_range_err(&unexpected_loc, "unexpected identifier '%s'", get_string(unexpected_identifier));
            } else {
                report_loc_err(GET_END_POINT(cxt), "unexpected token '%c'", *cxt->cursor);
            }
        }
        clean(cxt, 0);
        array_push(cxt->top_level_nodes, node);
    }

    ROOTS_LOCK(); {
        array_traverse(cxt->top_level_nodes, node_it) {
            array_push(roots, *node_it);
        }
    } ROOTS_UNLOCK();

    array_free(cxt->top_level_nodes);

    GS_LOCK(); {
        i = 0;
        array_traverse(cxt->global_scope->symbols, name_it) {
            add_symbol(global_scope, *name_it, *(ast_t**)array_item(cxt->global_scope->nodes, i));
            i += 1;
        }
        array_traverse(cxt->global_scope->subscopes, subscope_it) {
            insert_subscope(global_scope, *subscope_it);
        }

        free_scope_no_recurse(cxt->global_scope);
    } GS_UNLOCK();

    MACRO_CALLS_LOCK(); {
        array_traverse(cxt->macro_calls, macro_it) {
             array_push(macro_calls, *macro_it);
        }
    } MACRO_CALLS_UNLOCK();

    array_free(cxt->macro_calls);

    LINES_LOCK(); {
        n_lines       += cxt->n_lines;
        n_blank_lines += cxt->n_blank_lines;
    } LINES_UNLOCK();
}

#define CHECK_SEEN_FILE(_pth)                                                             \
do {                                                                                      \
    string_id path_id = get_string_id((_pth));                                            \
    if (get_ifile(path_id) != NULL) {                                                     \
        verb_message("         file '%s' already counted amongst input files\n", (_pth)); \
        return;                                                                           \
    }                                                                                     \
} while (0)

static void parse_file_thread(void *_path) {
    const char      *path;
    u64              start_us;
    parse_context_t  cxt;

    path = (const char*)_path;

    CHECK_SEEN_FILE(path);

    start_us = measure_time_now_us();

    memset(&cxt, 0, sizeof(cxt));

    cxt.file = add_ifile_readonly(get_string_id(path));

    parse(&cxt);

    verb_message("  parsed file '%s' (%lu lines, %s) in %lu us\n",
                 path,
                 cxt.n_lines,
                 pretty_bytes(cxt.file->len),
                 measure_time_now_us() - start_us);
}

void parse_file(const char *path) {
    u64             start_us;
    parse_context_t cxt;

    CHECK_SEEN_FILE(path);

    start_us = measure_time_now_us();

    memset(&cxt, 0, sizeof(cxt));

    cxt.file = add_ifile_readonly(get_string_id(path));

    parse(&cxt);

    verb_message("  parsed file '%s' (%lu lines, %s) in %lu us\n",
                 path,
                 cxt.n_lines,
                 pretty_bytes(cxt.file->len),
                 measure_time_now_us() - start_us);
}

void start_parsing_file_async(const char *path) {
    CHECK_SEEN_FILE(path);
    tp_add_task(tp, parse_file_thread, (void*)path);
}

void wait_for_parsing_async(void) { tp_wait(tp); }
