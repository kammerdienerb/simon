#include "c_backend.h"
#include "internal.h"
#include "ast.h"
#include "file.h"
#include "options.h"
#include "ui.h"
#include "globals.h"
#include "type.h"
#include "parse.h"
#include "array.h"

static u32 which_varg;

typedef struct {
    string_id string;
    u32       ty;
} Proc_Type_String;

#define EMIT_C(c)                                \
do {                                             \
    fputc((c), output_file);                     \
} while (0)

#define EMIT_STRING_F(_fmt, ...)                 \
do {                                             \
    fprintf(output_file, (_fmt), ##__VA_ARGS__); \
} while (0)

#define EMIT_STRING_N(_s, _len)                  \
do {                                             \
    fwrite((_s), (_len), 1, output_file);        \
} while (0)

#define EMIT_STRING(_s)     EMIT_STRING_N((_s), strlen((_s)))
#define EMIT_STRING_ID(_id) EMIT_STRING(get_string((_id)))

static void emit_prelude(void) {
    const char *prelude =
"typedef unsigned char      u8;\n"
"typedef unsigned short     u16;\n"
"typedef unsigned int       u32;\n"
"typedef unsigned long long u64;\n"
"typedef signed char        s8;\n"
"typedef short              s16;\n"
"typedef int                s32;\n"
"typedef long long          s64;\n"
"typedef float              f32;\n"
"typedef double             f64;\n"
"\n"
"typedef struct { void *data; u64 len; } __si_slice;\n"
"\n"
"#define __si_slice_data(_s)         ((_s).data)\n"
"#define __si_slice_len(_s)          ((_s).len)\n"
"#define __si_slice_idx(_s, _i, _t)  (((_t*)__si_slice_data(_s)) + (_i))\n"
"#define _builtin_slice_from(_p, _l) ((__si_slice){ (_p), (_l) })\n"
"\n"
"#define _builtin_stack_alloc(_n) (__builtin_alloca((_n)))\n"
"#ifdef __x86_64__\n"
"__attribute__((used, always_inline))\n"
"static inline void _builtin_outb(u8* addr, u8 b) { __asm__ volatile ( \"outb %0, %1\" : : \"a\"(b), \"Nd\"((u16)(u64)addr) );                      }\n"
"__attribute__((used, always_inline))\n"
"static inline u8   _builtin_inb(u8* addr)        { u8 ret; __asm__ volatile ( \"inb %1, %0\" : \"=a\"(ret) : \"Nd\"((u16)(u64)addr) ); return ret; }\n"
"#endif\n"
"\n";

    EMIT_STRING(prelude);
}

#define INDENT(_lvl)                                            \
do {                                                            \
    int _i;                                                     \
    for (_i = 0; _i < (_lvl); _i += 1) { EMIT_STRING("    "); } \
} while (0)


static void emit_name(ast_t *node, i32 poly_idx) {
    ast_decl_t    *decl;
    const char    *full_name;
    u32            len;
    u32            i;
    ast_param_t   *param;
    ast_builtin_t *builtin;

    if (node->kind == AST_DECL_VAR) {
        decl = (ast_decl_t*)node;
        EMIT_STRING_ID(decl->name);
    } else if (ast_kind_is_decl(node->kind)) {
        decl = (ast_decl_t*)node;

        full_name = get_string(decl->full_name);
        len       = strlen(full_name);
        for (i = 0; i < len; i += 1) {
            if (full_name[i] == '.') {
                EMIT_C('_');
                EMIT_C('_');
            } else {
                EMIT_C(full_name[i]);
            }
        }
        if (poly_idx >= 0) {
            EMIT_STRING_F("__p%d", poly_idx);
        }
    } else if (node->kind == AST_PARAM) {
        param = (ast_param_t*)node;
        EMIT_STRING_ID(param->name);
    } else if (node->kind == AST_BUILTIN) {
        builtin = (ast_builtin_t*)node;
        EMIT_STRING_ID(builtin->name);
    }
}

static void emit_type(u32 ty) {
    type_t            t;
    u32               under;
    ast_decl_t       *decl;

    switch (type_kind(ty)) {
        case TY_GENERIC_INT:
        case TY_GENERIC_FLOAT:
            EMIT_STRING(get_string(get_type_string_id(ty)));
            break;
        case TY_STRUCT:
        case TY_STRUCT_MONO:
            t    = get_type_t(ty);
            decl = t.decl;
            emit_name(ASTP(decl), (ASTP(decl)->flags & AST_FLAG_POLYMORPH) ? t.mono_constants_idx : -1);
            break;
        case TY_PTR:
            under = get_under_type(ty);
            if (type_kind(under) == TY_PROC) {
                ty = under;
                goto proc;
            }
            emit_type(under);
            EMIT_C('*');
            break;
        case TY_SLICE:
            EMIT_STRING("__si_slice");
            break;
        case TY_PROC:
proc:;
            EMIT_STRING_F("__proc_type_%u", ty);
            break;
        default:
            report_simple_err_no_exit("INTERNAL ERROR: unhandled type in emit_type(): %s", get_string(get_type_string_id(ty)));
            ASSERT(0, "don't know how to emit this type");
    }
}

static void emit_struct_pre_decl(ast_decl_t *decl, ast_struct_t *st, i32 poly_idx) {
    if (st->bitfield_struct_bits != 0) {
        EMIT_STRING_F("typedef u%d ", st->bitfield_struct_bits);
        emit_name(ASTP(decl), poly_idx);
        EMIT_STRING(";\n");
    } else {
        EMIT_STRING("struct ");
        emit_name(ASTP(decl), poly_idx);
        EMIT_STRING("_struct;\n");
        EMIT_STRING("typedef struct ");
        emit_name(ASTP(decl), poly_idx);
        EMIT_STRING("_struct ");
        emit_name(ASTP(decl), poly_idx);
        EMIT_STRING(";\n");
    }
}

static void emit_struct_pre_decls(void) {
    ast_decl_t    **it;
    ast_decl_t     *decl;
    ast_struct_t   *st;
    i32             poly_idx;
    monomorphed_t  *mit;

    array_traverse(all_types, it) {
        decl = *it;
        st   = (ast_struct_t*)decl->val_expr;

        if (ASTP(decl)->flags & AST_FLAG_POLYMORPH) {
            poly_idx = 0;
            array_traverse(st->monomorphs, mit) {
                if (mit->specialization == NULL) {
                    emit_struct_pre_decl(decl, st, poly_idx);
                }
                poly_idx += 1;
            }
        } else {
            emit_struct_pre_decl(decl, st, -1);
        }
    }
}

static void emit_struct(ast_decl_t *decl, ast_struct_t *st, i32 poly_idx) {
    ast_t      **fit;
    ast_decl_t  *field;

    if (st->bitfield_struct_bits != 0) { return; }

    EMIT_STRING("struct ");
    emit_name(ASTP(decl), poly_idx);
    EMIT_STRING("_struct {\n");
    array_traverse(st->fields, fit) {
        field = (ast_decl_t*)*fit;
        INDENT(1);
        emit_type(ASTP(field)->type);
        EMIT_C(' ');
        EMIT_STRING_ID(field->name);
        EMIT_STRING(";\n");
    }
    EMIT_STRING("};\n\n");
}

static void emit_structs(void) {
    ast_decl_t    **it;
    ast_decl_t     *decl;
    ast_struct_t   *st;
    i32             poly_idx;
    monomorphed_t  *mit;

    array_traverse(all_types, it) {
        decl = *it;
        st   = (ast_struct_t*)decl->val_expr;

        if (ASTP(decl)->flags & AST_FLAG_POLYMORPH) {
            poly_idx = 0;
            array_traverse(st->monomorphs, mit) {
                if (mit->specialization == NULL) {
                    emit_struct(decl, (ast_struct_t*)mit->node, poly_idx);
                }
                poly_idx += 1;
            }
        } else {
            emit_struct(decl, st, -1);
        }
    }
}

static void emit_proc_types(void) {
    u32         ty;
    type_t     *it;
    const char *lazy_comma;
    u32         n;
    u32         i;

    ty = 0;

    array_traverse(type_table, it) {
        if (ty != TY_PROC
        &&  it->kind == TY_PROC
        &&  !type_is_poly(ty)) {

            EMIT_STRING("typedef ");
            if (it->ret_id == TY_NOT_TYPED) {
                EMIT_STRING("void");
            } else {
                emit_type(it->ret_id);
            }

            EMIT_STRING_F(" (*__proc_type_%u)(", ty);

            lazy_comma = "";
            n          = get_num_param_types(ty);
            if (n == 0) {
                EMIT_STRING("void");
            } else {
                for (i = 0; i < n; i += 1) {
                    EMIT_STRING(lazy_comma);
                    emit_type(get_param_type(ty, i));
                    lazy_comma = ", ";
                }
            }

            EMIT_STRING(");\n");
        }
        ty += 1;
    }
}

static void emit_param(ast_param_t *param, const char *lazy_comma) {
    type_t list_type;
    int    i;

    if (ASTP(param)->flags & AST_FLAG_VARARGS) {
        if (ASTP(param)->flags & AST_FLAG_POLY_VARARGS) {
            list_type  = get_type_t(ASTP(param)->type);
            for (i = 0; i < list_type.list_len; i += 1) {
                EMIT_STRING(lazy_comma);
                emit_type(list_type.id_list[i]);
                EMIT_C(' ');
                EMIT_STRING_F("__varg%d", i);
                lazy_comma = ", ";
            }
        } else {
            ASSERT(0, "TODO");
        }
    } else {
        EMIT_STRING(lazy_comma);
        emit_type(ASTP(param)->type);
        EMIT_C(' ');
        EMIT_STRING_ID(param->name);
    }
}

static void emit_proc_pre_decl(ast_decl_t *decl, ast_proc_t *proc, i32 poly_idx) {
    const char   *lazy_comma;
    ast_t       **pit;
    ast_param_t  *param;

    if (proc->ret_type_expr != NULL) {
        emit_type(proc->ret_type_expr->value.t);
        EMIT_C(' ');
    } else {
        EMIT_STRING("void ");
    }

    emit_name(ASTP(decl), poly_idx);

    if (array_len(proc->params) == 0) {
        EMIT_STRING("(void)");
    } else {
        EMIT_C('(');
        lazy_comma = "";
        array_traverse(proc->params, pit) {
            param = (ast_param_t*)*pit;

            if (ASTP(param)->type == TY_TYPE
            ||  ASTP(param)->type == TY_MODULE) {

                continue;
            }

            emit_param(param, lazy_comma);
            lazy_comma = ", ";
        }
        EMIT_C(')');
    }

    if (!(ASTP(proc)->flags & AST_FLAG_IS_EXTERN)) {
        EMIT_STRING(" __asm__ (\"");
        EMIT_STRING_ID(decl->full_name);
        if (poly_idx >= 0) {
            EMIT_STRING_F(".%d", poly_idx);
        }
        EMIT_STRING("\")");
    }

    EMIT_STRING(";\n");
}

static void emit_proc_pre_decls(void) {
    ast_decl_t    **it;
    ast_decl_t     *decl;
    ast_proc_t     *proc;
    i32             poly_idx;
    monomorphed_t  *mit;

    array_traverse(all_procs, it) {
        decl = *it;
        proc = (ast_proc_t*)decl->val_expr;

        if (ASTP(proc)->flags & AST_FLAG_POLYMORPH) {
            poly_idx = 0;
            array_traverse(proc->monomorphs, mit) {
                if (mit->specialization == NULL) {
                    emit_proc_pre_decl(decl, (ast_proc_t*)mit->node, poly_idx);
                }
                poly_idx += 1;
            }
        } else {
            emit_proc_pre_decl(decl, proc, -1);
        }
    }
}

static void emit_expr(ast_t *expr);

static void emit_vars(void) {
    ast_decl_t **it;
    ast_decl_t  *decl;

    array_traverse(all_vars, it) {
        decl = *it;

        if (ASTP(decl)->flags & AST_FLAG_CONSTANT) {
            continue;
        }

        emit_type(ASTP(decl)->type);
        EMIT_C(' ');
        emit_name(ASTP(decl), -1);
        EMIT_C(';');
        EMIT_C('\n');
    }
}

static void emit_unary_expr(ast_t *expr) {
    int    op;
    ast_t *child;

    op    = ((ast_unary_expr_t*)expr)->op;
    child = ((ast_unary_expr_t*)expr)->child;
    (void)child;

    switch (op) {
        case OP_NEG:
            EMIT_STRING(OP_STR(op));
            emit_expr(child);
            break;
        case OP_NOT:
            EMIT_STRING("!(");
            emit_expr(child);
            EMIT_C(')');
            break;
        case OP_ADDR:
            EMIT_STRING("&(");
            emit_expr(child);
            EMIT_C(')');
            break;
        case OP_DEREF:
            EMIT_STRING("(*(");
            emit_expr(child);
            EMIT_STRING("))");
            break;
        case OP_SIZEOF:
            EMIT_STRING("sizeof ");
            emit_expr(child);
            break;
        case OP_LENOF:
        	EMIT_C('(');
            emit_expr(child);
        	EMIT_STRING(").len");
            break;
        default:
            report_simple_err_no_exit("INTERNAL ERROR: unhandled expression in emit_unary_expr(): %s", OP_STR(op));
            ASSERT(0, "don't know how to emit this expression");
    }
}

static void emit_binary_expr(ast_t *expr) {
    int              op;
    ast_t           *l;
    ast_t           *r;
    ast_arg_list_t  *arg_list;
    arg_t           *arg;
    arg_t           *it;
    const char      *lazy_comma = "";
    u32              st_ty;
    ast_decl_t      *st_decl;
    ast_struct_t    *st;
    ast_t          **field_it;
    ast_decl_t      *field;
    u32              i;

    op = ((ast_bin_expr_t*)expr)->op;
    l  = ((ast_bin_expr_t*)expr)->left;
    r  = ((ast_bin_expr_t*)expr)->right;

    (void)l;
    (void)r;

    switch (op) {
        case OP_CALL:
            if (expr->flags & AST_FLAG_CALL_IS_CAST) {
                EMIT_STRING("((");
                arg_list = (ast_arg_list_t*)r;
                arg      = array_item(arg_list->args, 0);
                emit_type(arg->expr->value.t);
                EMIT_STRING(")(");
                arg = array_item(arg_list->args, 1);
                emit_expr(arg->expr);
                EMIT_STRING("))");
            } else if (expr->flags & AST_FLAG_CALL_IS_BUILTIN_VARG) {
                EMIT_STRING_F("__varg%d", which_varg);
            } else {
                emit_expr(l);
                EMIT_C('(');
                array_traverse(((ast_arg_list_t*)r)->args, it) {
                    EMIT_STRING(lazy_comma);
                    emit_expr(it->expr);
                    lazy_comma = ", ";
                }
                EMIT_C(')');
            }
            break;
        case OP_SUBSCRIPT:
            EMIT_STRING("(*__si_slice_idx(");
            emit_expr(l);
            EMIT_STRING(", ");
            emit_expr(r);
            EMIT_STRING(", ");
            emit_type(get_under_type(l->type));
            EMIT_STRING("))");
            break;
        case OP_DOT:
            if (expr->flags & AST_FLAG_BITFIELD_DOT) {
                EMIT_C('(');
                EMIT_C('(');
                if (type_kind(l->type) == TY_PTR) {
                    EMIT_C('(');
                    EMIT_C('*');
                    emit_expr(l);
                    EMIT_C(')');
                } else {
                    emit_expr(l);
                }

                st_ty   = l->type;
                st_decl = struct_type_to_decl(st_ty);
                ASSERT(st_decl != NULL, "did not get struct decl");

                st = (ast_struct_t*)st_decl->val_expr;

                field = NULL;
                array_traverse(st->fields, field_it) {
                    field = (ast_decl_t*)*field_it;
                    if (((ast_ident_t*)r)->str_rep == field->name) {
                        break;
                    }
                    field = NULL;
                }

                ASSERT(field != NULL, "did not get field");

                EMIT_STRING_F(" & 0x%"PRIx64, field->bitfield_mask);

                EMIT_C(')');

                if (field->bitfield_shift) {
                    EMIT_STRING_F(" >> %d", field->bitfield_shift);
                }

                EMIT_C(')');
            } else {
                emit_expr(l);
                if (type_kind(l->type) == TY_PTR) {
                    EMIT_C('-');
                    EMIT_C('>');
                } else {
                    EMIT_C('.');
                }
                ASSERT(r->kind == AST_IDENT, "right of dot not an ident");
                EMIT_STRING_ID(((ast_ident_t*)r)->str_rep);
            }
            break;
        case OP_PLUS_ASSIGN:
        case OP_MINUS_ASSIGN:
        case OP_MULT_ASSIGN:
        case OP_DIV_ASSIGN:
        case OP_MOD_ASSIGN:
        case OP_ASSIGN:
            if (l->flags & AST_FLAG_BITFIELD_DOT) {
                st_ty   = ((ast_bin_expr_t*)l)->left->type;
                st_decl = struct_type_to_decl(st_ty);
                ASSERT(st_decl != NULL, "did not get struct decl");

                st = (ast_struct_t*)st_decl->val_expr;

                field = NULL;
                array_traverse(st->fields, field_it) {
                    field = (ast_decl_t*)*field_it;
                    if (((ast_ident_t*)((ast_bin_expr_t*)(l))->right)->str_rep == field->name) {
                        break;
                    }
                    field = NULL;
                }

                ASSERT(field != NULL, "did not get field");

                emit_expr(((ast_bin_expr_t*)l)->left);
                EMIT_STRING(" = (");
                emit_expr(((ast_bin_expr_t*)l)->left);
                EMIT_STRING_F(" & ~0x%"PRIx64") | (", field->bitfield_mask);

                if (op == OP_ASSIGN) {
                    EMIT_STRING("(((u64)");
                    emit_expr(r);
                    EMIT_C(')');
                    EMIT_STRING_F(" << %dULL) & 0x%"PRIx64")", field->bitfield_shift, field->bitfield_mask);
                } else {
                    emit_expr(((ast_bin_expr_t*)l)->left);
                    EMIT_C('(');
                    EMIT_C('(');
                    emit_expr(l);
                    EMIT_C(' ');
                    for (i = 0; i < strlen(OP_STR(op)) - 1; i += 1) {
                        EMIT_C(OP_STR(op)[i]);
                    }
                    EMIT_STRING_F(" ((u64)");
                    emit_expr(r);
                    EMIT_C(')');
                    EMIT_C(')');
                    EMIT_STRING_F(" << %dULL) & 0x%"PRIx64")", field->bitfield_shift, field->bitfield_mask);
                }
            } else {
                emit_expr(l);
                EMIT_STRING_F(" %s ", OP_STR(op));
                emit_expr(r);
            }
            break;
        case OP_PLUS:
        case OP_MINUS:
        case OP_MULT:
        case OP_DIV:
        case OP_MOD:
        case OP_BSHL:
        case OP_BSHR:
        case OP_BAND:
        case OP_BOR:
        case OP_BXOR:
        case OP_EQU:
        case OP_NEQ:
        case OP_LSS:
        case OP_LEQ:
        case OP_GTR:
        case OP_GEQ:
            emit_expr(l);
            EMIT_STRING_F(" %s ", OP_STR(op));
            emit_expr(r);
            break;
        case OP_AND:
            emit_expr(l);
            EMIT_STRING(" && ");
            emit_expr(r);
            break;
        case OP_OR:
            emit_expr(l);
            EMIT_STRING(" || ");
            emit_expr(r);
            break;
        default:
            report_simple_err_no_exit("INTERNAL ERROR: unhandled expression in emit_binary_expr(): %s", OP_STR(op));
            ASSERT(0, "don't know how to emit this expression");
    }
}

static void emit_expr(ast_t *expr) {
    ast_ident_t   *ident;
    ast_t         *resolved_node;
    ast_decl_t    *decl;
    ast_proc_t    *proc;
    monomorphed_t *spec_monomorphed;

    switch (expr->kind) {
        case AST_UNARY_EXPR:
            emit_unary_expr(expr);
            break;
        case AST_BIN_EXPR:
            emit_binary_expr(expr);
            break;
        case AST_INT:
            EMIT_STRING_ID(((ast_int_t*)expr)->str_rep);
            break;
        case AST_FLOAT:
            EMIT_STRING_ID(((ast_float_t*)expr)->str_rep);
            break;
        case AST_STRING:
            EMIT_STRING("((__si_slice){ ");
            EMIT_STRING_ID(((ast_string_t*)expr)->str_rep);
            EMIT_STRING(", (sizeof ");
            EMIT_STRING_ID(((ast_string_t*)expr)->str_rep);
            EMIT_STRING(") - 1 })");
            break;
        case AST_CHAR:
            EMIT_STRING_ID(((ast_char_t*)expr)->str_rep);
            break;
        case AST_IDENT:
            ident         = (ast_ident_t*)expr;
            resolved_node = ident->resolved_node;

            if (ident->varg_idx >= 0) {
                EMIT_STRING_F("__varg%d", ident->varg_idx);
            } else if (expr->flags & AST_FLAG_CONSTANT
            &&  resolved_node->kind == AST_DECL_VAR) {

                emit_expr(((ast_decl_t*)resolved_node)->val_expr);

            } else if (resolved_node->kind == AST_DECL_PROC
                   && resolved_node->flags & AST_FLAG_POLYMORPH
                   &&  ident->poly_idx >= 0) {

                decl             = (ast_decl_t*)resolved_node;
                proc             = (ast_proc_t*)decl->val_expr;
                spec_monomorphed = array_item(proc->monomorphs, ident->poly_idx);

                if (spec_monomorphed->specialization == NULL) { goto normal_name; }

                emit_name(spec_monomorphed->specialization, -1);

            } else {
normal_name:;
                emit_name(((ast_ident_t*)expr)->resolved_node, ((ast_ident_t*)expr)->poly_idx);
            }

            break;
        default:
            ASSERT(0, "not an expression");
    }
}

enum {
    FMT_FLAGS_NONE = 0,
    FMT_NO_INDENT  = 1 << 0,
    FMT_NO_END     = 1 << 1,
};

static void emit_stmt(ast_t *stmt, int lvl, int fmt_flags) {
    ast_t **it;
    ast_t  *s;

    if (stmt->kind == AST_DECL_VAR && stmt->flags & AST_FLAG_CONSTANT) {
        return;
    }

    if (!(fmt_flags & FMT_NO_INDENT) && stmt->kind != AST_VARGS_BLOCK) {
        INDENT(lvl);
    }

    switch (stmt->kind) {
        case AST_BLOCK:
            EMIT_STRING("{\n");
            array_traverse(((ast_block_t*)stmt)->stmts, it) {
                s = *it;
                emit_stmt(s, lvl + 1, FMT_FLAGS_NONE);
            }
            INDENT(lvl);
            EMIT_STRING("}");
            if (!(fmt_flags & FMT_NO_END)) {
                EMIT_STRING("\n\n");
            }
            break;

        case AST_VARGS_BLOCK:
            which_varg = 0;
            array_traverse(((ast_vargs_block_t*)stmt)->new_blocks, it) {
                emit_stmt(*it, lvl, FMT_FLAGS_NONE);
                which_varg += 1;
            }
            break;

        case AST_DECL_VAR:
            emit_type(stmt->type);
            EMIT_C(' ');
            EMIT_STRING_ID(((ast_decl_t*)stmt)->name);

            if (((ast_decl_t*)stmt)->val_expr) {
                EMIT_STRING(" = ");
                emit_expr(((ast_decl_t*)stmt)->val_expr);
            }

            if (!(fmt_flags & FMT_NO_END)) {
                EMIT_STRING(";\n");
            }
            break;

        case AST_IF:
            EMIT_STRING("if (");
            emit_expr(((ast_if_t*)stmt)->expr);
            EMIT_STRING(") ");
            emit_stmt(((ast_if_t*)stmt)->then_block, lvl, FMT_NO_INDENT | FMT_NO_END);
            if (((ast_if_t*)stmt)->els != NULL) {
                EMIT_STRING(" else ");
                emit_stmt(((ast_if_t*)stmt)->els, lvl, FMT_NO_INDENT);
            } else {
                EMIT_STRING("\n\n");
            }
            break;

        case AST_LOOP:
            EMIT_STRING("for (");

            if (((ast_loop_t*)stmt)->init != NULL) {
                emit_stmt(((ast_loop_t*)stmt)->init, lvl, FMT_NO_INDENT | FMT_NO_END);
            }
            EMIT_STRING("; ");

            if (((ast_loop_t*)stmt)->cond != NULL) {
                emit_stmt(((ast_loop_t*)stmt)->cond, lvl + 1, FMT_NO_INDENT | FMT_NO_END);
            }
            EMIT_STRING("; ");

            if (((ast_loop_t*)stmt)->post != NULL) {
                emit_stmt(((ast_loop_t*)stmt)->post, lvl, FMT_NO_INDENT | FMT_NO_END);
            }
            EMIT_STRING(")");

            emit_stmt(((ast_loop_t*)stmt)->block, lvl, FMT_NO_INDENT);
            break;

        case AST_RETURN:
            EMIT_STRING("return");
            if (((ast_return_t*)stmt)->expr != NULL) {
                EMIT_C(' ');
                emit_expr(((ast_return_t*)stmt)->expr);
            }
            EMIT_STRING(";\n");
            break;

/*         case AST_DEFER: */
/*             break; */

        case AST_BREAK:
            EMIT_STRING("break;\n");
            break;

        case AST_CONTINUE:
            EMIT_STRING("continue;\n");
            break;

#define X(kind) case kind:
        X_AST_ALL_EXPRS
#undef X
            emit_expr(stmt);
            if (!(fmt_flags & FMT_NO_END)) {
                EMIT_STRING(";\n");
            }
            break;

        default:
            report_simple_err_no_exit("INTERNAL ERROR: unhandled statement in emit_stmt(): %s", ast_get_kind_str(stmt->kind));
            ASSERT(0, "don't know how to emit this statement");
    }
}

static void emit_proc(ast_decl_t *decl, ast_proc_t *proc, i32 poly_idx) {
    const char   *lazy_comma;
    ast_t       **pit;
    ast_param_t  *param;

    if (proc->ret_type_expr != NULL) {
        emit_type(proc->ret_type_expr->value.t);
        EMIT_C(' ');
    } else {
        EMIT_STRING("void ");
    }

    emit_name(ASTP(decl), poly_idx);

    if (array_len(proc->params) == 0) {
        EMIT_STRING("(void) ");
    } else {
        EMIT_C('(');
        lazy_comma = "";
        array_traverse(proc->params, pit) {
            param = (ast_param_t*)*pit;

            if (ASTP(param)->type == TY_TYPE
            ||  ASTP(param)->type == TY_MODULE) {

                continue;
            }

            emit_param(param, lazy_comma);
            lazy_comma = ", ";
        }
        EMIT_C(')');
        EMIT_C(' ');
    }

    emit_stmt(proc->block, 0, FMT_NO_INDENT);
}

static void emit_procs(void) {
    ast_decl_t    **it;
    ast_decl_t     *decl;
    ast_proc_t     *proc;
    i32             poly_idx;
    monomorphed_t  *mit;

    array_traverse(all_procs, it) {
        decl = *it;
        proc = (ast_proc_t*)decl->val_expr;

        if (ASTP(proc)->flags & AST_FLAG_IS_EXTERN) {
            continue;
        }

        if (ASTP(proc)->flags & AST_FLAG_POLYMORPH) {
            poly_idx = 0;
            array_traverse(proc->monomorphs, mit) {
                if (mit->specialization == NULL) {
                    emit_proc(decl, (ast_proc_t*)mit->node, poly_idx);
                }
                poly_idx += 1;
            }
        } else {
            emit_proc(decl, proc, -1);
        }
    }
}

void do_c_backend(void) {
    u64  start_us;
    int  err;
    char exe_name[128];
    char cmd_buff[4096];

    start_us = measure_time_now_us();

    emit_prelude();          EMIT_STRING("\n\n");
    emit_struct_pre_decls(); EMIT_STRING("\n\n");
    emit_proc_types();       EMIT_STRING("\n\n");
    emit_structs();          EMIT_STRING("\n\n");
    emit_proc_pre_decls();   EMIT_STRING("\n\n");
    emit_vars();             EMIT_STRING("\n\n");
    emit_procs();

    fflush(output_file);

    verb_message("C generation took %lu us\n", measure_time_now_us() - start_us);

    if (options.c_source) { return; }

    strcpy(exe_name, options.output_name);
    if (exe_name[strlen(exe_name) - 1] == 'c'
    &&  exe_name[strlen(exe_name) - 2] == '.') {
        exe_name[strlen(exe_name) - 2] = 0;
    }

/*     sprintf(cmd_buff, "cat %s && cc -o %s %s -std=c99 -O0 -g -nostdlib -ffreestanding -fno-builtin -Wall -Wextra -Werror -Wno-unused-variable -Wno-unused-parameter -Wl,-e,%s", */
/*             options.output_name, exe_name, options.output_name, get_string(program_entry->full_name)); */
    sprintf(cmd_buff, "cc -o %s %s -std=c99 -O0 -g -nostdlib -ffreestanding -fno-builtin -Wl,-e,%s",
            exe_name, options.output_name, get_string(program_entry->full_name));

    verb_message("C COMPILER COMMAND:\n%s\n", cmd_buff);

#ifdef __APPLE__
    strcat(cmd_buff, " -lSystem");
#endif

    err = system(cmd_buff);

    if (err != 0) {
        report_simple_err("c backend failed");
    }
}
