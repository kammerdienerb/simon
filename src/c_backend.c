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

static ast_proc_t *current_proc;
static u32         which_varg;
static array_t     defer_stack;
static u64         defer_label_counter;
static array_t     defer_label_stack;
static array_t     loop_label_stack;

enum {
    BLOCK_NORMAL,
    BLOCK_EXPR,
    BLOCK_PROC_BODY,
    BLOCK_LOOP,
    BLOCK_SYNTHETIC,
};

#define PUSH_DEFER_SCOPE(bk)                               \
do {                                                       \
    array_t defers = array_make(ast_t*);                   \
    array_push(defer_stack, defers);                       \
    array_push(defer_label_stack, defer_label_counter);    \
    if ((bk) == BLOCK_LOOP) {                              \
        array_push(loop_label_stack, defer_label_counter); \
    }                                                      \
    defer_label_counter += 1;                              \
} while (0)

#define POP_DEFER_SCOPE(bk)                                \
do {                                                       \
    array_t *old_defers = array_last(defer_stack);         \
    if (old_defers == NULL) { break; }                     \
    array_free(*old_defers);                               \
    array_pop(defer_stack);                                \
    array_pop(defer_label_stack);                          \
    if ((bk) == BLOCK_LOOP) {                              \
        array_pop(loop_label_stack);                       \
    }                                                      \
} while (0)

#define PUSH_DEFER(_defer)                                 \
do {                                                       \
    array_t *top = array_last(defer_stack);                \
    if (top == NULL) { break; }                            \
    array_push(*top, (_defer));                            \
} while (0)


#define DEFER_STACK_LEN()    (array_len(defer_stack))
#define HAVE_DEFERS()        (array_len(defer_stack) && array_last(*(array_t*)array_last(defer_stack)))
#define TOP_DEFER_LABEL()    (*(u64*)array_last(defer_label_stack))
#define TOP_LOOP_LABEL()     (*(u64*)array_last(loop_label_stack))
#define BOTTOM_DEFER_LABEL() (*(u64*)array_item(defer_label_stack, 0))
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
"typedef struct { void *data; s64 len; } __si_slice;\n"
"\n"
"#define __si_slice_data(_s)         ((_s).data)\n"
"#define __si_slice_len(_s)          ((_s).len)\n"
"#define __si_slice_idx(_s, _i, _t)  (((_t*)__si_slice_data(_s)) + (_i))\n"
"#define _builtin_slice_from(_p, _l) ((__si_slice){ (_p), (_l) })\n"
"\n"
"#define _builtin_stack_alloc(_n) (__builtin_alloca((_n)))\n"
"#ifdef __x86_64__\n"
"#define _builtin_stack_pointer() ({ \\\n"
"    register unsigned long esp __asm(\"esp\"); \\\n"
"    __asm(\"\" : \"=r\"(esp)); \\\n"
"    (void*)esp; \\\n"
"})\n"
"#else\n"
"#define _builtin_stack_pointer() ({ \\\n"
"    register unsigned long esp __asm(\"sp\"); \\\n"
"    __asm(\"\" : \"=r\"(esp)); \\\n"
"    (void*)esp; \\\n"
"})\n"
"#endif\n"
"#define _builtin_base_pointer() (__builtin_frame_address(0))\n"
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

static void _emit_name(ast_t *node, i32 mono_idx, int asm_name) {
    ast_decl_t          *decl;
    scope_t             *scope;
    ast_t               *parent_node;
    int                  parent_mono_idx;
    ast_decl_t          *parent_decl;
    monomorphed_t       *mono;
    ast_poly_constant_t *it;
    const char          *lazy_comma;
    ast_param_t         *param;
    ast_builtin_t       *builtin;

    if (node->kind == AST_DECL_VAR) {
        decl = (ast_decl_t*)node;
        if (!(ASTP(decl)->flags & AST_FLAG_IS_EXTERN) && !decl->containing_scope->in_proc) { goto long_name; }

        if (decl->containing_scope->in_macro && !(node->flags & AST_FLAG_MACRO_PUBLIC)) {
            EMIT_STRING_F("__si_macro_%"PRIu64"_", decl->containing_scope->macro_scope_id);
        }

        EMIT_STRING_ID(decl->name);
    } else if (ast_kind_is_decl(node->kind)) {
long_name:;
        decl = (ast_decl_t*)node;

        if (decl->containing_scope->in_macro && !(node->flags & AST_FLAG_MACRO_PUBLIC)) {
            EMIT_STRING_F("__si_macro_%"PRIu64"_", decl->containing_scope->macro_scope_id);
        }

        scope       = decl->containing_scope;
        parent_node = NULL;

        if (scope         != NULL
        &&  scope->parent != NULL
        &&  scope->kind   != AST_GLOBAL_SCOPE
        &&  !scope->in_macro) {

            parent_node     = scope->node;
            parent_mono_idx = -1;

            switch (parent_node->kind) {
                case AST_PROC:
                    parent_decl     = ((ast_proc_t*)parent_node)->parent_decl;
                    parent_mono_idx = ((ast_proc_t*)parent_node)->mono_idx;
                    break;
                case AST_STRUCT:
                    parent_decl     = ((ast_struct_t*)parent_node)->parent_decl;
                    parent_mono_idx = ((ast_struct_t*)parent_node)->mono_idx;
                    break;
                case AST_MODULE:
                    parent_decl = ((ast_module_t*)parent_node)->parent_decl;
                    break;
                default:
                    parent_decl = NULL;
                    ASSERT(0, "unhandled scope node kind");
                    break;
            }

            _emit_name(ASTP(parent_decl), parent_mono_idx, asm_name);
            EMIT_STRING(asm_name ? "." : "__");
        }

        EMIT_STRING_ID(decl->name);
        if (mono_idx >= 0) {
            if (asm_name) {
                switch (node->kind) {
                    case AST_DECL_PROC:
                        mono = array_item(((ast_proc_t*)decl->val_expr)->monomorphs, mono_idx);
                        break;
                    case AST_DECL_STRUCT:
                        mono = array_item(((ast_struct_t*)decl->val_expr)->monomorphs, mono_idx);
                        break;
                    default:
                        mono = NULL;
                        ASSERT(0, "bad decl kind with mono idx");
                        break;
                }

                EMIT_C('(');
                lazy_comma = "";
                array_traverse(mono->constants, it) {
                    EMIT_STRING_F("%s%s: %s", lazy_comma, get_string(it->name), get_string(value_to_string_id(ASTP(it)->value, ASTP(it)->type)));
                    lazy_comma = ", ";

                }
                EMIT_C(')');
            } else {
                EMIT_STRING_F("__p%d", mono_idx);
            }
        }
    } else if (node->kind == AST_PARAM) {
        param = (ast_param_t*)node;
        EMIT_STRING_ID(param->name);
    } else if (node->kind == AST_BUILTIN) {
        builtin = (ast_builtin_t*)node;
        EMIT_STRING_ID(builtin->name);
    }
}

static void emit_asm_name(ast_t *node, i32 mono_idx) {
    _emit_name(node, mono_idx, 1);
}
static void emit_name(ast_t *node, i32 mono_idx) {
    _emit_name(node, mono_idx, 0);
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
        case TY_ARRAY:
            EMIT_STRING_F("__array_type_%u", ty);
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

static void emit_struct_pre_decl(ast_decl_t *decl, ast_struct_t *st, i32 mono_idx) {
    if (st->bitfield_struct_bits != 0) {
        EMIT_STRING_F("typedef u%d ", st->bitfield_struct_bits);
        emit_name(ASTP(decl), mono_idx);
        EMIT_STRING(";\n");
    } else {
        EMIT_STRING("struct ");
        emit_name(ASTP(decl), mono_idx);
        EMIT_STRING("_struct;\n");
        EMIT_STRING("typedef struct ");
        emit_name(ASTP(decl), mono_idx);
        EMIT_STRING("_struct ");
        emit_name(ASTP(decl), mono_idx);
        EMIT_STRING(";\n");
    }
}

static void emit_struct_pre_decls(void) {
    ast_decl_t    **it;
    ast_decl_t     *decl;
    ast_struct_t   *st;
    i32             mono_idx;
    monomorphed_t  *mit;

    array_traverse(all_types, it) {
        decl = *it;
        st   = (ast_struct_t*)decl->val_expr;

        if (ASTP(decl)->flags & AST_FLAG_POLYMORPH) {
            mono_idx = 0;
            array_traverse(st->monomorphs, mit) {
                if (mit->specialization == NULL) {
                    emit_struct_pre_decl(decl, st, mono_idx);
                }
                mono_idx += 1;
            }
        } else {
            emit_struct_pre_decl(decl, st, -1);
        }
    }
}

static void emit_basic_array_types(void) {
    u32     ty;
    type_t *it;

    ty = 0;
    array_traverse(type_table, it) {
        if (ty != TY_ARRAY && it->kind == TY_ARRAY && !type_is_poly(ty)) {
            if (type_kind(it->under_id) != TY_STRUCT && type_kind(it->under_id) != TY_STRUCT_MONO) {
                EMIT_STRING("typedef ");
                emit_type(it->under_id);
                EMIT_STRING_F(" __array_type_%u[%u];\n", ty, it->array_length);
            }
        }
        ty += 1;
    }
}

static void emit_struct_array_types(u32 struct_ty) {
    u32     ty;
    type_t *it;

    ty = 0;
    array_traverse(type_table, it) {
        if (ty != TY_ARRAY && it->kind == TY_ARRAY && !type_is_poly(ty)) {
            if (it->under_id == struct_ty) {
                EMIT_STRING("typedef ");
                emit_type(it->under_id);
                EMIT_STRING_F(" __array_type_%u[%u];\n", ty, it->array_length);
            }
        }
        ty += 1;
    }
}

static void emit_struct(ast_decl_t *decl, ast_struct_t *st, i32 mono_idx) {
    ast_t        **fit;
    ast_decl_t    *field;
    u32            ty;
    ast_decl_t    *field_type_decl;
    ast_struct_t  *field_type_st;

    ASSERT(!(ASTP(st)->flags & AST_FLAG_CHECKED), "cycle in struct dep graph!");

    ASTP(st)->flags |= AST_FLAG_CHECKED;

    if (st->bitfield_struct_bits != 0) { goto out; }

    array_traverse(st->fields, fit) {
        field = (ast_decl_t*)*fit;
        ty    = ASTP(field)->type;

again:;
        switch (type_kind(ty)) {
            case TY_ARRAY:
                ty = get_under_type(ty);
                goto again;
            case TY_STRUCT:
            case TY_STRUCT_MONO:
                field_type_decl = struct_type_to_decl(ty);
                ASSERT(field_type_decl->val_expr->kind == AST_STRUCT, "not a struct?");
                field_type_st = (ast_struct_t*)field_type_decl->val_expr;
                if (!(ASTP(field_type_st)->flags & AST_FLAG_VISIT_WORK_DONE)) {
                    emit_struct(field_type_decl, field_type_st, field_type_st->mono_idx);
                }
                break;
        }
    }

    EMIT_STRING("struct ");
    emit_name(ASTP(decl), mono_idx);
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

out:;

    emit_struct_array_types(ASTP(st)->value.t);

    ASTP(st)->flags |= AST_FLAG_VISIT_WORK_DONE;
}

static void emit_structs(void) {
    ast_decl_t    **it;
    ast_decl_t     *decl;
    ast_struct_t   *st;
    i32             mono_idx;
    monomorphed_t  *mit;

    /* Clear all visited flags. We're going to do a dependency-order traversal. */
    array_traverse(all_types, it) {
        decl = *it;
        st   = (ast_struct_t*)decl->val_expr;

        if (ASTP(decl)->flags & AST_FLAG_POLYMORPH) {
            mono_idx = 0;
            array_traverse(st->monomorphs, mit) {
                if (mit->specialization == NULL) {
                    mit->node->flags &= ~(AST_FLAG_CHECKED | AST_FLAG_VISIT_WORK_DONE);
                }
                mono_idx += 1;
            }
        } else {
            ASTP(st)->flags &= ~(AST_FLAG_CHECKED | AST_FLAG_VISIT_WORK_DONE);
        }
    }

    array_traverse(all_types, it) {
        decl = *it;
        st   = (ast_struct_t*)decl->val_expr;

        if (ASTP(decl)->flags & AST_FLAG_POLYMORPH) {
            mono_idx = 0;
            array_traverse(st->monomorphs, mit) {
                if (mit->specialization == NULL) {
                    if (!(mit->node->flags & AST_FLAG_CHECKED)) {
                        emit_struct(decl, (ast_struct_t*)mit->node, mono_idx);
                    }
                }
                mono_idx += 1;
            }
        } else {
            if (!(ASTP(st)->flags & AST_FLAG_CHECKED)) {
                emit_struct(decl, st, -1);
            }
        }
    }
}

static void emit_proc_types(void) {
    u32         ty;
    type_t     *it;
    const char *lazy_comma;
    u32         n;
    u32         i;
    u32         param_ty;

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
                    param_ty = get_param_type(ty, i);
                    if (param_ty == TY_TYPE || param_ty == TY_MODULE) {
                        continue;
                    }

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
        EMIT_STRING(lazy_comma);
        emit_type(ASTP(param)->type);
        EMIT_C(' ');
        EMIT_STRING_ID(param->name);
    }
}

static void emit_proc_pre_decl(ast_decl_t *decl, ast_proc_t *proc, i32 mono_idx) {
    const char   *lazy_comma;
    ast_t       **pit;
    ast_param_t  *param;

    current_proc = proc;

    if (proc->ret_type_expr != NULL) {
        emit_type(proc->ret_type_expr->value.t);
        EMIT_C(' ');
    } else {
        EMIT_STRING("void ");
    }

    emit_name(ASTP(decl), mono_idx);

    if (array_len(proc->params) == 0) {
        EMIT_STRING("(void)");
    } else {
        EMIT_C('(');
        lazy_comma = "";
        array_traverse(proc->params, pit) {
            param = (ast_param_t*)*pit;

            if (ASTP(param)->flags & AST_FLAG_POLYMORPH) { continue; }

            emit_param(param, lazy_comma);
            lazy_comma = ", ";
        }
        EMIT_C(')');
    }

    if (!(ASTP(proc)->flags & AST_FLAG_IS_EXTERN)) {
        EMIT_STRING(" __asm__ (\"");
        emit_asm_name(ASTP(decl), proc->mono_idx);
        EMIT_STRING("\")");
    }

    EMIT_STRING(";\n");
}

static void emit_proc_pre_decls(void) {
    ast_decl_t    **it;
    ast_decl_t     *decl;
    ast_proc_t     *proc;
    i32             mono_idx;
    monomorphed_t  *mit;

    array_traverse(all_procs, it) {
        decl = *it;
        proc = (ast_proc_t*)decl->val_expr;

        if (ASTP(proc)->flags & AST_FLAG_POLYMORPH) {
            mono_idx = 0;
            array_traverse(proc->monomorphs, mit) {
                if (mit->specialization == NULL) {
                    emit_proc_pre_decl(decl, (ast_proc_t*)mit->node, mono_idx);
                }
                mono_idx += 1;
            }
        } else {
            emit_proc_pre_decl(decl, proc, -1);
        }
    }
}

static void emit_expr(ast_t *expr, int lvl);

static void emit_sizeof_assertions(void) {
    u64 n_types;
    u32 ty;
    u32 tk;

    n_types = array_len(type_table);

    for (ty = 0; ty < n_types; ty += 1) {
        tk = type_kind(ty);
        if (ty != TY_STRUCT
        &&  ty != TY_STRUCT_MONO
        &&  (tk == TY_STRUCT || tk == TY_STRUCT_MONO)
        &&  !type_is_poly(ty)) {

            EMIT_STRING("_Static_assert(sizeof(");
            emit_name(ASTP(struct_type_to_decl(ty)), -1);
            EMIT_STRING_F(") == %" PRIi64 ", \"incorrect sizeof\");\n", type_size(ty, NULL));
        }
        ty += 1;
    }

    EMIT_STRING("\n");
}

static void emit_vars(void) {
    ast_decl_t **it;
    ast_decl_t  *decl;

    array_traverse(all_vars, it) {
        decl = *it;

        if (ASTP(decl)->flags & AST_FLAG_CONSTANT) {
            continue;
        }

        if (ASTP(decl)->flags & AST_FLAG_IS_EXTERN) {
            EMIT_STRING("extern ");
        }

        emit_type(ASTP(decl)->type);
        EMIT_C(' ');
        emit_name(ASTP(decl), -1);
        EMIT_C(';');
        EMIT_C('\n');
    }
}

static void emit_unary_expr(ast_t *expr, int lvl) {
    int    op;
    ast_t *child;

    op    = ((ast_unary_expr_t*)expr)->op;
    child = ((ast_unary_expr_t*)expr)->child;
    (void)child;

    switch (op) {
        case OP_NEG:
            EMIT_STRING(OP_STR(op));
            emit_expr(child, lvl);
            break;
        case OP_NOT:
            EMIT_STRING("!(");
            emit_expr(child, lvl);
            EMIT_C(')');
            break;
        case OP_ADDR:
            EMIT_STRING("&(");
            emit_expr(child, lvl);
            EMIT_C(')');
            break;
        case OP_DEREF:
            EMIT_STRING("(*(");
            emit_expr(child, lvl);
            EMIT_STRING("))");
            break;
        case OP_SIZEOF:
            EMIT_STRING("sizeof (");
            emit_expr(child, lvl);
            EMIT_C(')');
            break;
        case OP_TYPEOF:
            emit_type(expr->value.t);
            break;
        case OP_LENOF:
            if (type_kind(child->type) == TY_ARRAY) {
                EMIT_STRING_F("%"PRIi64, (i64)get_array_length(child->type));
            } else if (type_kind(child->type) == TY_SLICE) {
            	EMIT_C('(');
                emit_expr(child, lvl);
            	EMIT_STRING(").len");
            }
            break;
        default:
            report_simple_err_no_exit("INTERNAL ERROR: unhandled expression in emit_unary_expr(): %s", OP_STR(op));
            ASSERT(0, "don't know how to emit this expression");
    }
}

static void emit_binary_expr(ast_t *expr, int lvl) {
    int              op;
    ast_t           *l;
    ast_t           *r;
    ast_t           *call_decl;
    ast_arg_list_t  *arg_list;
    arg_t           *arg;
    u32              i;
    arg_t           *it;
    const char      *lazy_comma = "";
    u32              st_ty;
    ast_decl_t      *st_decl;
    ast_struct_t    *st;
    ast_t          **field_it;
    ast_decl_t      *field;

    op = ((ast_bin_expr_t*)expr)->op;
    l  = ((ast_bin_expr_t*)expr)->left;
    r  = ((ast_bin_expr_t*)expr)->right;

    (void)l;
    (void)r;

    EMIT_C('(');

    switch (op) {
        case OP_CALL:
            call_decl = ((ast_bin_expr_t*)expr)->call_decl;
            if (call_decl != NULL && call_decl->kind == AST_BUILTIN) {
                if (((ast_builtin_t*)call_decl)->name == CAST_ID) {
                    EMIT_STRING("((");
                    arg_list = (ast_arg_list_t*)r;
                    arg      = array_item(arg_list->args, 0);
                    emit_type(arg->expr->value.t);
                    EMIT_STRING(")(");
                    arg = array_item(arg_list->args, 1);
                    emit_expr(arg->expr, lvl);
                    EMIT_STRING("))");
                } else if (((ast_builtin_t*)call_decl)->name == _BUILTIN_VARG_ID) {
                    EMIT_STRING_F("__varg%d", which_varg);
                } else {
                    goto normal_call;
                }
            } else {
normal_call:;
                emit_expr(l, lvl);
                EMIT_C('(');
                i = 0;
                array_traverse(((ast_arg_list_t*)r)->args, it) {
                    if (it->expr->flags & AST_FLAG_POLYMORPH) { continue; }

                    EMIT_STRING(lazy_comma);
                    emit_expr(it->expr, lvl);
                    lazy_comma = ", ";
                    i += 1;
                }
                EMIT_C(')');
            }
            break;
        case OP_SUBSCRIPT:
            if (type_kind(l->type) == TY_ARRAY) {
                emit_expr(l, lvl);
                EMIT_C('[');
                emit_expr(r, lvl);
                EMIT_C(']');
            } else if (type_kind(l->type) == TY_SLICE) {
                EMIT_STRING("(*__si_slice_idx(");
                emit_expr(l, lvl);
                EMIT_STRING(", ");
                emit_expr(r, lvl);
                EMIT_STRING(", ");
                emit_type(get_under_type(l->type));
                EMIT_STRING("))");
            } else {
                ASSERT(0, "bad type to emit subscript");
            }
            break;
        case OP_DOT:
            if (expr->flags & AST_FLAG_BITFIELD_DOT) {
                EMIT_C('(');
                EMIT_C('(');
                emit_type(expr->type);
                EMIT_C(')');
                EMIT_C('(');
                EMIT_C('(');
                if (type_kind(l->type) == TY_PTR) {
                    EMIT_C('(');
                    EMIT_C('*');
                    emit_expr(l, lvl);
                    EMIT_C(')');
                } else {
                    emit_expr(l, lvl);
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
                EMIT_C(')');
            } else {
                emit_expr(l, lvl);
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

                emit_expr(((ast_bin_expr_t*)l)->left, lvl);
                EMIT_STRING(" = (");
                emit_expr(((ast_bin_expr_t*)l)->left, lvl);
                EMIT_STRING_F(" & ~0x%"PRIx64") | (", field->bitfield_mask);

                if (op == OP_ASSIGN) {
                    EMIT_STRING("(((u64)");
                    emit_expr(r, lvl);
                    EMIT_C(')');
                    EMIT_STRING_F(" << %dULL) & 0x%"PRIx64")", field->bitfield_shift, field->bitfield_mask);
                } else {
                    emit_expr(((ast_bin_expr_t*)l)->left, lvl);
                    EMIT_C('(');
                    EMIT_C('(');
                    emit_expr(l, lvl);
                    EMIT_C(' ');
                    for (i = 0; i < strlen(OP_STR(op)) - 1; i += 1) {
                        EMIT_C(OP_STR(op)[i]);
                    }
                    EMIT_STRING_F(" ((u64)");
                    emit_expr(r, lvl);
                    EMIT_C(')');
                    EMIT_C(')');
                    EMIT_STRING_F(" << %dULL) & 0x%"PRIx64")", field->bitfield_shift, field->bitfield_mask);
                }
            } else {
                EMIT_C('(');
                emit_expr(l, lvl);
                EMIT_STRING_F(" %s ", OP_STR(op));
                emit_expr(r, lvl);
                EMIT_C(')');
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
            emit_expr(l, lvl);
            EMIT_STRING_F(" %s ", OP_STR(op));
            emit_expr(r, lvl);
            break;
        case OP_AND:
            emit_expr(l, lvl);
            EMIT_STRING(" && ");
            emit_expr(r, lvl);
            break;
        case OP_OR:
            emit_expr(l, lvl);
            EMIT_STRING(" || ");
            emit_expr(r, lvl);
            break;
        default:
            report_simple_err_no_exit("INTERNAL ERROR: unhandled expression in emit_binary_expr(): %s", OP_STR(op));
            ASSERT(0, "don't know how to emit this expression");
    }

    EMIT_C(')');
}

enum {
    FMT_FLAGS_NONE = 0,
    FMT_NO_INDENT  = 1 << 0,
    FMT_NO_END     = 1 << 1,
};

static void emit_stmt(ast_t *stmt, int lvl, int fmt_flags, int block_kind);

static void emit_expr(ast_t *expr, int lvl) {
    ast_ident_t   *ident;
    ast_t         *resolved_node;
    ast_decl_t    *decl;
    ast_proc_t    *proc;
    monomorphed_t *spec_monomorphed;

    if (expr->flags & AST_FLAG_CONSTANT) {
        switch (expr->type) {
        #define X(ty) case ty:
            X_INT_TYPES
        #undef X
                if (INT_TYPE_IS_SIGNED(expr->type)) {
                    EMIT_STRING_F("%"PRIi64, expr->value.i);
                } else {
                    EMIT_STRING_F("%"PRIu64"ULL", expr->value.u);
                }
                return;
            case TY_F32:
            case TY_F64:
                EMIT_STRING_F("%f", expr->value.f);
                return;
        }
    }

    switch (expr->kind) {
        case AST_UNARY_EXPR:
            emit_unary_expr(expr, lvl);
            break;
        case AST_BIN_EXPR:
            emit_binary_expr(expr, lvl);
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

                emit_expr(((ast_decl_t*)resolved_node)->val_expr, lvl);
            } else if (resolved_node->kind == AST_DECL_PROC
                   && resolved_node->flags & AST_FLAG_POLYMORPH
                   &&  ident->mono_idx >= 0) {

                decl             = (ast_decl_t*)resolved_node;
                proc             = (ast_proc_t*)decl->val_expr;
                spec_monomorphed = array_item(proc->monomorphs, ident->mono_idx);

                if (spec_monomorphed->specialization == NULL) { goto normal_name; }

                emit_name(spec_monomorphed->specialization, -1);

            } else {
normal_name:;
                emit_name(((ast_ident_t*)expr)->resolved_node, ((ast_ident_t*)expr)->mono_idx);
            }

            break;
        case AST_BLOCK:
            emit_stmt(expr, lvl, FMT_NO_END | FMT_NO_INDENT, BLOCK_EXPR);
            break;
        default:
            ASSERT(0, "not an expression");
    }
}

static void emit_defers(int lvl) {
    array_t  *scope;
    ast_t   **stmt_it;

    scope = array_last(defer_stack);
    if (scope == NULL) { return; }

    if (array_len(*scope) == 0) { return; }

    INDENT(lvl); EMIT_STRING_F("{ /* DEFER SCOPE %"PRIu64" */\n", TOP_DEFER_LABEL());
    array_rtraverse(*scope, stmt_it) {
        emit_stmt(*stmt_it, lvl + 1, FMT_FLAGS_NONE, BLOCK_NORMAL);
    }
    INDENT(lvl); EMIT_STRING("}\n");
}

static void emit_all_defers_except_return(int lvl) {
    array_t  *scope;
    ast_t   **stmt_it;

    array_rtraverse(defer_stack, scope) {
        if (scope == array_item(defer_stack, 0)) { break; }

        if (array_len(*scope) == 0) { continue; }

        INDENT(lvl); EMIT_STRING_F("{ /* DEFER SCOPE %"PRIu64" */\n", TOP_DEFER_LABEL());
        array_rtraverse(*scope, stmt_it) {
            emit_stmt(*stmt_it, lvl + 1, FMT_FLAGS_NONE, BLOCK_NORMAL);
        }
        INDENT(lvl); EMIT_STRING("}\n");
    }
}

static void emit_all_defers_for_loop(int lvl) {
    int       i;
    array_t  *scope;
    int       stop;
    ast_t   **stmt_it;

    i = array_len(defer_stack) - 1;
    array_rtraverse(defer_stack, scope) {
        stop = *(u64*)array_item(defer_label_stack, i) == TOP_LOOP_LABEL();

        if (array_len(*scope) == 0) { goto next; }

        INDENT(lvl); EMIT_STRING_F("{ /* DEFER SCOPE %"PRIu64" */\n", TOP_DEFER_LABEL());
        array_rtraverse(*scope, stmt_it) {
            emit_stmt(*stmt_it, lvl + 1, FMT_FLAGS_NONE, BLOCK_NORMAL);
        }
        INDENT(lvl); EMIT_STRING("}\n");

next:;
        if (stop) { break; }

        i -= 1;
    }
}

static void emit_line(src_point_t *pt) {
    EMIT_STRING_F("#line %d \"%s\"\n", pt->line, get_string(pt->path_id));
}

static void emit_stmt(ast_t *stmt, int lvl, int fmt_flags, int block_kind) {
    ast_t **it;
    ast_t  *s;

    if (stmt->kind == AST_DECL_VAR && stmt->flags & AST_FLAG_CONSTANT) {
        return;
    }

    if (!(fmt_flags & FMT_NO_INDENT) && stmt->kind != AST_VARGS_BLOCK) {
        emit_line(&stmt->loc.beg);
        INDENT(lvl);
    }

    switch (stmt->kind) {
        case AST_BLOCK:
            if (stmt->flags & AST_FLAG_SYNTHETIC_BLOCK) {
                ASSERT(block_kind != BLOCK_PROC_BODY, "proc synthetic block???");
                ASSERT(block_kind != BLOCK_LOOP,      "loop synthetic block???");
                block_kind = BLOCK_SYNTHETIC;
            }

            if (block_kind != BLOCK_SYNTHETIC) {
                if (block_kind == BLOCK_EXPR) {
                    EMIT_STRING("(");
                }
                EMIT_STRING("{\n");
            }
            if (block_kind != BLOCK_SYNTHETIC) {
                PUSH_DEFER_SCOPE(block_kind);
            }

            if (block_kind == BLOCK_PROC_BODY && current_proc->ret_type_expr != NULL) {
                INDENT(lvl + 1);
                emit_type(current_proc->ret_type_expr->value.t);
                EMIT_STRING(" __si_ret;\n\n");
            } else if (block_kind == BLOCK_EXPR && stmt->type != TY_NOT_TYPED) {
                INDENT(lvl + 1);
                emit_type(stmt->type);
                EMIT_STRING(" __si_block_val;\n\n");
            }

            array_traverse(((ast_block_t*)stmt)->stmts, it) {
                s = *it;
                if (s->kind == AST_DEFER) {
                    PUSH_DEFER(((ast_defer_t*)s)->block);
                }
            }
            array_traverse(((ast_block_t*)stmt)->stmts, it) {
                s = *it;
                if (s->kind != AST_DEFER) {
                    if (block_kind == BLOCK_EXPR && it == array_last(((ast_block_t*)stmt)->stmts)) {
                        INDENT(lvl + 1);
                        if (ast_kind_is_decl(s->kind)) {
                            emit_stmt(s, lvl + 1, FMT_NO_INDENT, BLOCK_NORMAL);
                            INDENT(lvl + 1);
                            EMIT_STRING("__si_block_val = ");
                            emit_name(s, -1);
                            EMIT_STRING(";\n");
                        } else {
                            EMIT_STRING("__si_block_val = ");
                            emit_stmt(s, lvl + 1, FMT_NO_INDENT, BLOCK_NORMAL);
                        }
                    } else {
                        emit_stmt(s, lvl + 1, FMT_FLAGS_NONE, BLOCK_NORMAL);
                    }
                }
            }

            if (block_kind != BLOCK_SYNTHETIC) {
                if (block_kind == BLOCK_PROC_BODY || HAVE_DEFERS()) {
                    emit_line(&stmt->loc.end);
                    EMIT_STRING_F("\n__si_scope_%"PRIu64"_exit:;\n", TOP_DEFER_LABEL());
                    emit_defers(lvl + 1);
                }
                POP_DEFER_SCOPE(block_kind);
            }

            if (block_kind == BLOCK_PROC_BODY && current_proc->ret_type_expr != NULL) {
                INDENT(lvl + 1);
                emit_line(&ASTP(current_proc)->loc.end);
                EMIT_STRING("return __si_ret;\n");
            } else if (block_kind == BLOCK_EXPR) {
                INDENT(lvl + 1);
                EMIT_STRING("__si_block_val;\n");
            }

            emit_line(&stmt->loc.end);
            INDENT(lvl);
            if (block_kind != BLOCK_SYNTHETIC) {
                EMIT_STRING("}");
                if (block_kind == BLOCK_EXPR) {
                    EMIT_STRING(")");
                }
            }
            if (!(fmt_flags & FMT_NO_END)) {
                EMIT_STRING("\n\n");
            }
            break;

        case AST_VARGS_BLOCK:
            which_varg = 0;
            array_traverse(((ast_vargs_block_t*)stmt)->new_blocks, it) {
                emit_stmt(*it, lvl, FMT_FLAGS_NONE, BLOCK_SYNTHETIC);
                which_varg += 1;
            }
            break;

        case AST_DECL_VAR:
            emit_type(stmt->type);
            EMIT_C(' ');
            emit_name(stmt, -1);
/*             EMIT_STRING_ID(((ast_decl_t*)stmt)->name); */

            if (((ast_decl_t*)stmt)->val_expr) {
                EMIT_STRING(" = ");
                emit_expr(((ast_decl_t*)stmt)->val_expr, lvl);
            }

            if (!(fmt_flags & FMT_NO_END)) {
                EMIT_STRING(";\n");
            }
            break;

        case AST_IF:
            EMIT_STRING("if (");
            emit_expr(((ast_if_t*)stmt)->expr, lvl);
            EMIT_STRING(") ");
            emit_stmt(((ast_if_t*)stmt)->then_block, lvl, FMT_NO_INDENT | FMT_NO_END, BLOCK_NORMAL);
            if (((ast_if_t*)stmt)->els != NULL) {
                EMIT_STRING(" else ");
                emit_stmt(((ast_if_t*)stmt)->els, lvl, FMT_NO_INDENT, BLOCK_NORMAL);
            } else {
                EMIT_STRING("\n\n");
            }
            break;

        case AST_LOOP:
            EMIT_STRING("for (");

            if (((ast_loop_t*)stmt)->init != NULL) {
                emit_stmt(((ast_loop_t*)stmt)->init, lvl, FMT_NO_INDENT | FMT_NO_END, BLOCK_NORMAL);
            }
            EMIT_STRING("; ");

            if (((ast_loop_t*)stmt)->cond != NULL) {
                emit_stmt(((ast_loop_t*)stmt)->cond, lvl + 1, FMT_NO_INDENT | FMT_NO_END, BLOCK_NORMAL);
            }
            EMIT_STRING("; ");

            if (((ast_loop_t*)stmt)->post != NULL) {
                emit_stmt(((ast_loop_t*)stmt)->post, lvl, FMT_NO_INDENT | FMT_NO_END, BLOCK_NORMAL);
            }
            EMIT_STRING(")");

            emit_stmt(((ast_loop_t*)stmt)->block, lvl, FMT_NO_INDENT, BLOCK_LOOP);

            break;

        case AST_RETURN:
            if (((ast_return_t*)stmt)->expr != NULL) {
                EMIT_STRING("__si_ret = ");
                emit_expr(((ast_return_t*)stmt)->expr, lvl);
                EMIT_STRING(";\n");
            }
            emit_all_defers_except_return(lvl);
            emit_line(&stmt->loc.beg);
            INDENT(lvl);
            EMIT_STRING_F("goto __si_scope_%"PRIu64"_exit;\n", BOTTOM_DEFER_LABEL());
            break;

        case AST_DEFER:
            /* Done at the start when we emit a block. */
            break;

        case AST_BREAK:
            emit_all_defers_for_loop(lvl + 1);
            EMIT_STRING("break;\n");
            break;

        case AST_CONTINUE:
            emit_all_defers_for_loop(lvl + 1);
            EMIT_STRING("continue;\n");
            break;

#define X(kind) case kind:
        X_AST_ALL_EXPRS_BUT_BLOCK
#undef X
            emit_expr(stmt, lvl);
            if (!(fmt_flags & FMT_NO_END)) {
                EMIT_STRING(";\n");
            }
            break;

        default:
            report_simple_err_no_exit("INTERNAL ERROR: unhandled statement in emit_stmt(): %s", ast_get_kind_str(stmt->kind));
            ASSERT(0, "don't know how to emit this statement");
    }
}

static void emit_proc(ast_decl_t *decl, ast_proc_t *proc, i32 mono_idx) {
    const char   *lazy_comma;
    ast_t       **pit;
    ast_param_t  *param;

    current_proc = proc;

    EMIT_STRING_F("#line %d \"%s\"\n", ASTP(proc)->loc.beg.line, get_string(ASTP(proc)->loc.beg.path_id));

    if (proc->ret_type_expr != NULL) {
        emit_type(proc->ret_type_expr->value.t);
        EMIT_C(' ');
    } else {
        EMIT_STRING("void ");
    }

    emit_name(ASTP(decl), mono_idx);

    if (array_len(proc->params) == 0) {
        EMIT_STRING("(void) ");
    } else {
        EMIT_C('(');
        lazy_comma = "";
        array_traverse(proc->params, pit) {
            param = (ast_param_t*)*pit;

            if (ASTP(param)->flags & AST_FLAG_POLYMORPH) { continue; }

            emit_param(param, lazy_comma);
            lazy_comma = ", ";
        }
        EMIT_C(')');
        EMIT_C(' ');
    }

    emit_stmt(proc->block, 0, FMT_NO_INDENT, BLOCK_PROC_BODY);
}

static void emit_procs(void) {
    ast_decl_t    **it;
    ast_decl_t     *decl;
    ast_proc_t     *proc;
    i32             mono_idx;
    monomorphed_t  *mit;

    array_traverse(all_procs, it) {
        decl = *it;
        proc = (ast_proc_t*)decl->val_expr;

        if (ASTP(proc)->flags & AST_FLAG_IS_EXTERN) {
            continue;
        }

        if (ASTP(proc)->flags & AST_FLAG_POLYMORPH) {
            mono_idx = 0;
            array_traverse(proc->monomorphs, mit) {
                if (mit->specialization == NULL) {
                    emit_proc(decl, (ast_proc_t*)mit->node, mono_idx);
                }
                mono_idx += 1;
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

    defer_stack       = array_make(array_t);
    defer_label_stack = array_make(u64);
    loop_label_stack  = array_make(u64);

    emit_prelude();           EMIT_STRING("\n\n");
    emit_struct_pre_decls();  EMIT_STRING("\n\n");
    emit_basic_array_types(); EMIT_STRING("\n\n");
    emit_proc_types();        EMIT_STRING("\n\n");
    emit_structs();           EMIT_STRING("\n\n");
    emit_sizeof_assertions(); EMIT_STRING("\n\n");
    emit_proc_pre_decls();    EMIT_STRING("\n\n");
    emit_vars();              EMIT_STRING("\n\n");
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
    sprintf(cmd_buff, "clang -o %s %s -std=c99 -O0 -g -nostdlib -ffreestanding -fno-builtin -fno-stack-protector -Wno-parentheses-equality %s -Wl,-e,%s",
            exe_name, options.output_name, options.with_libc ? "-lc" : "", get_string(program_entry->full_name));

    verb_message("C compiler command:\n  %s\n", cmd_buff);

#ifdef __APPLE__
    strcat(cmd_buff, " -lSystem");
#endif

    err = system(cmd_buff);

    if (err != 0) {
        report_simple_err("c backend failed");
    }
}
