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

static array_t        mod_stack;
static polymorphed_t *polymorphed;
static u32            poly_version;
static ast_proc_t    *proc;
static u32            proc_ty;
static u32            varg_ty;
static u32            which_varg;
static array_t        defer_stack;
static u64            defer_label_counter;
static array_t        defer_label_stack;
static u64            loop_label_counter;
static array_t        loop_label_stack;



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

static void emit_underscore_name(string_id name) {
    const char *s;
    const char *t;

    s = get_string(name);

    for (t = s; *t; t += 1) {
        if (*t == '.') {
            EMIT_STRING("__");
            break;
        }
    }
    for (t = s; *t; t += 1) {
        if (*t == '.') {
             EMIT_C('_');
        } else {
             EMIT_C(*t);
        }
    }
}

/* static void emit_base_name(string_id name) { */
/*     const char *s; */
/*     int         len; */
/*     const char *t; */
/*  */
/*     s = get_string(name); */
/*     len = strlen(s); */
/*  */
/*     if (len <= 0) { return; } */
/*  */
/*     for (t = s + len - 1; t >= s; t -= 1) { */
/*         if (*t == '.') { */
/*             EMIT_STRING(t + 1); */
/*             break; */
/*         } */
/*     } */
/* } */

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
"typedef char*              str;\n"
"\n"
"#define _builtin_stack_alloc(_n) (__builtin_alloca((_n)))\n"
#ifdef __x86_64__
"__attribute__((used, always_inline))\n"
"static inline void _builtin_outb(u8* addr, u8 b) { asm volatile ( \"outb %0, %1\" : : \"a\"(b), \"Nd\"((u16)(u64)addr) );                      }\n"
"__attribute__((used, always_inline))\n"
"static inline u8   _builtin_inb(u8* addr)        { u8 ret; asm volatile ( \"inb %1, %0\" : \"=a\"(ret) : \"Nd\"((u16)(u64)addr) ); return ret; }\n"
#endif
"\n";

    EMIT_STRING(prelude);
}



#define PUSH_DEFER_SCOPE(bk)                              \
do {                                                      \
    array_t defers = array_make(ast_t*);                  \
    array_push(defer_stack, defers);                      \
    array_push(defer_label_stack, defer_label_counter);   \
    if ((bk) == BLOCK_LOOP) {                             \
        array_push(loop_label_stack, loop_label_counter); \
        loop_label_counter += 1;                          \
    }                                                     \
    defer_label_counter += 1;                             \
} while (0)

#define POP_DEFER_SCOPE(bk)                               \
do {                                                      \
    array_t *old_defers = array_last(defer_stack);        \
    if (old_defers == NULL) { break; }                    \
    array_free(*old_defers);                              \
    array_pop(defer_stack);                               \
    array_pop(defer_label_stack);                         \
    if ((bk) == BLOCK_LOOP) {                             \
        array_pop(loop_label_stack);                      \
    }                                                     \
} while (0)

#define PUSH_DEFER(_defer)                                \
do {                                                      \
    array_t *top = array_last(defer_stack);               \
    if (top == NULL) { break; }                           \
    array_push(*top, (_defer));                           \
} while (0)


#define DEFER_STACK_LEN()    (array_len(defer_stack))
#define HAVE_DEFERS()        (array_len(defer_stack) && array_last(*(array_t*)array_last(defer_stack)))
#define TOP_DEFER_LABEL()    (*(u64*)array_last(defer_label_stack))
#define TOP_LOOP_LABEL()     (*(u64*)array_last(loop_label_stack))
#define BOTTOM_DEFER_LABEL() (*(u64*)array_item(defer_label_stack, 0))

static void emit_type_declarator(u32 t) {
    switch (type_kind(t)) {
        case TY_NOT_TYPED:
            EMIT_STRING("void");
            break;
        case TY_GENERIC_INT:
        case TY_GENERIC_FLOAT:
            EMIT_STRING_ID(get_type_string_id(t));
            break;
        case TY_STR:
            EMIT_STRING("str");
            break;
        case TY_PTR:
            emit_type_declarator(get_under_type(t));
            EMIT_STRING("*");
            break;
        case TY_STRUCT:
            emit_underscore_name(get_type_string_id(t));
            break;
        default:
            report_simple_err("type '%s' not handled in emit_type_declarator()", get_string(get_type_string_id(t)));
            return;
    }
}


static void emit_expr(ast_t *expr) {
    ast_ident_t           *ident;
    polymorph_constant_t  *it;
    ast_decl_t            *decl;
    ast_proc_t            *proc;
    polymorphed_t         *spec_polymorphed;
    ast_unary_expr_t      *un_expr;
    int                    op;
    ast_bin_expr_t        *bin_expr;
    u32                    st_ty;
    ast_decl_t            *st_decl;
    ast_struct_t          *st;
    ast_t                **field_it;
    ast_struct_field_t    *field;
    ast_arg_list_t        *arg_list;
    arg_t                 *arg;
    const char            *comma;

    if (expr->flags & AST_FLAG_PAREN_EXPR) { EMIT_C('('); }

    switch(expr->kind) {
        case AST_INT:
            if (expr->flags & AST_FLAG_HEX_INT) {
                EMIT_STRING_F("0x%"PRIx64"LL", expr->value.u);
            } else {
                EMIT_STRING_F("%"PRIi64"LL", expr->value.i);
            }
            break;
        case AST_FLOAT:
            EMIT_STRING_F("%f", expr->value.f);
            break;
        case AST_CHAR:
            EMIT_STRING_ID(((ast_char_t*)expr)->str_rep);
            break;
        case AST_STRING:
            EMIT_STRING_ID(((ast_string_t*)expr)->str_rep);
            break;
        case AST_IDENT:
            ident = (ast_ident_t*)expr;
            if (polymorphed != NULL) {
                array_traverse(polymorphed->constants, it) {
                    if (it->name == ident->str_rep) {
                        ASSERT(it->type == TY_TYPE, "unimplemented");
                        emit_type_declarator(it->value.t);
                        goto renamed;
                    }
                }
            }

            ASSERT(ident->resolved_node != NULL, "ident not resolved");

            if (ident->resolved_node->flags & AST_FLAG_POLYMORPH
            &&  ident->poly_idx != -1) {


                ASSERT(ast_kind_is_decl(ident->resolved_node->kind), "poly, but doesn't reference a decl?");
                decl = (ast_decl_t*)ident->resolved_node;

                if (decl->val_expr->kind == AST_PROC) {
                    proc             = (ast_proc_t*)decl->val_expr;
                    spec_polymorphed = array_item(proc->polymorphs, ident->poly_idx);

                    if (spec_polymorphed->specialization != NULL) {
                        emit_underscore_name(((ast_decl_t*)spec_polymorphed->specialization)->full_name);
                        goto renamed;
                    }
                } else if (decl->val_expr->kind == AST_STRUCT) {
                    ASSERT(0, "unimplemented");
                }

                emit_underscore_name(decl->full_name);
                EMIT_STRING_F("__p%d", ident->poly_idx);

                goto renamed;
            } else if (ident->varg_idx != -1) {
                EMIT_STRING_F("__varg%d", ident->varg_idx);

                goto renamed;
            }

/*             if (ident->resolved_node->flags & AST_FLAG_IS_EXTERN) { */
/*                 emit_base_name(ident->str_rep); */
/*                 goto renamed; */
/*             } */

            if (ast_kind_is_decl(ident->resolved_node->kind)) {
                decl = (ast_decl_t*)ident->resolved_node;
                if (decl->scope->in_proc) { goto basic_name; }
                emit_underscore_name(decl->full_name);
            } else {
basic_name:;
                emit_underscore_name(ident->str_rep);
            }
renamed:;
            break;
        case AST_UNARY_EXPR:
            un_expr = (ast_unary_expr_t*)expr;
            op      = un_expr->op;

            if (op == OP_ADDR &&
                (type_kind(un_expr->child->type) == TY_TYPE
                || type_is_poly(un_expr->child->type))) {

                emit_expr(un_expr->child);
                EMIT_STRING("*");
            } else {
                switch (op) {
                    case OP_NOT:
                        EMIT_STRING("!");
                        break;
                    case OP_ADDR:
                        EMIT_STRING("&");
                        break;
                    case OP_DEREF:
                        EMIT_STRING("*");
                        break;
                    case OP_AUTOCAST:
                        EMIT_STRING("((");
                        emit_type_declarator(expr->type);
                        EMIT_C(')');
                        break;
                    default:
                        EMIT_STRING(OP_STR(op));
                }
                emit_expr(un_expr->child);
                if (op == OP_AUTOCAST) {
                    EMIT_C(')');
                }
            }
            break;
        case AST_BIN_EXPR:
            bin_expr = (ast_bin_expr_t*)expr;
            op       = bin_expr->op;

            switch (op) {
                case OP_CALL:
                    break;
                case OP_DOT:
                    if (!(expr->flags & AST_FLAG_BITFIELD_DOT)) {
                        goto do_left;
                    }
                    break;
                case OP_PLUS_ASSIGN:
                case OP_MINUS_ASSIGN:
                case OP_MULT_ASSIGN:
                case OP_DIV_ASSIGN:
                case OP_MOD_ASSIGN:
                case OP_ASSIGN:
                    if (bin_expr->left->flags & AST_FLAG_BITFIELD_DOT) {
                        emit_expr(((ast_bin_expr_t*)bin_expr->left)->left);
                    } else {
                        goto do_left;
                    }
                    break;
                do_left:;
                default:
                    emit_expr(bin_expr->left);
            }

            switch (op) {
                case OP_CALL:
                    if (ASTP(bin_expr)->flags & AST_FLAG_CALL_IS_CAST) {
                        EMIT_STRING("((");
                        arg_list = (ast_arg_list_t*)bin_expr->right;
                        arg      = array_item(arg_list->args, 0);
                        emit_type_declarator(arg->expr->value.t);
                        EMIT_STRING(")(");
                        arg = array_item(arg_list->args, 1);
                        emit_expr(arg->expr);
                        EMIT_STRING("))");
                    } else if (ASTP(bin_expr)->flags & AST_FLAG_CALL_IS_BUILTIN_VARG) {
                        EMIT_STRING_F("__varg%d", which_varg);
                    } else {
                        emit_expr(bin_expr->left);
                        EMIT_STRING("(");
                        emit_expr(bin_expr->right);
                        EMIT_STRING(")");
                    }
                    break;
                case OP_PLUS_ASSIGN:
                case OP_MINUS_ASSIGN:
                case OP_MULT_ASSIGN:
                case OP_DIV_ASSIGN:
                case OP_MOD_ASSIGN:
                case OP_ASSIGN:
                    if (bin_expr->left->flags & AST_FLAG_BITFIELD_DOT) {
                        st_ty   = ((ast_bin_expr_t*)bin_expr->left)->left->type;
                        st_decl = struct_type_to_decl(st_ty);
                        ASSERT(st_decl != NULL, "did not get struct decl");

                        st = (ast_struct_t*)st_decl->val_expr;

                        field = NULL;
                        array_traverse(st->fields, field_it) {
                            field = (ast_struct_field_t*)*field_it;
                            if (((ast_ident_t*)((ast_bin_expr_t*)(bin_expr->left))->right)->str_rep == field->name) {
                                break;
                            }
                            field = NULL;
                        }

                        ASSERT(field != NULL, "did not get field");

                        EMIT_STRING(" = (");
                        emit_expr(((ast_bin_expr_t*)bin_expr->left)->left);
                        EMIT_STRING_F(" & ~0x%"PRIx64") | (", field->bitfield_mask);

                        switch (op) {
                            case OP_PLUS_ASSIGN:
                                EMIT_C('(');
                                EMIT_C('(');
                                emit_expr(bin_expr->left);
                                EMIT_STRING(" + ((u64)");
                                emit_expr(bin_expr->right);
                                EMIT_C(')');
                                EMIT_C(')');
                                EMIT_STRING_F(" << %dULL) & 0x%"PRIx64")", field->bitfield_shift, field->bitfield_mask);
                                break;
                            case OP_MINUS_ASSIGN:
                                EMIT_C('(');
                                EMIT_C('(');
                                emit_expr(bin_expr->left);
                                EMIT_STRING(" - ((u64)");
                                emit_expr(bin_expr->right);
                                EMIT_C(')');
                                EMIT_C(')');
                                EMIT_STRING_F(" << %dULL) & 0x%"PRIx64")", field->bitfield_shift, field->bitfield_mask);
                                break;
                            case OP_MULT_ASSIGN:
                                EMIT_C('(');
                                EMIT_C('(');
                                emit_expr(bin_expr->left);
                                EMIT_STRING(" * ((u64)");
                                emit_expr(bin_expr->right);
                                EMIT_C(')');
                                EMIT_C(')');
                                EMIT_STRING_F(" << %dULL) & 0x%"PRIx64")", field->bitfield_shift, field->bitfield_mask);
                                break;
                            case OP_DIV_ASSIGN:
                                EMIT_C('(');
                                EMIT_C('(');
                                emit_expr(bin_expr->left);
                                EMIT_STRING(" / ((u64)");
                                emit_expr(bin_expr->right);
                                EMIT_C(')');
                                EMIT_C(')');
                                EMIT_STRING_F(" << %dULL) & 0x%"PRIx64")", field->bitfield_shift, field->bitfield_mask);
                                break;
                            case OP_MOD_ASSIGN:
                                EMIT_C('(');
                                EMIT_C('(');
                                emit_expr(bin_expr->left);
                                EMIT_STRING(" % ((u64)");
                                emit_expr(bin_expr->right);
                                EMIT_C(')');
                                EMIT_C(')');
                                EMIT_STRING_F(" << %dULL) & 0x%"PRIx64")", field->bitfield_shift, field->bitfield_mask);
                                break;
                            case OP_ASSIGN:
                                EMIT_STRING("(((u64)");
                                emit_expr(bin_expr->right);
                                EMIT_C(')');
                                EMIT_STRING_F(" << %dULL) & 0x%"PRIx64")", field->bitfield_shift, field->bitfield_mask);
                                break;
                        }
                    } else {
                        goto basic;
                    }
                    break;
                case OP_AND:
                    EMIT_STRING(" && ");
                    emit_expr(bin_expr->right);
                    break;
                case OP_OR:
                    EMIT_STRING(" || ");
                    emit_expr(bin_expr->right);
                    break;
                case OP_SUBSCRIPT:
                    EMIT_C('[');
                    emit_expr(bin_expr->right);
                    EMIT_C(']');
                    break;
                case OP_DOT:
                    if (expr->flags & AST_FLAG_BITFIELD_DOT) {
                        EMIT_C('(');
                        EMIT_C('(');
                        if (type_kind(bin_expr->left->type) == TY_PTR) {
                            EMIT_C('(');
                            EMIT_C('*');
                            emit_expr(bin_expr->left);
                            EMIT_C(')');
                        } else {
                            emit_expr(bin_expr->left);
                        }

                        st_ty   = bin_expr->left->type;
                        st_decl = struct_type_to_decl(st_ty);
                        ASSERT(st_decl != NULL, "did not get struct decl");

                        st = (ast_struct_t*)st_decl->val_expr;

                        field = NULL;
                        array_traverse(st->fields, field_it) {
                            field = (ast_struct_field_t*)*field_it;
                            if (((ast_ident_t*)bin_expr->right)->str_rep == field->name) {
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
                        if (type_kind(bin_expr->left->type) == TY_PTR) {
                            EMIT_C('-');
                            EMIT_C('>');
                        } else {
                            EMIT_C('.');
                        }
                        ASSERT(bin_expr->right->kind == AST_IDENT, "right of dot not an ident");
                        EMIT_STRING_ID(((ast_ident_t*)bin_expr->right)->str_rep);
                    }
                    break;
                basic:;
                default:
                    EMIT_STRING_F(" %s ", OP_STR(op));
                    emit_expr(bin_expr->right);
            }

            break;
        case AST_ARG_LIST:
            arg_list = (ast_arg_list_t*)expr;
            comma = "";
            array_traverse(arg_list->args, arg) {
                EMIT_STRING(comma);
                emit_expr(arg->expr);
                comma = ", ";
            }
            break;
        case AST_BUILTIN:
            EMIT_STRING("NULL");
            break;
        default:
            report_simple_err("encountered AST kind %s in emit_expr()",
                              ast_get_kind_str(expr->kind));
    }

    if (expr->flags & AST_FLAG_PAREN_EXPR) { EMIT_C(')'); }
}

static void emit_param(ast_param_t *param, const char *lazy_comma) {
    polymorph_constant_t *it;
    type_t                list_type;
    int                   i;

    if (ASTP(param)->flags & AST_FLAG_VARARGS) {
        if (ASTP(param)->flags & AST_FLAG_POLY_VARARGS) {
            ASSERT(polymorphed != NULL, "no poly constants");

            it = array_last(polymorphed->constants);
            ASSERT(it != NULL, "empty constants");
            ASSERT(it->name == ELLIPSIS_ID, "not dots");

            list_type  = get_type_t(it->value.t);
            for (i = 0; i < list_type.list_len; i += 1) {
                EMIT_STRING(lazy_comma);
                emit_type_declarator(list_type.id_list[i]);
                EMIT_C(' ');
                EMIT_STRING_F("__varg%d", i);
                lazy_comma = ", ";
            }
        } else {
            EMIT_STRING(lazy_comma);
            emit_expr(param->type_expr);
            EMIT_C('*');
            EMIT_C(' ');
            EMIT_STRING_ID(param->name);
        }
    } else {
        EMIT_STRING(lazy_comma);
        emit_expr(param->type_expr);
        EMIT_C(' ');
        EMIT_STRING_ID(param->name);
    }
}

static void emit_type_pre_decl(ast_decl_t *parent_decl) {
    ast_struct_t  *st;
    ast_decl_t   **mod_it;

    st = (ast_struct_t*)parent_decl->val_expr;

    if (st->bitfield_struct_bits != 0) {
        EMIT_STRING_F("typedef u%d ", st->bitfield_struct_bits);
    } else {
        EMIT_STRING("struct ");
    }

    array_traverse(mod_stack, mod_it) {
        EMIT_STRING("__");
        EMIT_STRING_ID((*mod_it)->name);
    }
    if (array_len(mod_stack) > 0) { EMIT_STRING("_"); }
    EMIT_STRING_ID(parent_decl->name);

    EMIT_STRING(";\n");
}


static void emit_proc_pre_decl(ast_decl_t *parent_decl) {
    ast_proc_t  *proc;
    ast_decl_t **mod_it;
    const char  *lazy_comma;
    ast_t      **it;

    if (polymorphed != NULL && polymorphed->specialization != NULL) { return; }

    proc = (ast_proc_t*)parent_decl->val_expr;

    if (proc->ret_type_expr != NULL) {
        emit_expr(proc->ret_type_expr);
        EMIT_C(' ');
    } else {
        EMIT_STRING("void ");
    }

    if (!(ASTP(proc)->flags & AST_FLAG_IS_EXTERN)) {
        array_traverse(mod_stack, mod_it) {
            EMIT_STRING("__");
            EMIT_STRING_ID((*mod_it)->name);
        }
        if (array_len(mod_stack) > 0) { EMIT_STRING("_"); }
    }
    EMIT_STRING_ID(parent_decl->name);

    if (polymorphed != NULL) {
        EMIT_STRING_F("__p%d", poly_version);
    }

    if (array_len(proc->params) == 0) {
        EMIT_STRING("(void)");
    } else {
        EMIT_C('(');
        lazy_comma = "";
        array_traverse(proc->params, it) {
            if ((*it)->type == TY_TYPE
            ||  (*it)->type == TY_MODULE) {

                continue;
            }
            emit_param((ast_param_t*)*it, lazy_comma);
            lazy_comma = ", ";
        }
        EMIT_C(')');
    }

    if (!(ASTP(proc)->flags & AST_FLAG_IS_EXTERN)) {
        EMIT_STRING(" asm (\"");
        EMIT_STRING_ID(parent_decl->full_name);
        if (polymorphed != NULL) {
            EMIT_STRING_F(".p%d", poly_version);
        }
        EMIT_STRING("\")");
    }

    EMIT_STRING(";\n");
}

static void emit_type_pre_decls_module(ast_decl_t *parent_decl) {
    ast_module_t  *module;
    ast_t        **nodep;
    ast_t         *node;
    ast_decl_t     *decl;
    ast_struct_t   *st;
    polymorphed_t  *it;

    module = (ast_module_t*)((ast_decl_t*)parent_decl)->val_expr;

    array_push(mod_stack, parent_decl);

    array_traverse(module->children, nodep) {
        node = *nodep;

        if (node->kind == AST_DECL_MODULE) {
            emit_type_pre_decls_module((ast_decl_t*)node);
        } else if (node->kind == AST_DECL_STRUCT) {
            if (node->flags & AST_FLAG_POLYMORPH) {
                decl         = (ast_decl_t*)node;
                st           = (ast_struct_t*)decl->val_expr;
                poly_version = 0;
                array_traverse(st->polymorphs, it) {
                    polymorphed = it;
                    emit_type_pre_decl(decl);
                    polymorphed = NULL;
                    poly_version += 1;
                }
            } else {
                emit_type_pre_decl((ast_decl_t*)node);
            }
        }
    }

    array_pop(mod_stack);
}

static void emit_type_pre_decls_global(void) {
    ast_t         **rootp;
    ast_t          *root;
    ast_decl_t     *decl;
    ast_struct_t   *st;
    polymorphed_t  *it;

    array_traverse(roots, rootp) {
        root = *rootp;

        if (root->kind == AST_DECL_MODULE) {
            emit_type_pre_decls_module((ast_decl_t*)root);
        } else if (root->kind == AST_DECL_STRUCT) {
            if (root->flags & AST_FLAG_POLYMORPH) {
                decl         = (ast_decl_t*)root;
                st           = (ast_struct_t*)decl->val_expr;
                poly_version = 0;
                array_traverse(st->polymorphs, it) {
                    polymorphed = it;
                    emit_type_pre_decl(decl);
                    polymorphed = NULL;
                    poly_version += 1;
                }
            } else {
                emit_type_pre_decl((ast_decl_t*)root);
            }
        }
    }
}

static void emit_proc_pre_decls_module(ast_decl_t *parent_decl) {
    ast_module_t  *module;
    ast_t        **nodep;
    ast_t         *node;
    ast_decl_t     *decl;
    ast_proc_t     *proc;
    polymorphed_t  *it;

    module = (ast_module_t*)((ast_decl_t*)parent_decl)->val_expr;

    array_push(mod_stack, parent_decl);

    array_traverse(module->children, nodep) {
        node = *nodep;

        if (node->kind == AST_DECL_MODULE) {
            emit_proc_pre_decls_module((ast_decl_t*)node);
        } else if (node->kind == AST_DECL_PROC) {
            if (node->flags & AST_FLAG_POLYMORPH) {
                decl         = (ast_decl_t*)node;
                proc         = (ast_proc_t*)decl->val_expr;
                poly_version = 0;
                array_traverse(proc->polymorphs, it) {
                    polymorphed = it;
                    emit_proc_pre_decl(decl);
                    polymorphed = NULL;
                    poly_version += 1;
                }
            } else {
                emit_proc_pre_decl((ast_decl_t*)node);
            }
        }
    }

    array_pop(mod_stack);
}

static void emit_proc_pre_decls_global(void) {
    ast_t         **rootp;
    ast_t          *root;
    ast_decl_t     *decl;
    ast_proc_t     *proc;
    polymorphed_t  *it;

    array_traverse(roots, rootp) {
        root = *rootp;

        if (root->kind == AST_DECL_MODULE) {
            emit_proc_pre_decls_module((ast_decl_t*)root);
        } else if (root->kind == AST_DECL_PROC) {
            if (root->flags & AST_FLAG_POLYMORPH) {
                decl         = (ast_decl_t*)root;
                proc         = (ast_proc_t*)decl->val_expr;
                poly_version = 0;
                array_traverse(proc->polymorphs, it) {
                    polymorphed = it;
                    emit_proc_pre_decl(decl);
                    polymorphed = NULL;
                    poly_version += 1;
                }
            } else {
                emit_proc_pre_decl((ast_decl_t*)root);
            }
        }
    }
}

#define INDENT(_lvl)                                            \
do {                                                            \
    int _i;                                                     \
    for (_i = 0; _i < (_lvl); _i += 1) { EMIT_STRING("    "); } \
} while (0)

static void emit_var_decl(ast_decl_t *decl) {
    emit_type_declarator(ASTP(decl)->type);
    EMIT_STRING(" ");
    EMIT_STRING_ID(decl->name);
    if (decl->val_expr != NULL) {
        EMIT_STRING(" = ");
        emit_expr(decl->val_expr);
    }
}

enum {
    BLOCK_NORMAL,
    BLOCK_PROC_BODY,
    BLOCK_LOOP,
};

static void emit_stmt(ast_t *stmt, int indent_level);

static void _emit_block(ast_block_t *block, int indent_level, int block_kind, ast_t *proc_ret_type_expr);

static void emit_block(ast_block_t *block, int indent_level) {
    _emit_block(block, indent_level, BLOCK_NORMAL, NULL);
}

static void emit_proc_block(ast_block_t *block, int indent_level, ast_t *proc_ret_type_expr) {
    _emit_block(block, indent_level, BLOCK_PROC_BODY, proc_ret_type_expr);
}

static void emit_loop_block(ast_block_t *block, int indent_level) {
    _emit_block(block, indent_level, BLOCK_LOOP, NULL);
}

static void emit_defers(int indent_level) {
    array_t  *scope;
    ast_t   **stmt_it;

    scope = array_last(defer_stack);
    if (scope == NULL) { return; }

    if (array_len(*scope) == 0) { return; }

    INDENT(indent_level); EMIT_STRING_F("{ /* DEFER SCOPE %"PRIu64" */\n", TOP_DEFER_LABEL());
    array_rtraverse(*scope, stmt_it) {
        emit_stmt(*stmt_it, indent_level + 1);
    }
    INDENT(indent_level); EMIT_STRING("}\n");
}

static void emit_all_defers_except_return(int indent_level) {
    array_t  *scope;
    ast_t   **stmt_it;

    array_rtraverse(defer_stack, scope) {
        if (scope == array_item(defer_stack, 0)) { break; }

        if (array_len(*scope) == 0) { continue; }

        INDENT(indent_level); EMIT_STRING_F("{ /* DEFER SCOPE %"PRIu64" */\n", TOP_DEFER_LABEL());
        array_rtraverse(*scope, stmt_it) {
            emit_stmt(*stmt_it, indent_level + 1);
        }
        INDENT(indent_level); EMIT_STRING("}\n");
    }
}

static void emit_all_defers_for_loop(int indent_level) {
    int       i;
    array_t  *scope;
    ast_t   **stmt_it;

    i = array_len(defer_stack) - 1;
    array_rtraverse(defer_stack, scope) {
        if (*(u64*)array_item(defer_label_stack, i) == TOP_LOOP_LABEL()) { break; }

        if (array_len(*scope) == 0) { goto next; }

        INDENT(indent_level); EMIT_STRING_F("{ /* DEFER SCOPE %"PRIu64" */\n", TOP_DEFER_LABEL());
        array_rtraverse(*scope, stmt_it) {
            emit_stmt(*stmt_it, indent_level + 1);
        }
        INDENT(indent_level); EMIT_STRING("}\n");

next:;
        i -= 1;
    }
}

static void _emit_block(ast_block_t *block, int indent_level, int block_kind, ast_t *proc_ret_type_expr) {
    ast_t **stmt;

    INDENT(indent_level); EMIT_STRING("{\n");

    if (ASTP(block)->kind == AST_BLOCK) { PUSH_DEFER_SCOPE(block_kind); }

    if (block_kind == BLOCK_PROC_BODY && proc_ret_type_expr != NULL) {
        INDENT(indent_level + 1);
        emit_expr(proc_ret_type_expr);
        EMIT_STRING(" _simon_proc_ret;\n\n");
    }

    array_traverse(block->stmts, stmt) { emit_stmt(*stmt, indent_level + 1); }

/*     if (block_kind == BLOCK_LOOP) { */
/*         ASSERT(ASTP(block)->kind == AST_BLOCK, "loop block should not be SD"); */
/*  */
/*         EMIT_STRING_F("\n_simon_loop_%"PRIu64"_continue:;\n", loop_label_counter); */
/*         emit_defers(indent_level + 1); */
/*         INDENT(indent_level + 1); EMIT_STRING("continue;\n"); */
/*         EMIT_STRING_F("\n_simon_loop_%"PRIu64"_break:;\n", loop_label_counter); */
/*     } */

    if (ASTP(block)->kind == AST_BLOCK) {
        if (block_kind == BLOCK_PROC_BODY || HAVE_DEFERS()) {
            EMIT_STRING_F("\n_simon_scope_%"PRIu64"_exit:;\n", TOP_DEFER_LABEL());
            emit_defers(indent_level + 1);
        }
        POP_DEFER_SCOPE(block_kind);
/*         if (block_kind == BLOCK_LOOP) { */
/*             INDENT(indent_level + 1); EMIT_STRING("break;\n"); */
/*       continue} */
    }

    if (block_kind == BLOCK_PROC_BODY && proc_ret_type_expr != NULL) {
        INDENT(indent_level + 1);
        EMIT_STRING("return _simon_proc_ret;\n");
    }

    defer_label_counter += 1;

    INDENT(indent_level); EMIT_STRING("}\n");
}

static void emit_stmt(ast_t *stmt, int indent_level) {
    u32                    vargs_ty;
    polymorph_constant_t  *it;
    type_t                 list_type;
    ast_block_t           *block;
    int                    i;
    ast_t                 *subblock;
    ast_t                **stmt_it;

    switch (stmt->kind) {
        case AST_BLOCK:
        case AST_SD_BLOCK:
        case AST_STATIC_VARGS:
            break;
        default:
            INDENT(indent_level);
    }

    switch (stmt->kind) {
        case AST_DECL_VAR:
            emit_var_decl((ast_decl_t*)stmt);
            EMIT_STRING(";\n");
            break;
        case AST_BIN_EXPR:
        case AST_UNARY_EXPR:
            emit_expr(stmt);
            EMIT_STRING(";\n");
            break;
        case AST_BLOCK:
        case AST_SD_BLOCK:
            emit_block((ast_block_t*)stmt, indent_level);
            break;
        case AST_IF:
            EMIT_STRING("if (");
            emit_expr(((ast_if_t*)stmt)->expr);
            EMIT_STRING(")\n");
            emit_block((ast_block_t*)((ast_if_t*)stmt)->then_block, indent_level);
            if (((ast_if_t*)stmt)->els != NULL) {
                INDENT(indent_level);
                EMIT_STRING("else\n");
                if (((ast_if_t*)stmt)->els->kind == AST_IF) {
                    emit_stmt(((ast_if_t*)stmt)->els, indent_level);
                } else {
                    ASSERT(((ast_if_t*)stmt)->els->kind == AST_BLOCK, "els is not an if, but is also not a block");
                    emit_block((ast_block_t*)((ast_if_t*)stmt)->els, indent_level);
                }
            }
            break;
        case AST_LOOP:
            EMIT_STRING("for (");
            if (((ast_loop_t*)stmt)->init != NULL) {
                emit_var_decl((ast_decl_t*)((ast_loop_t*)stmt)->init);
            }
            EMIT_STRING("; ");
            if (((ast_loop_t*)stmt)->cond != NULL) {
                emit_expr(((ast_loop_t*)stmt)->cond);
            }
            EMIT_STRING("; ");
            if (((ast_loop_t*)stmt)->post != NULL) {
                emit_expr(((ast_loop_t*)stmt)->post);
            }
            EMIT_STRING(")\n");
            emit_loop_block((ast_block_t*)((ast_loop_t*)stmt)->block, indent_level);

            loop_label_counter += 1;
            break;
        case AST_RETURN:
            if (((ast_return_t*)stmt)->expr != NULL) {
                EMIT_STRING("_simon_proc_ret = ");
                emit_expr(((ast_return_t*)stmt)->expr);
                EMIT_STRING(";\n");
                INDENT(indent_level);
            }
            emit_all_defers_except_return(indent_level);
            EMIT_STRING_F("goto _simon_scope_%"PRIu64"_exit;\n", BOTTOM_DEFER_LABEL());
            break;
        case AST_DEFER:
            (void)stmt_it;
            PUSH_DEFER(((ast_defer_t*)stmt)->block);
/*             block = (ast_block_t*)((ast_defer_t*)stmt)->block; */
/*             array_traverse(block->stmts, stmt_it) { */
/*                 PUSH_DEFER(*stmt_it); */
/*             } */
            break;
        case AST_BREAK:
            emit_all_defers_for_loop(indent_level);
            EMIT_STRING("break;\n");
            break;
        case AST_CONTINUE:
            emit_all_defers_for_loop(indent_level);
            EMIT_STRING("continue;\n");
            break;
        case AST_STATIC_VARGS:
            if (stmt->type == TY_POLY) {
                ASSERT(polymorphed != NULL, "no poly constants?");
                vargs_ty = TY_UNKNOWN;

                array_rtraverse(polymorphed->constants, it) {
                    if (it->name == ELLIPSIS_ID) {
                        ASSERT(it->type == TY_TYPE, "... should be a type value");
                        vargs_ty = it->value.t;
                        break;
                    }
                }
                ASSERT(vargs_ty != TY_UNKNOWN, "did not find ... constant");
                ASSERT(type_kind(vargs_ty) == _TY_TYPE_LIST, "... type is not a type list");

                list_type = get_type_t(vargs_ty);
                block     = (ast_block_t*)((ast_static_vargs_t*)stmt)->block;
                ASSERT(array_len(block->stmts) == list_type.list_len, "num vargs blocks doesn't match num types");

                which_varg = 0;
                for (i = 0; i < list_type.list_len; i += 1) {
                    varg_ty  = list_type.id_list[i];
                    subblock = *(ast_t**)array_item(block->stmts, i);
                    emit_stmt(subblock, indent_level);
                    which_varg += 1;
                }
            } else {
                ASSERT(0, "unimplemented");
            }

            varg_ty = TY_UNKNOWN;
            break;
        default:
            report_simple_err("encountered AST kind %s in emit_stmt()",
                                ast_get_kind_str(stmt->kind));
    }
}

static void emit_type(ast_decl_t *parent_decl) {
    ast_struct_t        *st;
    ast_decl_t         **mod_it;
    ast_t              **it;
    ast_struct_field_t  *field;

    st = (ast_struct_t*)parent_decl->val_expr;

    if (st->bitfield_struct_bits != 0) {
        return;
    }

    EMIT_STRING("typedef struct ");

    array_traverse(mod_stack, mod_it) {
        EMIT_STRING("__");
        EMIT_STRING_ID((*mod_it)->name);
    }
    if (array_len(mod_stack) > 0) { EMIT_STRING("_"); }
    EMIT_STRING_ID(parent_decl->name);
    if (polymorphed != NULL) {
        EMIT_STRING_F("__p%d", poly_version);
    }

    EMIT_STRING(" {\n");
    array_traverse(st->fields, it) {
        field = (ast_struct_field_t*)*it;

        INDENT(1);
        emit_type_declarator(field->type_expr->value.t);
        EMIT_C(' ');
        EMIT_STRING_ID(field->name);
        EMIT_C(';');
        EMIT_C('\n');
    }
    EMIT_STRING("} ");

    array_traverse(mod_stack, mod_it) {
        EMIT_STRING("__");
        EMIT_STRING_ID((*mod_it)->name);
    }
    if (array_len(mod_stack) > 0) { EMIT_STRING("_"); }
    EMIT_STRING_ID(parent_decl->name);
    if (polymorphed != NULL) {
        EMIT_STRING_F("__p%d", poly_version);
    }

    EMIT_C(';');
    EMIT_C('\n');
    EMIT_C('\n');
}

static void emit_types_module(ast_decl_t *parent_decl) {
    ast_module_t   *module;
    ast_t         **nodep;
    ast_t          *node;
    ast_decl_t     *decl;
    ast_struct_t   *st;
    polymorphed_t  *it;

    module = (ast_module_t*)parent_decl->val_expr;

    array_push(mod_stack, parent_decl);

    array_traverse(module->children, nodep) {
        node = *nodep;

        if (node->kind == AST_DECL_MODULE) {
            emit_types_module((ast_decl_t*)node);
        } else if (node->kind == AST_DECL_STRUCT) {
            if (node->flags & AST_FLAG_POLYMORPH) {
                decl         = (ast_decl_t*)node;
                st           = (ast_struct_t*)decl->val_expr;
                poly_version = 0;
                array_traverse(st->polymorphs, it) {
                    polymorphed = it;
                    emit_type(decl);
                    polymorphed = NULL;
                    poly_version += 1;
                }
            } else {
                emit_type((ast_decl_t*)node);
            }
        }
    }

    array_pop(mod_stack);
}

static void emit_types_global(void) {
    ast_t         **rootp;
    ast_t          *root;
    ast_decl_t     *decl;
    ast_struct_t   *st;
    polymorphed_t  *it;

    array_traverse(roots, rootp) {
        root = *rootp;

        if (root->kind == AST_DECL_MODULE) {
            emit_types_module((ast_decl_t*)root);
        } else if (root->kind == AST_DECL_STRUCT) {
            if (root->flags & AST_FLAG_POLYMORPH) {
                decl         = (ast_decl_t*)root;
                st           = (ast_struct_t*)decl->val_expr;
                poly_version = 0;
                array_traverse(st->polymorphs, it) {
                    polymorphed = it;
                    emit_type(decl);
                    polymorphed = NULL;
                    poly_version += 1;
                }
            } else {
                emit_type((ast_decl_t*)root);
            }
        }
    }
}

static void emit_var(ast_decl_t *parent_decl) {
    ast_decl_t **mod_it;

    emit_type_declarator(ASTP(parent_decl)->type);
    EMIT_C(' ');
    array_traverse(mod_stack, mod_it) {
        EMIT_STRING("__");
        EMIT_STRING_ID((*mod_it)->name);
    }
    if (array_len(mod_stack) > 0) { EMIT_STRING("_"); }
    EMIT_STRING_ID(parent_decl->name);

    EMIT_STRING(" asm (\"");
    if (ASTP(parent_decl)->flags & AST_FLAG_IS_EXTERN) {
        EMIT_STRING_ID(parent_decl->name);
    } else {
        EMIT_STRING_ID(parent_decl->full_name);
    }
    if (polymorphed != NULL) {
        EMIT_STRING_F(".p%d", poly_version);
    }
    EMIT_STRING("\")");

    if (parent_decl->val_expr != NULL) {
        EMIT_STRING(" = ");
        emit_expr(parent_decl->val_expr);
    }

    EMIT_C(';');
    EMIT_C('\n');
}

static void emit_vars_module(ast_decl_t *parent_decl) {
    ast_module_t   *module;
    ast_t         **nodep;
    ast_t          *node;
    ast_decl_t     *decl;

    module = (ast_module_t*)parent_decl->val_expr;

    array_push(mod_stack, parent_decl);

    array_traverse(module->children, nodep) {
        node = *nodep;
        decl = (ast_decl_t*)node;

        if (node->kind == AST_DECL_MODULE) {
            emit_vars_module(decl);
        } else if (node->kind == AST_DECL_VAR) {
            emit_var(decl);
        }
    }

    array_pop(mod_stack);
}

static void emit_vars_global(void) {
    ast_t      **rootp;
    ast_t       *root;
    ast_decl_t  *decl;

    array_traverse(roots, rootp) {
        root = *rootp;
        decl = (ast_decl_t*)root;

        if (root->kind == AST_DECL_MODULE) {
            emit_vars_module(decl);
        } else if (root->kind == AST_DECL_VAR) {
            emit_var(decl);
        }
    }
}

static void emit_proc(ast_decl_t *parent_decl, ast_proc_t *p) {
    ast_decl_t **mod_it;
    const char  *lazy_comma;
    ast_t      **it;


    if (polymorphed != NULL && polymorphed->specialization != NULL) { return; }

    proc = p;

    if (ASTP(proc)->flags & AST_FLAG_IS_EXTERN) { return; }

    if (polymorphed != NULL) {
        proc_ty = polymorphed->type;
    } else {
        proc_ty = ASTP(proc)->type;
    }

    if (proc->ret_type_expr != NULL) {
        emit_expr(proc->ret_type_expr);
        EMIT_C(' ');
    } else {
        EMIT_STRING("void ");
    }

    array_traverse(mod_stack, mod_it) {
        EMIT_STRING("__");
        EMIT_STRING_ID((*mod_it)->name);
    }
    if (array_len(mod_stack) > 0) { EMIT_STRING("_"); }
    EMIT_STRING_ID(parent_decl->name);

    if (polymorphed != NULL) {
        EMIT_STRING_F("__p%d", poly_version);
    }

    if (array_len(proc->params) == 0) {
        EMIT_STRING("(void)");
    } else {
        EMIT_C('(');
        lazy_comma = "";
        array_traverse(proc->params, it) {
            if ((*it)->type == TY_TYPE
            ||  (*it)->type == TY_MODULE) {

                continue;
            }
            emit_param((ast_param_t*)*it, lazy_comma);
            lazy_comma = ", ";
        }
        EMIT_C(')');
    }

    EMIT_STRING("\n");
    emit_proc_block((ast_block_t*)proc->block, 0, proc->ret_type_expr);
    EMIT_STRING("\n");

    proc = NULL;
}

static void emit_procs_module(ast_decl_t *parent_decl) {
    ast_module_t   *module;
    ast_t         **nodep;
    ast_t          *node;
    ast_decl_t     *decl;
    ast_proc_t     *proc;
    polymorphed_t  *it;

    module = (ast_module_t*)parent_decl->val_expr;

    array_push(mod_stack, parent_decl);

    array_traverse(module->children, nodep) {
        node = *nodep;

        if (node->kind == AST_DECL_MODULE) {
            emit_procs_module((ast_decl_t*)node);
        } else if (node->kind == AST_DECL_PROC) {
            decl = (ast_decl_t*)node;

            if (node->flags & AST_FLAG_POLYMORPH) {
                proc         = (ast_proc_t*)decl->val_expr;
                poly_version = 0;
                array_traverse(proc->polymorphs, it) {
                    polymorphed = it;
                    emit_proc(decl, (ast_proc_t*)it->node);
                    polymorphed = NULL;
                    poly_version += 1;
                }
            } else {
                polymorphed  = NULL;
                poly_version = 0;
                emit_proc(decl, (ast_proc_t*)decl->val_expr);
            }
        }
    }

    array_pop(mod_stack);
}

static void emit_procs_global(void) {
    ast_t         **rootp;
    ast_t          *root;
    ast_decl_t     *decl;
    ast_proc_t     *proc;
    polymorphed_t  *it;

    array_traverse(roots, rootp) {
        root = *rootp;

        if (root->kind == AST_DECL_MODULE) {
            emit_procs_module((ast_decl_t*)root);
        } else if (root->kind == AST_DECL_PROC) {
            decl = (ast_decl_t*)root;

            if (root->flags & AST_FLAG_POLYMORPH) {
                proc         = (ast_proc_t*)decl->val_expr;
                poly_version = 0;
                array_traverse(proc->polymorphs, it) {
                    polymorphed = it;
                    emit_proc(decl, (ast_proc_t*)it->node);
                    polymorphed = NULL;
                    poly_version += 1;
                }
            } else {
                polymorphed  = NULL;
                poly_version = 0;
                emit_proc(decl, (ast_proc_t*)decl->val_expr);
            }
        }
    }
}

void do_c_backend(void) {
    u64  start_us;
    int  err;
    char exe_name[128];
    char cmd_buff[4096];

    start_us = measure_time_now_us();

    mod_stack         = array_make(ast_decl_t*);
    defer_stack       = array_make(array_t);
    defer_label_stack = array_make(u64);
    loop_label_stack  = array_make(u64);

    emit_prelude();
    EMIT_STRING("\n");
    emit_type_pre_decls_global();
    EMIT_STRING("\n\n");
    emit_types_global();
    EMIT_STRING("\n\n");
    emit_proc_pre_decls_global();
    EMIT_STRING("\n\n");
    emit_vars_global();
    EMIT_STRING("\n\n");
    emit_procs_global();

    fflush(output_file);

    verb_message("C generation took %lu us\n", measure_time_now_us() - start_us);

    if (options.c_source) { return; }

    strcpy(exe_name, options.output_name);
    if (exe_name[strlen(exe_name) - 1] == 'c'
    &&  exe_name[strlen(exe_name) - 2] == '.') {
        exe_name[strlen(exe_name) - 2] = 0;
    }

    sprintf(cmd_buff, "cc -o %s %s -O0 -nostdlib -ffreestanding -fno-builtin -Wl,-e,%s",
            exe_name, options.output_name, get_string(program_entry->full_name));

#ifdef __APPLE__
    strcat(cmd_buff, " -lSystem");
#endif

    err = system(cmd_buff);

    if (err != 0) {
        report_simple_err("c backend failed");
    }
}
