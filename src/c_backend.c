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

static void emit_base_name(string_id name) {
    const char *s;
    int         len;
    const char *t;

    s = get_string(name);
    len = strlen(s);

    if (len <= 0) { return; }

    for (t = s + len - 1; t >= s; t -= 1) {
        if (*t == '.') {
            EMIT_STRING(t + 1);
            break;
        }
    }
}

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
"\n";

    EMIT_STRING(prelude);
}

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
            EMIT_STRING("char*");
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
    ast_ident_t          *ident;
    polymorph_constant_t *it;
    ast_decl_t           *decl;
    ast_unary_expr_t     *un_expr;
    int                   op;
    ast_bin_expr_t       *bin_expr;
    ast_arg_list_t       *arg_list;
    arg_t                *arg;
    const char           *comma;

    switch(expr->kind) {
        case AST_INT:
            EMIT_STRING_F("%"PRIi64"LL", expr->value.i);
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

            if (ident->resolved_node->flags & AST_FLAG_IS_EXTERN) {
                emit_base_name(ident->str_rep);
                goto renamed;
            }

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

            if (op == OP_ADDR && type_kind(un_expr->child->type) == TY_TYPE) {
                emit_expr(un_expr->child);
                EMIT_STRING("*");
            } else {
                switch (op) {
                    case OP_DEREF:
                        EMIT_STRING("*");
                        break;
                    default:
                        EMIT_STRING(OP_STR(op));
                }
                emit_expr(un_expr->child);
            }
            break;
        case AST_BIN_EXPR:
            bin_expr = (ast_bin_expr_t*)expr;
            op       = bin_expr->op;

            if (op == OP_CALL) {
                if (ASTP(bin_expr)->flags & AST_FLAG_CALL_IS_CAST) {
                    EMIT_STRING("(");
                    arg_list = (ast_arg_list_t*)bin_expr->right;
                    arg      = array_item(arg_list->args, 0);
                    emit_type_declarator(arg->expr->value.t);
                    EMIT_STRING(")(");
                    arg = array_item(arg_list->args, 1);
                    emit_expr(arg->expr);
                    EMIT_STRING(")");
                } else {
                    emit_expr(bin_expr->left);
                    EMIT_STRING("(");
                    emit_expr(bin_expr->right);
                    EMIT_STRING(")");
                }
            } else {
                emit_expr(bin_expr->left);

                if (op == OP_SUBSCRIPT) {
                    EMIT_C('[');
                    emit_expr(bin_expr->right);
                    EMIT_C(']');
                } else {
                    EMIT_STRING_F(" %s ", OP_STR(op));
                    emit_expr(bin_expr->right);
                }
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
}

static void emit_param(ast_param_t *param) {
    polymorph_constant_t *it;
    type_t                list_type;
    const char           *lazy_comma;
    int                   i;

    if (ASTP(param)->flags & AST_FLAG_VARARGS) {
        if (ASTP(param)->flags & AST_FLAG_POLY_VARARGS) {
            ASSERT(polymorphed != NULL, "no poly constants");

            it = array_last(polymorphed->constants);
            ASSERT(it != NULL, "empty constants");
            ASSERT(it->name == ELLIPSIS_ID, "not dots");

            list_type  = get_type_t(it->value.t);
            lazy_comma = "";

            for (i = 0; i < list_type.list_len; i += 1) {
                EMIT_STRING(lazy_comma);
                emit_type_declarator(list_type.id_list[i]);
                EMIT_C(' ');
                EMIT_STRING_ID(param->name);
                EMIT_STRING_F("__varg%d", i);
                lazy_comma = ", ";
            }
        } else {
            emit_expr(param->type_expr);
            EMIT_C('*');
            EMIT_C(' ');
            EMIT_STRING_ID(param->name);
        }
    } else {
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
            EMIT_STRING(lazy_comma);
            emit_param((ast_param_t*)*it);
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

static void emit_stmt(ast_t *stmt, int indent_level);

static void emit_block(ast_block_t *block, int indent_level) {
    ast_t **stmt;

    INDENT(indent_level); EMIT_STRING("{\n");

    array_traverse(block->stmts, stmt) { emit_stmt(*stmt, indent_level + 1); }

    INDENT(indent_level); EMIT_STRING("}\n");
}

static void emit_stmt(ast_t *stmt, int indent_level) {
    INDENT(indent_level);

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
            emit_block((ast_block_t*)((ast_loop_t*)stmt)->block, indent_level);
            break;
        case AST_RETURN:
            EMIT_STRING("return ");
            if (((ast_return_t*)stmt)->expr != NULL) {
                emit_expr(((ast_return_t*)stmt)->expr);
            }
            EMIT_STRING(";\n");
            break;
        case AST_DEFER:      break;
        case AST_BREAK:
            EMIT_STRING("break\n");
            EMIT_STRING(";\n");
            break;
        case AST_CONTINUE:
            EMIT_STRING("continue\n");
            EMIT_STRING(";\n");
            break;
        case AST_STATIC_VARGS:
            break;
        default:
            report_simple_err("encountered AST kind %s in emit_stmt()",
                                ast_get_kind_str(stmt->kind));
    }
}

static void emit_type(ast_decl_t *parent_decl) {
    ast_struct_t  *st;
    ast_decl_t   **mod_it;

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

    if (st->bitfield_struct_bits != 0) {
        EMIT_STRING("{\n");
        EMIT_STRING("} ");
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

    EMIT_C(';');
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
    EMIT_STRING_ID(parent_decl->full_name);
    if (polymorphed != NULL) {
        EMIT_STRING_F(".p%d", poly_version);
    }
    EMIT_STRING("\")");

    if (parent_decl->val_expr != NULL) {
        EMIT_STRING(" = ");
        emit_expr(parent_decl->val_expr);
    }

    EMIT_C(';');
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

static void emit_proc(ast_decl_t *parent_decl) {
    ast_decl_t **mod_it;
    const char  *lazy_comma;
    ast_t      **it;


    proc = (ast_proc_t*)parent_decl->val_expr;

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
            EMIT_STRING(lazy_comma);
            emit_param((ast_param_t*)*it);
            lazy_comma = ", ";
        }
        EMIT_C(')');
    }

    EMIT_STRING("\n");
    emit_block((ast_block_t*)proc->block, 0);
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
            if (node->flags & AST_FLAG_POLYMORPH) {
                decl         = (ast_decl_t*)node;
                proc         = (ast_proc_t*)decl->val_expr;
                poly_version = 0;
                array_traverse(proc->polymorphs, it) {
                    polymorphed = it;
                    emit_proc(decl);
                    polymorphed = NULL;
                    poly_version += 1;
                }
            } else {
                emit_proc((ast_decl_t*)node);
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
            if (root->flags & AST_FLAG_POLYMORPH) {
                decl         = (ast_decl_t*)root;
                proc         = (ast_proc_t*)decl->val_expr;
                poly_version = 0;
                array_traverse(proc->polymorphs, it) {
                    polymorphed = it;
                    emit_proc(decl);
                    polymorphed = NULL;
                    poly_version += 1;
                }
            } else {
                emit_proc((ast_decl_t*)root);
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

    mod_stack = array_make(ast_decl_t*);

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
