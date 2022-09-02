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

static array_t mod_stack;

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
"\n";

    EMIT_STRING(prelude);
}

static void emit_type_declarator(u32 t) {
    switch (type_kind(t)) {
        case TY_NOT_TYPED:
            EMIT_STRING("void");
            break;
        case TY_GENERIC_INT:
        case TY_CHAR:
            EMIT_STRING_ID(get_type_string_id(t));
            break;
        case TY_PTR:
            emit_type_declarator(get_under_type(t));
            EMIT_STRING("*");
            break;
        default:
            report_simple_err("type '%s' not handled in emit_type_declarator()", get_string(get_type_string_id(t)));
            return;
    }
}

static void emit_proc_pre_decl(ast_decl_t *parent_decl) {
    ast_proc_t  *proc;
    u32          proc_type;
    u32          ret_type;
    ast_decl_t **mod_it;
    u32          num_param_types;
    u32          i;
    u32          param_type;

    proc      = (ast_proc_t*)parent_decl->val_expr;
    proc_type = ASTP(proc)->type;
    ret_type  = get_ret_type(proc_type);

    emit_type_declarator(ret_type); EMIT_STRING(" ");

    if (!(ASTP(proc)->flags & AST_FLAG_IS_EXTERN)) {
        array_traverse(mod_stack, mod_it) {
            EMIT_STRING("__");
            EMIT_STRING_ID((*mod_it)->name);
        }
        if (array_len(mod_stack) > 0) { EMIT_STRING("_"); }
    }
    EMIT_STRING_ID(parent_decl->name);

    num_param_types = get_num_param_types(proc_type);
    if (num_param_types == 0) {
        EMIT_STRING("(void)");
    } else {
        EMIT_STRING("(");
        for (i = 0; i < num_param_types; i += 1) {
            param_type = get_param_type(proc_type, i);
            emit_type_declarator(param_type);
            if (i < num_param_types - 1) {
                EMIT_STRING(", ");
            }
        }
        EMIT_STRING(")");
    }

    EMIT_STRING(";\n");
}

static void emit_proc_pre_decls_module(ast_decl_t *parent_decl) {
    ast_module_t  *module;
    ast_t        **nodep;
    ast_t         *node;

    module = (ast_module_t*)((ast_decl_t*)parent_decl)->val_expr;

    array_push(mod_stack, parent_decl);

    array_traverse(module->children, nodep) {
        node = *nodep;

        if (node->kind == AST_DECL_MODULE) {
            emit_proc_pre_decls_module((ast_decl_t*)node);
        } else if (node->kind == AST_DECL_PROC) {
            emit_proc_pre_decl((ast_decl_t*)node);
        }
    }

    array_pop(mod_stack);
}

static void emit_proc_pre_decls_global(void) {
    ast_t **rootp;
    ast_t  *root;

    array_traverse(roots, rootp) {
        root = *rootp;

        if (root->kind == AST_DECL_MODULE) {
            emit_proc_pre_decls_module((ast_decl_t*)root);
        } else if (root->kind == AST_DECL_PROC) {
            emit_proc_pre_decl((ast_decl_t*)root);
        }
    }
}

#define INDENT(_lvl)                                            \
do {                                                            \
    int _i;                                                     \
    for (_i = 0; _i < (_lvl); _i += 1) { EMIT_STRING("    "); } \
} while (0)


static void emit_expr(ast_t *expr) {
    ast_unary_expr_t *un_expr;
    int               op;
    ast_bin_expr_t   *bin_expr;
    ast_arg_list_t   *arg_list;
    arg_t            *arg;
    const char       *comma;

    switch(expr->kind) {
        case AST_INT:
            EMIT_STRING_F("%lld", expr->value.i);
            break;
        case AST_STRING:
            EMIT_STRING_ID(((ast_string_t*)expr)->str_rep);
            break;
        case AST_IDENT:
            EMIT_STRING_ID(((ast_ident_t*)expr)->str_rep);
            break;
        case AST_UNARY_EXPR:
            un_expr = (ast_unary_expr_t*)expr;
            op      = un_expr->op;
            switch (op) {
                case OP_DEREF:
                    EMIT_STRING("*");
                    break;
                default:
                    EMIT_STRING(OP_STR(op));
            }
            emit_expr(un_expr->child);
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
                EMIT_STRING_F(" %s ", OP_STR(op));
                emit_expr(bin_expr->right);
                if (op == OP_SUBSCRIPT) { EMIT_STRING("]"); }
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
            if (((ast_loop_t*)stmt)->init != NULL){
                emit_var_decl((ast_decl_t*)((ast_loop_t*)stmt)->init);
            }
            EMIT_STRING("; ");
            if (((ast_loop_t*)stmt)->cond != NULL){
                emit_expr(((ast_loop_t*)stmt)->cond);
            }
            EMIT_STRING("; ");
            if (((ast_loop_t*)stmt)->post != NULL){
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
        default:
            report_simple_err("encountered AST kind %s in emit_stmt()",
                                ast_get_kind_str(stmt->kind));
    }
}

static void emit_proc(ast_decl_t *parent_decl) {
    ast_proc_t   *proc;
    u32           proc_type;
    u32           ret_type;
    ast_decl_t  **mod_it;
    u32           num_param_types;
    u32           i;
    u32           param_type;
    ast_t       **param_p_it;
    ast_param_t  *param;

    proc      = (ast_proc_t*)parent_decl->val_expr;
    proc_type = ASTP(proc)->type;
    ret_type  = get_ret_type(proc_type);

    if (ASTP(proc)->flags & AST_FLAG_IS_EXTERN) {
        return;
    }

    emit_type_declarator(ret_type); EMIT_STRING(" ");

    array_traverse(mod_stack, mod_it) {
        EMIT_STRING("__");
        EMIT_STRING_ID((*mod_it)->name);
    }
    if (array_len(mod_stack) > 0) { EMIT_STRING("_"); }
    EMIT_STRING_ID(parent_decl->name);

    num_param_types = get_num_param_types(proc_type);
    if (num_param_types == 0) {
        EMIT_STRING("(void)");
    } else {
        EMIT_STRING("(");
        i = 0;
        array_traverse(proc->params, param_p_it) {
            param_type = get_param_type(proc_type, i);
            emit_type_declarator(param_type);
            EMIT_STRING(" ");
            param = (ast_param_t*)*param_p_it;
            EMIT_STRING_ID(param->name);
            if (i < num_param_types - 1) {
                EMIT_STRING(", ");
            }
            i += 1;
        }
        EMIT_STRING(")");
    }

    EMIT_STRING("\n");
    emit_block((ast_block_t*)proc->block, 0);
    EMIT_STRING("\n");
}

static void emit_procs_module(ast_decl_t *parent_decl) {
    ast_module_t  *module;
    ast_t        **nodep;
    ast_t         *node;

    module = (ast_module_t*)parent_decl->val_expr;

    array_push(mod_stack, parent_decl);

    array_traverse(module->children, nodep) {
        node = *nodep;

        if (node->kind == AST_DECL_MODULE) {
            emit_procs_module((ast_decl_t*)node);
        } else if (node->kind == AST_DECL_PROC) {
            emit_proc((ast_decl_t*)node);
        }
    }

    array_pop(mod_stack);
}

static void emit_procs_global(void) {
    ast_t **rootp;
    ast_t  *root;

    array_traverse(roots, rootp) {
        root = *rootp;

        if (root->kind == AST_DECL_MODULE) {
            emit_procs_module((ast_decl_t*)root);
        } else if (root->kind == AST_DECL_PROC) {
            emit_proc((ast_decl_t*)root);
        }
    }
}

void do_c_backend(void) {
    int  err;
    char exe_name[4096];
    char cmd_buff[4096];

    mod_stack = array_make(ast_decl_t*);

    emit_prelude();
    EMIT_STRING("\n");
    emit_proc_pre_decls_global();
    EMIT_STRING("\n");
    emit_procs_global();

    fflush(output_file);

    strcpy(exe_name, options.output_name);
    if (exe_name[strlen(exe_name) - 1] == 'c'
    &&  exe_name[strlen(exe_name) - 2] == '.') {
        exe_name[strlen(exe_name) - 2] = 0;
    }

    sprintf(cmd_buff, "cc -o %s %s -O3 -nostdlib -ffreestanding -Wl,-e,_%s",
            exe_name, options.output_name, get_string(program_entry->name));

#ifdef __APPLE__
    strcat(cmd_buff, " -lSystem");
#endif

    err = system(cmd_buff);

    if (err != 0) {
        report_simple_err("c backend failed");
    }
}
