#include "ast.h"
#include "scope.h"
#include "ui.h"
#include "type.h"
#include "parse.h"

int ast_kind_is_assign(int kind) {
    return
#define X(k) kind == (k) ||
    X_AST_ASSIGNS
#undef X
    0;
}

int ast_kind_can_be_symbol_origin(int kind) {
    return    kind == AST_PROC_PARAM
           || kind == AST_POLYMORPH_TYPE_NAME
           || kind == AST_BUILTIN
           || ast_kind_is_assign(kind);
}

static const char *ast_kind_to_name[] = {
#define X(kind) #kind,
X_AST
#undef X
};

const char *ast_get_kind_str(int kind) {
    return ast_kind_to_name[kind] + 4;
}

static void redecl_error(string_id name, ast_t *bad, ast_t *existing) {
    report_range_err_no_exit(&bad->loc, "redeclaration of '%s'", get_string(name));
    if (existing->kind == AST_BUILTIN) {
        report_simple_info("'%s' is a compiler builtin", get_string(name));
    } else {
        report_range_info(&existing->loc, "competing declaration here:");
    }
}

static void undeclared_error(string_id name, ast_t *node) {
    report_range_err(&node->loc, "use of undeclared identifier '%s'", get_string(name));
}

static void type_builtin(ast_builtin_t *node, scope_t *scope) {
}

static void typecheck_assign(ast_assign_t *assign, ast_t *origin, scope_t *scope) {
    ASSERT(ASTP(assign)->type != TY_UNKNOWN, "right hand side of assignment doesn't have a type");

    if (origin->type == TY_UNKNOWN) {
        origin->type = ASTP(assign)->type;
    } else if (assign->val->type != origin->type) {
        report_range_err(&ASTP(assign)->loc,
                         "'%s' has type %s, but you're trying to assign it a value of type %s",
                         get_string(assign->name),
                         get_string(get_type_string_id(origin->type)),
                         get_string(get_type_string_id(assign->val->type)));
        return;
    }
}

static void check_assign(ast_assign_t *assign, scope_t *scope) {
    ast_t      *existing_node;
    const char *which;

    ASSERT(assign->val != NULL, "assignment has no value");

    if (assign->name == UNDERSCORE_ID) {
        check_node(assign->val, scope, assign);
        ASTP(assign)->type = assign->val->type;
        return;
    }

    if (!(ASTP(assign)->flags & AST_FLAG_ORIGIN)) {
        existing_node = search_up_scopes_stop_at_module(scope, assign->name);
        if (existing_node == NULL) {
            undeclared_error(assign->name, ASTP(assign));
            return;
        }
        ASSERT(ast_kind_can_be_symbol_origin(existing_node->kind), "kind isn't allowed to be an origin");
        ASSERT(ASTP(assign) != existing_node, "assign search found itself");
#ifdef SIMON_DO_ASSERTIONS
        if (ast_kind_is_assign(existing_node->kind)) {
            ASSERT(existing_node->flags & AST_FLAG_ORIGIN, "existing node isn't an origin");
        }
#endif
    }

    check_node(assign->val, scope, assign);
    ASTP(assign)->type = assign->val->type;

    if (scope->in_proc) {
        if (assign->val->type == TY_PROC) {
            report_range_err(&ASTP(assign)->loc, "procedures may not be defined within another procedure");
            return;
        }

        if (!(ASTP(assign)->flags & AST_FLAG_ORIGIN)) {
            if (type_has_compile_time_only_values(existing_node->type)) {
                switch (existing_node->type) {
                    case TY_MODULE: which = "module";    break;
                    case TY_MACRO:  which = "macro";     break;
                    case TY_TYPE:   which = "type";      break;
                    case TY_PROC:   which = "procedure"; break;
                    default:
                        which = "???";
                        ASSERT(0, "unhandled type");
                }
                report_range_err_no_exit(&ASTP(assign)->loc,
                                         "can't reassign to compile-time values ('%s' is a %s)",
                                         get_string(assign->name), which);
                report_range_info(&existing_node->loc, "original definition is here:");
                return;
            }

            typecheck_assign(assign, existing_node, scope);
        }
    } else if (!(ASTP(assign)->flags & AST_FLAG_ORIGIN)) {
        redecl_error(assign->name, ASTP(assign), existing_node);
        return;
    }
}

static void check_proc(ast_proc_t *proc, scope_t *scope, ast_assign_t *parent_assign) {
    ast_t   **it;
    scope_t  *new_scope;
    u32       n_params;
    u32      *param_types;
    int       i;
    u32       ret_type;

    ASSERT(!(ASTP(proc)->flags & AST_FLAG_POLYMORPH), "TODO");

    new_scope = get_subscope_from_node(scope, ASTP(proc));

    n_params    = array_len(proc->params);
    param_types = alloca(sizeof(u32) * n_params);
    i           = 0;
    array_traverse(proc->params, it) {
        check_node(*it, new_scope, NULL);
        param_types[i] = (*it)->type;
        i += 1;
    }

    if (proc->ret_type_expr != NULL) {
        check_node(proc->ret_type_expr, new_scope, NULL);
        if (proc->ret_type_expr->type != TY_TYPE) {
            report_range_err_no_exit(&proc->ret_type_expr->loc,
                                "expression must be a type since it declares the return type of procedure '%s'",
                                get_string(parent_assign->name));
            report_simple_info("got '%s' instead", get_string(get_type_string_id(proc->ret_type_expr->type)));
            return;
        }

        ret_type = proc->ret_type_expr->value.t;
    } else {
        ret_type = TY_NOT_TYPED;
    }

    ASTP(proc)->type = get_proc_type(n_params, param_types, ret_type);

    check_node(proc->block, new_scope, NULL);
}

static void check_proc_param(ast_proc_param_t *param, scope_t *scope) {
    ast_t                     *existing_node;
    ast_polymorph_type_name_t *poly;

    ASSERT(scope->parent != NULL, "proc param in global scope??");

    existing_node = search_up_scopes_stop_at_module(scope->parent, param->name);

    if (existing_node != NULL) {
        redecl_error(param->name, ASTP(param), existing_node);
        return;
    }

    if (param->type_expr_or_polymorph_type_name->kind == AST_POLYMORPH_TYPE_NAME) {
        poly = (ast_polymorph_type_name_t*)param->type_expr_or_polymorph_type_name;

        existing_node = search_up_scopes_stop_at_module(scope->parent, poly->name);

        if (existing_node != NULL) {
            redecl_error(poly->name, ASTP(poly), existing_node);
            return;
        }

        check_node(param->type_expr_or_polymorph_type_name, scope, NULL);
    } else {
        check_node(param->type_expr_or_polymorph_type_name, scope, NULL);
        if (param->type_expr_or_polymorph_type_name->type != TY_TYPE) {
            report_range_err_no_exit(&param->type_expr_or_polymorph_type_name->loc,
                             "expression must be a type since it declares the type of parameter '%s'",
                             get_string(param->name));
            report_simple_info("got '%s' instead", get_string(get_type_string_id(param->type_expr_or_polymorph_type_name->type)));
            return;
        }

        ASTP(param)->type = param->type_expr_or_polymorph_type_name->value.t;
    }
}

static void check_int(ast_int_t *integer, scope_t *scope) {
    ASTP(integer)->type = TY_S64;
}

static void check_bool(ast_bool_t *b, scope_t *scope) {
    ASTP(b)->type = TY_BOOL;
}

static void check_call(ast_bin_expr_t *expr, scope_t *scope) {
    u32             proc_ty;
    u32             n_params;
    ast_arg_list_t *arg_list;
    u32             n_args;
    arg_t          *arg_p;

    proc_ty = expr->left->type;

    if (type_kind(proc_ty) != TY_PROC) {
        report_range_err_no_exit(&ASTP(expr)->loc,
                                 "attempting to call a value that is not a procedure");
        report_range_info(&expr->left->loc,
                          "value has type '%s'",
                          get_string(get_type_string_id(proc_ty)));
        return;
    }

    n_params = get_num_param_types(proc_ty);
    arg_list = (ast_arg_list_t*)expr->right;
    n_args   = array_len(arg_list->args);

    if (n_args < n_params) {
        report_loc_err(expr->right->loc.end, "too few");
        return;
    } else if (n_args > n_params) {
        arg_p = array_item(arg_list->args, n_params);
        report_range_err(&arg_p->expr->loc, "too many");
        return;
    }

    ASTP(expr)->type = get_ret_type(expr->left->type);
}

static void check_bin_expr(ast_bin_expr_t *expr, scope_t *scope) {
    check_node(expr->left, scope, NULL);
    check_node(expr->right, scope, NULL);

    if (expr->op == OP_CALL) {
        check_call(expr, scope);
    }
}

static void check_ident(ast_ident_t *ident, scope_t *scope) {
    ast_t   *resolved_node;
    scope_t *resolved_ident_scope;

    if (ident->str_rep == UNDERSCORE_ID) {
        report_range_err_no_exit(&ASTP(ident)->loc, "'_' can be assigned to, but not referenced");
        report_simple_info("'_' acts as an assignment sink for values meant to be unreferenceable");
        return;
    }

    resolved_node = ident->resolved_node;

    if (resolved_node == NULL) {
        resolved_node = search_up_scopes_return_scope(scope, ident->str_rep, &resolved_ident_scope);

        if (resolved_node == NULL) {
            report_range_err(&ASTP(ident)->loc,
                             "use of undeclared identifier '%s'", get_string(ident->str_rep));
            return;
        }

        check_node(resolved_node, resolved_ident_scope, NULL);

        ASTP(ident)->type  = resolved_node->type;
        ASTP(ident)->value = resolved_node->value;
    }
}

static void check_struct(ast_struct_t *st, scope_t *scope, ast_assign_t *parent_assign) {
    ASTP(st)->type = get_struct_type(st, parent_assign->name, scope, 0);
}

static void check_arg_list(ast_arg_list_t *arg_list, scope_t *scope) {
    arg_t *arg;

    ASTP(arg_list)->type = TY_NOT_TYPED;

    array_traverse(arg_list->args, arg) {
        check_node(arg->expr, scope, NULL);
    }
}

void check_node(ast_t *node, scope_t *scope, ast_assign_t *parent_assign) {
    ast_t **it;

    if (node->flags & AST_FLAG_CHECKED) { return; }
    node->flags |= AST_FLAG_CHECKED;

    switch (node->kind) {
#define X(_kind) case _kind:
        X_AST_ASSIGNS
#undef X
            check_assign((ast_assign_t*)node, scope);
            break;

        case AST_MODULE:
            array_traverse(((ast_module_t*)node)->children, it) {
                check_node(*it, get_subscope_from_node(scope, node), NULL);
            }
            break;
        case AST_PROC:
            check_proc((ast_proc_t*)node, scope, parent_assign);
            break;

        case AST_PROC_PARAM:
            check_proc_param((ast_proc_param_t*)node, scope);
            break;

        case AST_STRUCT:
            check_struct((ast_struct_t*)node, scope, parent_assign);
            break;

        case AST_BLOCK:
            node->type = TY_NOT_TYPED;
            array_traverse(((ast_block_t*)node)->stmts, it) {
                check_node(*it, scope, NULL);
            }
            break;

        case AST_BIN_EXPR:
            check_bin_expr((ast_bin_expr_t*)node, scope);
            break;

        case AST_BOOL:
            check_bool((ast_bool_t*)node, scope);
            break;
        case AST_INT:
            check_int((ast_int_t*)node, scope);
            break;
        case AST_STRING:
            node->type = get_ptr_type(TY_CHAR, 0);
            break;

        case AST_IDENT:
            check_ident((ast_ident_t*)node, scope);
            break;

        case AST_ARG_LIST:
            check_arg_list((ast_arg_list_t*)node, scope);
            break;

        case AST_BUILTIN:
            type_builtin((ast_builtin_t*)node, scope);
            break;

        default:;
#ifdef SIMON_DO_ASSERTIONS
            report_range_err_no_exit(&node->loc, "INTERNAL ERROR: AST_%s unhandled in check_node()", ast_get_kind_str(node->kind));
            ASSERT(0, "unhandled AST node kind in check_node()");
#endif
    }

    ASSERT(node->type != TY_UNKNOWN, "did not resolve type");
}
