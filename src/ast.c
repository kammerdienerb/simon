#include "ast.h"
#include "scope.h"
#include "ui.h"
#include "type.h"
#include "parse.h"
#include "array.h"

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

static array_t get_declaration_path(ast_ident_t *ident) {
    array_t    path;
    ast_assign_t *assign;
    ast_t        *assign_ast;

    path = array_make(ast_t*);

    while (ident->resolved_node != NULL) {
        ASSERT(ast_kind_is_assign(ident->resolved_node->kind),
               "resolved_node is not an assignment");

        assign = (ast_assign_t*)ident->resolved_node;

        ASSERT(assign->val != NULL, "assign has no val");

        assign_ast = ASTP(assign);
        array_push(path, assign_ast);

        if (ASTP(assign)->kind != AST_ASSIGN_EXPR
        ||  assign->val->kind  != AST_IDENT) {

            break;
        }

        ident = (ast_ident_t*)assign->val;
    }

    return path;
}

static void _report_declaration_path(int should_exit, array_t path) {
    int            i;
    ast_t        **it;
    ast_assign_t  *assign;

    i = 0;
    array_rtraverse(path, it) {
        assign = (ast_assign_t*)*it;

        if (i == 0) {
            if (i == array_len(path) - 1 && should_exit) {
                report_range_info_no_context(&ASTP(assign)->loc,
                                             "'%s' originally assigned here:",
                                             get_string(assign->name));
            } else {
                report_range_info_no_context_no_exit(&ASTP(assign)->loc,
                                                     "'%s' originally assigned here:",
                                                     get_string(assign->name));
            }
        } else if (i == array_len(path) - 1 && should_exit) {
            report_range_info_no_context(&ASTP(assign)->loc,
                                         "then to '%s' here:",
                                         get_string(assign->name));
        } else {
            report_range_info_no_context_no_exit(&((*it)->loc),
                                                 "then to '%s' here:",
                                                 get_string(assign->name));
        }
        i += 1;
    }
}

#define report_declaration_path(_path)         (_report_declaration_path(1, (_path)))
#define report_declaration_path_no_exit(_path) (_report_declaration_path(0, (_path)))


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

    if (assign->val->type == TY_NOT_TYPED) {
        report_range_err_no_exit(&assign->val->loc,
                                 "invalid expression in assignment of '%s'",
                                 get_string(assign->name));
        report_simple_info("the expression does not have a type or value");
        return;
    }

    ASTP(assign)->type  = assign->val->type;
    ASTP(assign)->value = assign->val->value;

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
    ASSERT(new_scope != NULL, "didn't get scope");

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
            report_simple_info("got %s instead", get_string(get_type_string_id(proc->ret_type_expr->type)));
            return;
        }

        ret_type = proc->ret_type_expr->value.t;
    } else {
        ret_type = TY_NOT_TYPED;
    }

    ASTP(proc)->type = get_proc_type(n_params, param_types, ret_type);


    /* @bad?, @refactor
    ** We have to bubble this type up to the assignment so that identifier lookups
    ** that occur before we return from this routine can get the right type.
    ** I'm not sure if this should just be done everywhere like this (shouldn't
    ** be _that_ many spots), or if something a little smarter should be done.
    */
    ASTP(parent_assign)->type  = ASTP(proc)->type;
    ASTP(parent_assign)->value = ASTP(proc)->value;

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
            report_simple_info("got %s instead", get_string(get_type_string_id(param->type_expr_or_polymorph_type_name->type)));
            return;
        }

        ASTP(param)->type = param->type_expr_or_polymorph_type_name->value.t;
    }

    if (ASTP(param)->flags & AST_FLAG_VARARGS) {
        ASTP(param)->type = get_vargs_type(ASTP(param)->type);
    }
}

static void check_int(ast_int_t *integer, scope_t *scope) {
    ASTP(integer)->type = TY_S64;
}

static void check_bool(ast_bool_t *b, scope_t *scope) {
    ASTP(b)->type = TY_BOOL;
}

static ast_assign_t * try_get_decl_and_path(ast_ident_t *ident, array_t *path) {
    if (ident->resolved_node == NULL) { return NULL; }

    ASSERT(ast_kind_is_assign(ident->resolved_node->kind),
           "resolved_node is not an assignment");

    *path = get_declaration_path(ident);

    if (array_len(*path) == 0) {
        return (ast_assign_t*)ident->resolved_node;
    }

    return (ast_assign_t*)*(ast_t**)array_last(*path);
}

static void check_call(ast_bin_expr_t *expr, scope_t *scope) {
    u32             proc_ty;
    ast_ident_t    *left_ident;
    u32             n_params;
    ast_arg_list_t *arg_list;
    u32             n_args;
    arg_t          *arg_p;
    u32             i;
    u32             param_type;
    u32             arg_type;
    u32             varg_ty;
    u32             last_ty;
    ast_assign_t   *proc_assign;
    array_t         path;
    ast_proc_t     *proc;
    ast_t          *parm_decl;

    proc_ty = expr->left->type;

    if (type_kind(proc_ty) != TY_PROC) {
        report_range_err_no_exit(&ASTP(expr)->loc,
                                 "attempting to call a value that is not a procedure");
        report_range_info(&expr->left->loc,
                          "value has type %s",
                          get_string(get_type_string_id(proc_ty)));
        return;
    }

    left_ident  = NULL;
    proc_assign = NULL;
    proc        = NULL;
    if (expr->left->kind == AST_IDENT) {
        left_ident = (ast_ident_t*)expr->left;
    }

    n_params = get_num_param_types(proc_ty);
    arg_list = (ast_arg_list_t*)expr->right;
    n_args   = array_len(arg_list->args);

    varg_ty = TY_NONE;
    if (n_params >= 1) {
        last_ty = get_param_type(proc_ty, n_params - 1);
        if (type_kind(last_ty) == TY_VARGS) {
            varg_ty   = get_under_type(last_ty);
            n_params -= 1;
        }
    }

    if (varg_ty != TY_NONE) {
        if (n_args < n_params) { goto too_few; }
    } else {
        if (n_args < n_params) {
too_few:
            report_loc_err_no_exit(expr->right->loc.end,
                                   "too few arguments in procedure call");

            if (left_ident == NULL) {
                report_simple_info_no_exit("expected %s %d, but got %d",
                                           varg_ty != TY_NONE ? "at least" : "", n_params, n_args);
                report_simple_info("indirect call with procedure type %s",
                                   get_string(get_type_string_id(proc_ty)));
            } else {
                proc_assign = try_get_decl_and_path(left_ident, &path);
                proc        = (ast_proc_t*)proc_assign->val;

                if (left_ident->resolved_node == ASTP(proc_assign)) {
                    report_simple_info_no_exit("expected %s %d, but got %d",
                                               varg_ty != TY_NONE ? "at least" : "", n_params, n_args);
                    report_range_info_no_context(&ASTP(proc_assign)->loc,
                                                 "'%s' defined here:", get_string(proc_assign->name));
                } else {
                    report_simple_info_no_exit("expected %s %d, but got %d",
                                               varg_ty != TY_NONE ? "at least" : "", n_params, n_args);
                    report_declaration_path(path);
                }
            }
            return;
        } else if (n_args > n_params) {
            arg_p = array_item(arg_list->args, n_params);
            report_range_err_no_exit(&arg_p->expr->loc,
                                     "too many arguments in procedure call");

            if (left_ident == NULL) {
                report_simple_info_no_exit("expected %d, but got %d", n_params, n_args);
                report_simple_info("indirect call with procedure type %s",
                                   get_string(get_type_string_id(proc_ty)));
            } else {
                proc_assign = try_get_decl_and_path(left_ident, &path);
                proc        = (ast_proc_t*)proc_assign->val;

                if (left_ident->resolved_node == ASTP(proc_assign)) {
                    report_simple_info_no_exit("expected %d, but got %d", n_params, n_args);
                    report_range_info_no_context(&ASTP(proc_assign)->loc,
                                                 "'%s' defined here:", get_string(proc_assign->name));
                } else {
                    report_simple_info_no_exit("expected %d, but got %d", n_params, n_args);
                    report_declaration_path(path);
                }
            }
            return;
        }
    }

    for (i = 0; i < n_params; i += 1) {
        param_type = get_param_type(proc_ty, i);
        arg_p      = array_item(arg_list->args, i);
        arg_type   = arg_p->expr->type;

        if (arg_type != param_type) {
            if (left_ident == NULL) {
                report_range_err_no_exit(&arg_p->expr->loc,
                                         "incorrect argument type: expected %s, but got %s",
                                         get_string(get_type_string_id(param_type)),
                                         get_string(get_type_string_id(arg_type)));
                report_simple_info("indirect call with procedure type %s",
                                   get_string(get_type_string_id(proc_ty)));
            } else {
                report_range_err_no_exit(&arg_p->expr->loc,
                                         "incorrect argument type: expected %s, but got %s",
                                         get_string(get_type_string_id(param_type)),
                                         get_string(get_type_string_id(arg_type)));

                proc_assign = try_get_decl_and_path(left_ident, &path);
                proc        = (ast_proc_t*)proc_assign->val;
                parm_decl   = *(ast_t**)array_item(proc->params, i);
                if (left_ident->resolved_node == ASTP(proc_assign)) {
                    report_range_info_no_context(&parm_decl->loc, "parameter declaration here:");
                } else {
                    report_range_info_no_context_no_exit(&parm_decl->loc, "parameter declaration here:");
                    report_declaration_path(path);
                }
            }

            return;
        }

        if (arg_p->name != STRING_ID_NULL) {
            if (left_ident == NULL) {
                report_range_err(&arg_p->expr->loc, "named arguments are not allowed in indirect calls");
                return;
            } else {
                if (proc == NULL) {
                    proc_assign = try_get_decl_and_path(left_ident, &path);
                    proc        = (ast_proc_t*)proc_assign->val;
                }

                parm_decl = *(ast_t**)array_item(proc->params, i);
                if (arg_p->name != ((ast_proc_param_t*)parm_decl)->name) {
                    report_range_err_no_exit(&arg_p->expr->loc,
                                                "argument name '%s' does not match parameter name '%s'",
                                                get_string(arg_p->name),
                                                get_string(((ast_proc_param_t*)parm_decl)->name));
                    if (left_ident->resolved_node == ASTP(proc_assign)) {
                        report_range_info_no_context(&parm_decl->loc, "parameter declaration here:");
                    } else {
                        report_range_info_no_context_no_exit(&parm_decl->loc, "parameter declaration here:");
                        report_declaration_path(path);
                    }
                }
            }
        }
    }

    if (varg_ty != TY_NONE) {
        for (i = n_params; i < n_args; i += 1) {
            arg_p    = array_item(arg_list->args, i);
            arg_type = arg_p->expr->type;

            if (arg_type != varg_ty) {
                report_range_err_no_exit(&arg_p->expr->loc,
                                         "incorrect argument type: expected %s, but got %s",
                                         get_string(get_type_string_id(varg_ty)),
                                         get_string(get_type_string_id(arg_type)));
                report_simple_info_no_exit("argument belongs to a variadic parameter list");

                if (left_ident == NULL) {
                    report_simple_info("indirect call with procedure type %s",
                                       get_string(get_type_string_id(proc_ty)));
                } else {
                    proc_assign = try_get_decl_and_path(left_ident, &path);
                    proc        = (ast_proc_t*)proc_assign->val;
                    parm_decl   = *(ast_t**)array_last(proc->params);
                    if (left_ident->resolved_node == ASTP(proc_assign)) {
                        report_range_info_no_context(&parm_decl->loc, "variadic parameter list declared here:");
                    } else if (left_ident->resolved_node != ASTP(proc_assign)) {
                        report_range_info_no_context_no_exit(&parm_decl->loc, "variadic parameter list declared here:");
                        report_declaration_path(path);
                    }
                }
            }

            if (arg_p->name != STRING_ID_NULL) {
                if (i == n_params) {
                    if (left_ident == NULL) {
                        report_range_err(&arg_p->expr->loc, "named arguments are not allowed in indirect calls");
                        return;
                    } else {
                        if (proc == NULL) {
                            proc_assign = try_get_decl_and_path(left_ident, &path);
                            proc        = (ast_proc_t*)proc_assign->val;
                        }

                        parm_decl = *(ast_t**)array_last(proc->params);
                        if (arg_p->name != ((ast_proc_param_t*)parm_decl)->name) {
                            report_range_err_no_exit(&arg_p->expr->loc,
                                                        "argument name '%s' does not match variadic parameter list name '%s'",
                                                        get_string(arg_p->name),
                                                        get_string(((ast_proc_param_t*)parm_decl)->name));
                            if (left_ident->resolved_node == ASTP(proc_assign)) {
                                report_range_info_no_context(&parm_decl->loc, "parameter declaration here:");
                            } else {
                                report_range_info_no_context_no_exit(&parm_decl->loc, "parameter declaration here:");
                                report_declaration_path(path);
                            }
                        }
                    }
                } else {
                    report_range_err_no_exit(&arg_p->expr->loc, "only the first argument for a variadic parameter list may be named");
                    if (left_ident->resolved_node == ASTP(proc_assign)) {
                        report_range_info_no_context(&parm_decl->loc, "parameter declaration here:");
                    } else {
                        report_range_info_no_context_no_exit(&parm_decl->loc, "parameter declaration here:");
                        report_declaration_path(path);
                    }
                    return;
                }
            }
        }
    }

    if (proc != NULL) { array_free(path); }

    ASTP(expr)->type = get_ret_type(expr->left->type);
}

static void check_ident(ast_ident_t *ident, scope_t *scope) {
    scope_t *resolved_node_scope;

    if (ident->str_rep == UNDERSCORE_ID) {
        report_range_err_no_exit(&ASTP(ident)->loc, "'_' can be assigned to, but not referenced");
        report_simple_info("'_' acts as an assignment sink for values meant to be unreferenceable");
        return;
    }

    ASSERT(ident->resolved_node == NULL, "ident has already been resolved");

    ident->resolved_node = search_up_scopes_return_scope(scope, ident->str_rep, &resolved_node_scope);

    if (ident->resolved_node == NULL) {
        report_range_err(&ASTP(ident)->loc,
                         "use of undeclared identifier '%s'", get_string(ident->str_rep));
        return;
    }

    check_node(ident->resolved_node, resolved_node_scope, NULL);

    ASTP(ident)->type  = ident->resolved_node->type;
    ASTP(ident)->value = ident->resolved_node->value;
}

static void check_module_dot(ast_bin_expr_t *expr, scope_t *scope) {
    /* @note:
    ** expr->left must have been checked by this point.
    ** Also, it can't be anything other than an identifier since there are
    ** no operators that do anything to modules other than assignment.
    */

    ast_ident_t  *left_ident;
    ast_assign_t *resolved_assign;
    array_t       path;
    scope_t      *module_scope;
    ast_ident_t  *right_ident;
    ast_t        *found_node;
    ast_ident_t  *new_ident;
    const char   *lname;
    u32           llen;
    const char   *rname;
    u32           rlen;
    u32           new_name_len;
    char         *new_name_buff;


    ASSERT(expr->left->kind == AST_IDENT,
           "expr->left must be an identifier in check_module_dot()");

    left_ident = (ast_ident_t*)expr->left;

    resolved_assign = try_get_decl_and_path(left_ident, &path);

    ASSERT(resolved_assign != NULL, "did not get resolved assign");

    ASSERT(ASTP(resolved_assign)->kind == AST_ASSIGN_MODULE,
           "resolved_assign is not a module assignment");

    module_scope = get_subscope_from_node(resolved_assign->scope,
                                          resolved_assign->val);

    ASSERT(module_scope != NULL, "did not find module scope");

    right_ident = (ast_ident_t*)expr->right;

    found_node = find_in_scope(module_scope, right_ident->str_rep);

    if (found_node == NULL) {
        if (left_ident->resolved_node == ASTP(resolved_assign)) {
            report_range_err(&expr->right->loc,
                             "nothing named '%s' in module '%s'",
                             get_string(right_ident->str_rep),
                             get_string(left_ident->str_rep));
        } else {
            report_range_err_no_exit(&expr->right->loc,
                                     "nothing named '%s' in module '%s'",
                                     get_string(right_ident->str_rep),
                                     get_string(left_ident->str_rep));
            report_declaration_path(path);
        }
        return;
    }

    array_free(path);

    check_node(found_node, module_scope, NULL);

    /* @note !!!
    ** We're overwriting the data of expr here..
    ** Be careful with this.
    ** There's an assertion in do_sanity_checks() that ensures that an
    ** ast_ident_t will fit in an ast_bin_expr_t.
    */
    new_ident = (ast_ident_t*)expr;

    ASTP(new_ident)->kind  = AST_IDENT;
    ASTP(new_ident)->type  = found_node->type;
    ASTP(new_ident)->value = found_node->value;
    /* @note -- are there any flags we need to set/clear? */

    lname         = get_string(left_ident->str_rep);
    llen          = strlen(lname);
    rname         = get_string(right_ident->str_rep);
    rlen          = strlen(rname);
    new_name_len  = llen + 1 + rlen;
    new_name_buff = alloca(new_name_len);
    strcpy(new_name_buff, lname);
    new_name_buff[llen] = '.';
    strcpy(new_name_buff + llen + 1, rname);

    new_ident->str_rep       = get_string_id_n(new_name_buff, new_name_len);
    new_ident->resolved_node = found_node;
}

static void check_dot(ast_bin_expr_t *expr, scope_t *scope) {
    if (expr->right->kind != AST_IDENT) {
        report_range_err(&expr->right->loc,
                         "the '.' operator must be followed by an identifier");
        return;
    }

    switch (type_kind(expr->left->type)) {
        case TY_MODULE:
            check_module_dot(expr, scope);
            break;
        case TY_STRUCT:
            ASSERT(0, "unimplemented");
            break;
        case TY_PTR:
            ASSERT(0, "unimplemented");
            break;
        default:
            report_range_err(&ASTP(expr)->loc,
                             "the '.' operator does not apply to left-hand-side operand type %s",
                             get_string(get_type_string_id(expr->left->type)));
            return;
    }
}

static void binop_bad_type_error(ast_bin_expr_t *expr) {
    u32 lt;
    u32 rt;

    lt = expr->left->type;
    rt = expr->right->type;

    report_range_err(&ASTP(expr)->loc,
                     "operator '%s' does not apply to types %s and %s",
                     OP_STR(expr->op),
                     get_string(get_type_string_id(lt)),
                     get_string(get_type_string_id(rt)));
}

static void check_add(ast_bin_expr_t *expr) {
    u32 t1;
    u32 t2;
    u32 tk1;
    u32 tk2;
    u64 tk_both;

    t1  = expr->left->type;
    t2  = expr->right->type;
    tk1 = type_kind(t1);
    tk2 = type_kind(t2);

    /*
    ** tk_both is the combo of tk1 and tk2 in sorted order so that we can do the
    ** below checks without considering right/left.
    */
    tk_both = tk1 < tk2
                ? (((u64)tk1) << 32ULL) + tk2
                : (((u64)tk2) << 32ULL) + tk1;

    switch (tk_both) {
        case TKINDPAIR_INT_INT:
            /* @todo check for width incompat */
            /* @todo How to handle this case? Do we promote the type? */
            ASTP(expr)->type = t1;
            break;
        case TKINDPAIR_PTR_INT:
            ASTP(expr)->type = (tk1 == TY_PTR ? t1 : t2);
            break;
        default:
            binop_bad_type_error(expr);
            return;
    }
}

static void check_sub(ast_bin_expr_t *expr) {
    u32 t1;
    u32 t2;
    u32 tk1;
    u32 tk2;

    t1  = expr->left->type;
    t2  = expr->right->type;
    tk1 = type_kind(t1);
    tk2 = type_kind(t2);

    if (tk1 == TY_GENERIC_INT && tk2 == TY_GENERIC_INT) {
        /* @todo check for width incompat */
        /* @todo How to handle this case? Do we promote the type? */
        ASTP(expr)->type = t1;
    } else if (tk1 == TY_PTR && tk2 == TY_GENERIC_INT) {
        ASTP(expr)->type = t1;
    } else {
        binop_bad_type_error(expr);
        return;
    }
}

static void operand_not_typed_error(ast_t *expr, ast_t *operand) {
    int op;

    op = expr->kind == AST_UNARY_EXPR
            ? ((ast_unary_expr_t*)expr)->op
            : ((ast_bin_expr_t*)expr)->op;

    report_range_err_no_exit(&operand->loc,
                             "invalid operand to %s '%s' expression",
                             OP_IS_UNARY(op) ? "unary" : "binary",
                             OP_STR(op));
    report_simple_info("the expression does not have a type or value");
}

static void check_bin_expr(ast_bin_expr_t *expr, scope_t *scope) {
    /*
    ** OP_DOT is special because the right operand could fail in symbol resolution
    ** if checked alone.
    */

    check_node(expr->left, scope, NULL);

    if (expr->left->type == TY_NOT_TYPED) {
        operand_not_typed_error(ASTP(expr), expr->left);
        return;
    }

    if (expr->op == OP_DOT) {
        check_dot(expr, scope);
        return;
    }

    check_node(expr->right, scope, NULL);

    if (expr->left->type == TY_NOT_TYPED) {
        operand_not_typed_error(ASTP(expr), expr->right);
        return;
    }

    switch (expr->op) {
        case OP_DOT: /* Handled above. */ break;

        case OP_CALL:
            check_call(expr, scope);
            break;

        case OP_SUBSCRIPT:
            if (type_kind(expr->left->type) != TY_PTR) {
                report_range_err_no_exit(&ASTP(expr)->loc,
                                         "left-hand-side operand of '[]' operator must be a pointer type");
                report_range_info_no_context(&expr->left->loc,
                                             "operand has type %s",
                                             get_string(get_type_string_id(expr->left->type)));
                return;
            }
            if (!type_kind_is_int(expr->right->type)) {
                report_range_err_no_exit(&ASTP(expr)->loc,
                                         "right-hand-side operand of '[]' operator must be an integer type");
                report_range_info_no_context(&expr->right->loc,
                                         "operand has type %s",
                                         get_string(get_type_string_id(expr->right->type)));
            }
            ASTP(expr)->type = get_under_type(expr->left->type);
            break;

        case OP_PLUS:
            check_add(expr);
            break;
        case OP_MINUS:
            check_sub(expr);
            break;

        case OP_EQU:
        case OP_NEQ:
        case OP_LSS:
        case OP_LEQ:
        case OP_GTR:
        case OP_GEQ:
            ASTP(expr)->type = TY_BOOL;
            break;

        case OP_PLUS_ASSIGN:
            check_add(expr);
            ASSERT(ASTP(expr)->type == expr->left->type, "type should not change in assignment");
            break;
        case OP_MINUS_ASSIGN:
            check_sub(expr);
            ASSERT(ASTP(expr)->type == expr->left->type, "type should not change in assignment");
            break;

        default:
            ASSERT(0, "unandled binary operator");
            return;
    }
}

static void check_unary_expr(ast_unary_expr_t *expr, scope_t *scope) {
    check_node(expr->child, scope, NULL);

    if (expr->child->type == TY_NOT_TYPED) {
        operand_not_typed_error(ASTP(expr), expr->child);
        return;
    }

    switch (expr->op) {
        case OP_ADDR:
            if (expr->child->type == TY_TYPE) {
                ASTP(expr)->type    = TY_TYPE;
                ASTP(expr)->value.t = get_ptr_type(expr->child->value.t);
            } else {
                ASTP(expr)->type = get_ptr_type(expr->child->type);
            }
            break;
        case OP_DEREF:
            if (expr->child->type == TY_TYPE) {
                ASTP(expr)->type    = TY_TYPE;
                if (type_kind(expr->child->value.t) != TY_PTR) {
                    report_range_err_no_exit(&ASTP(expr)->loc,
                                             "right-hand-side operand of '@' operator must be a pointer");
                    report_range_info_no_context(&expr->child->loc,
                                                 "operand is the type value %s",
                                                 get_string(get_type_string_id(expr->child->value.t)));
                }
                ASTP(expr)->value.t = get_under_type(expr->child->value.t);
            } else {
                if (type_kind(expr->child->type) != TY_PTR) {
                    report_range_err_no_exit(&ASTP(expr)->loc,
                                             "right-hand-side operand of '@' operator must be a pointer");
                    report_range_info_no_context(&expr->child->loc,
                                                 "operand has type %s",
                                                 get_string(get_type_string_id(expr->child->type)));
                }
                ASTP(expr)->type = get_under_type(expr->child->type);
            }
            break;
        default:
            ASSERT(0, "unandled unary operator");
            return;
    }
}

static void check_struct(ast_struct_t *st, scope_t *scope, ast_assign_t *parent_assign) {
    ASTP(st)->type = get_struct_type(st, parent_assign->name, scope);
}

static void check_arg_list(ast_arg_list_t *arg_list, scope_t *scope) {
    arg_t *arg;

    ASTP(arg_list)->type = TY_NOT_TYPED;

    array_traverse(arg_list->args, arg) {
        check_node(arg->expr, scope, NULL);
    }
}

static void check_loop(ast_loop_t *loop, scope_t *scope) {
    scope_t *new_scope;

    ASTP(loop)->type = TY_NOT_TYPED;

    new_scope = get_subscope_from_node(scope, ASTP(loop));
    ASSERT(new_scope != NULL, "didn't get scope");

    if (loop->init != NULL) {
        check_node(loop->init, new_scope, NULL);
    }
    if (loop->cond != NULL) {
        check_node(loop->cond, new_scope, NULL);
        if (loop->cond->type != TY_BOOL) {
            report_range_err(&loop->cond->loc, "loop condition must have type bool");
            return;
        }
    }
    if (loop->post != NULL) {
        check_node(loop->post, new_scope, NULL);
    }

    check_node(loop->block, new_scope, NULL);
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

        case AST_UNARY_EXPR:
            check_unary_expr((ast_unary_expr_t*)node, scope);
            break;

        case AST_BOOL:
            check_bool((ast_bool_t*)node, scope);
            break;
        case AST_INT:
            check_int((ast_int_t*)node, scope);
            break;
        case AST_STRING:
            node->type = get_ptr_type(TY_CHAR);
            break;

        case AST_IDENT:
            check_ident((ast_ident_t*)node, scope);
            break;

        case AST_ARG_LIST:
            check_arg_list((ast_arg_list_t*)node, scope);
            break;

        case AST_LOOP:
            check_loop((ast_loop_t*)node, scope);
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
