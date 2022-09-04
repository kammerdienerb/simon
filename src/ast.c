#include "ast.h"
#include "scope.h"
#include "ui.h"
#include "type.h"
#include "parse.h"
#include "array.h"


static array_t proc_stack;
static int     in_loop;

void init_checking(void) {
    proc_stack = array_make(ast_proc_t*);
}

int ast_kind_is_decl(int kind) {
    return
#define X(k) kind == (k) ||
    X_AST_DECLARATIONS
#undef X
    0;
}

int ast_kind_can_be_symbol_origin(int kind) {
    return    kind == AST_PARAM
           || kind == AST_POLYMORPH_TYPE_NAME
           || kind == AST_BUILTIN
           || ast_kind_is_decl(kind);
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

/* @tmp */
__attribute__((used))
static void undeclared_error(string_id name, ast_t *node) {
    report_range_err(&node->loc, "use of undeclared identifier '%s'", get_string(name));
}

static array_t get_declaration_path(ast_ident_t *ident) {
    array_t     path;
    ast_decl_t *decl;
    ast_t      *decl_ast;

    path = array_make(ast_t*);

    while (ident->resolved_node != NULL) {
        if (ident->resolved_node->kind == AST_BUILTIN) {
            array_push(path, ident->resolved_node);
            break;
        } else {
            ASSERT(ast_kind_is_decl(ident->resolved_node->kind),
                   "resolved_node is not a declaration");
        }

        decl = (ast_decl_t*)ident->resolved_node;

        ASSERT(decl->val_expr != NULL, "decl has no val");

        decl_ast = ASTP(decl);
        array_push(path, decl_ast);

        if (ASTP(decl)->kind != AST_DECL_VAR
        ||  decl->val_expr->kind  != AST_IDENT) {

            break;
        }

        ident = (ast_ident_t*)decl->val_expr;
    }

    return path;
}

static void _report_declaration_path(int should_exit, array_t path) {
    int          i;
    ast_t      **it;
    ast_decl_t  *decl;

    i = 0;
    array_rtraverse(path, it) {
        decl = (ast_decl_t*)*it;

        if (i == 0) {
            if (i == array_len(path) - 1 && should_exit) {
                if ((*it)->kind == AST_BUILTIN) {
                    report_simple_info("'%s' originally declared as a compiler builtin",
                                       get_string(((ast_builtin_t*)*it)->name));
                } else {
                    report_range_info_no_context(&ASTP(decl)->loc,
                                                 "'%s' originally declared here:",
                                                 get_string(decl->name));
                }
            } else {
                if ((*it)->kind == AST_BUILTIN) {
                    report_simple_info_no_exit("'%s' originally declared as a compiler builtin",
                                               get_string(((ast_builtin_t*)*it)->name));
                } else {
                    report_range_info_no_context_no_exit(&ASTP(decl)->loc,
                                                         "'%s' originally declared here:",
                                                         get_string(decl->name));
                }
            }
        } else if (i == array_len(path) - 1 && should_exit) {
            report_range_info_no_context(&ASTP(decl)->loc,
                                         "then to '%s' here:",
                                         get_string(decl->name));
        } else {
            report_range_info_no_context_no_exit(&((*it)->loc),
                                                 "then to '%s' here:",
                                                 get_string(decl->name));
        }
        i += 1;
    }
}

#define report_declaration_path(_path)         (_report_declaration_path(1, (_path)))
#define report_declaration_path_no_exit(_path) (_report_declaration_path(0, (_path)))


/* static void typecheck_assign(ast_t *lval, ast_t *rval, scope_t *scope) { */
/*     ASSERT(rval->type != TY_UNKNOWN, "right hand side of assignment doesn't have a type"); */
/*  */
/*     if (rval->type != lval->type) { */
/*         report_range_err(&ASTP(assign)->loc, */
/*                          "'%s' has type %s, but you're trying to assign it a value of type %s", */
/*                          get_string(assign->name), */
/*                          get_string(get_type_string_id(origin->type)), */
/*                          get_string(get_type_string_id(assign->val->type))); */
/*         return; */
/*     } */
/* } */

static void check_decl(ast_decl_t *decl, scope_t *scope) {
    u32 val_t;
    u32 decl_t;

    ASSERT(decl->type_expr || decl->val_expr,
           "decl misssing both type and val");

    if (decl->val_expr != NULL) {
        check_node(decl->val_expr, scope, decl);
        val_t = decl->val_expr->type;
    }

    if (decl->type_expr != NULL) {
        check_node(decl->type_expr, scope, decl);
        decl_t = decl->type_expr->value.t;
    } else {
        decl_t = val_t;
    }

    if (scope->in_proc && decl_t == TY_PROC) {
        report_range_err(&ASTP(decl)->loc, "procedures may not be defined within another procedure");
        return;
    }

    if (decl->type_expr != NULL
    &&  decl->val_expr  != NULL
    &&  decl_t          != val_t) {

        report_range_err_no_exit(&decl->val_expr->loc,
                                 "initialization of '%s' does not match declared type of %s",
                                 get_string(decl->name),
                                 get_string(get_type_string_id(decl_t)));
        report_range_info(&decl->type_expr->loc,
                          "expected %s, but got %s",
                          get_string(get_type_string_id(decl_t)),
                          get_string(get_type_string_id(val_t)));
    }

    ASTP(decl)->type = decl_t;
}

static void check_proc(ast_proc_t *proc, scope_t *scope, ast_decl_t *parent_decl) {
    scope_t  *new_scope;
    u32       n_params;
    u32      *param_types;
    int       i;
    ast_t   **it;
    u32       ret_type;

    ASTP(proc)->value.a = ASTP(proc);

    ASSERT(!(ASTP(proc)->flags & AST_FLAG_POLYMORPH), "TODO");

    array_push(proc_stack, proc);

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
                                get_string(parent_decl->name));
            report_simple_info("got %s instead", get_string(get_type_string_id(proc->ret_type_expr->type)));
            return;
        }

        ret_type = proc->ret_type_expr->value.t;
    } else {
        ret_type = TY_NOT_TYPED;
    }

    ASTP(proc)->type = get_proc_type(n_params, param_types, ret_type);


    /* @bad?, @refactor
    ** We have to bubble this type up to the declaration so that identifier lookups
    ** that occur before we return from this routine can get the right type.
    ** I'm not sure if this should just be done everywhere like this (shouldn't
    ** be _that_ many spots), or if something a little smarter should be done.
    */
    ASTP(parent_decl)->type  = ASTP(proc)->type;
    ASTP(parent_decl)->value = ASTP(proc)->value;

    if (proc->block != NULL) {
        check_node(proc->block, new_scope, NULL);
    } else {
        ASSERT(ASTP(proc)->flags & AST_FLAG_IS_EXTERN,
               "proc is not extern, but has no body");
    }

    array_pop(proc_stack);
}

static void check_param(ast_param_t *param, scope_t *scope) {
    ast_t                     *existing_node;
    ast_polymorph_type_name_t *poly;

    ASSERT(scope->parent != NULL, "param in global scope??");

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

#if 0
static void check_var(ast_decl_t *decl, scope_t *scope) {
    u32 val_t;
    u32 decl_t;

    ASSERT(decl->type_expr || decl->val_expr,
           "decl misssing both type and val");

    if (decl->val_expr != NULL) {
        check_node(decl->val_expr, scope, decl);
        val_t = decl->val_expr->type;
    }

    if (decl->type_expr != NULL) {
        check_node(decl->type_expr, scope, decl);
        decl_t = decl->type_expr->value.t;
    } else {
        decl_t = val_t;
    }

    if (decl->type_expr != NULL
    &&  decl->val_expr  != NULL
    &&  decl_t          != val_t) {

        report_range_err_no_exit(&decl->val_expr->loc,
                                 "initialization of '%s' does not match declared type of %s",
                                 get_string(decl->name),
                                 get_string(get_type_string_id(decl_t)));
        report_range_info(&decl->type_expr->loc,
                          "expected %s, but got %s",
                          get_string(get_type_string_id(decl_t)),
                          get_string(get_type_string_id(val_t)));
    }

    ASTP(decl)->type = decl_t;
}
#endif

static void check_int(ast_int_t *integer, scope_t *scope) {
    ASTP(integer)->type    = TY_S64;
    ASTP(integer)->value.i = strtoll(get_string(integer->str_rep), NULL, 10);
}

static void check_float(ast_int_t *integer, scope_t *scope) {
    ASTP(integer)->type    = TY_F64;
    ASTP(integer)->value.f = strtod(get_string(integer->str_rep), NULL);
}

static void check_string(ast_string_t *string, scope_t *scope) {
    const char *contents;
    u32         len;
    char       *buff;
    u32         new_len;
    u32         i;
    char        c;

    contents = get_string(string->str_rep);
    len      = strlen(contents);
    buff     = alloca(len);
    new_len  = 0;

    ASSERT(contents[0] == '"', "string doesn't start with quote");

    i = 1;
    for (; i < len; i += 1) {
        c = contents[i];

        if (c == '"') { break; }
        if (c == '\\') {
            i += 1;
            if (i < len) {
                c = contents[i];
                switch (c) {
                    case 'n':
                        c = '\n';
                        break;
                    case 'r':
                        c = '\r';
                        break;
                    case 't':
                        c = '\t';
                        break;
                    case '"':
                        c = '"';
                        break;
                    default:
                        report_range_err(&ASTP(string)->loc, "unknown escape character '%c'", c);
                        return;
                }
            }
            goto add_char;
        } else {
add_char:;
            buff[new_len] = c;
        }
        new_len += 1;
    }

    ASTP(string)->value.s = get_string_id_n(buff, new_len);
    ASTP(string)->type    = get_ptr_type(TY_U8);
}

static ast_t * try_get_decl_and_path(ast_ident_t *ident, array_t *path) {
    if (ident->resolved_node == NULL) { return NULL; }

    ASSERT(   ast_kind_is_decl(ident->resolved_node->kind)
           || ident->resolved_node->kind == AST_BUILTIN,
           "resolved_node is not an assignment or a builtin");

    *path = get_declaration_path(ident);

    if (array_len(*path) == 0) {
        return ident->resolved_node;
    }

    return *(ast_t**)array_last(*path);
}

static void check_builtin_special_call(ast_bin_expr_t *expr, scope_t *scope) {
    ast_ident_t    *left_ident;
    ast_t          *builtin_origin;
    array_t         path;
    ast_builtin_t  *builtin;
    ast_arg_list_t *arg_list;
    arg_t          *arg_p;

    ASSERT(expr->left->kind == AST_IDENT, "call to builtin must be through an identifier");

    left_ident     = (ast_ident_t*)expr->left;
    builtin_origin = try_get_decl_and_path(left_ident, &path);

    ASSERT(builtin_origin != NULL,              "didn't get builtin_origin");
    ASSERT(builtin_origin->kind == AST_BUILTIN, "builtin_origin is not an AST_BUILTIN");

    builtin = (ast_builtin_t*)builtin_origin;

    ASSERT(expr->right->kind == AST_ARG_LIST, "expr->right is not an AST_ARG_LIST in call");

    arg_list = (ast_arg_list_t*)expr->right;

    if (builtin->name == CAST_ID) {
        if (array_len(arg_list->args) < 2) {
            report_loc_err_no_exit(expr->right->loc.end,
                                   "too few arguments in cast");
            report_simple_info("expected 2, but got %d", array_len(arg_list->args));
        }
        if (array_len(arg_list->args) > 2) {
            arg_p = array_item(arg_list->args, 2);
            report_range_err_no_exit(&arg_p->expr->loc,
                                     "too many arguments in cast");
            report_simple_info("expected 2, but got %d", array_len(arg_list->args));
        }

        arg_p = array_item(arg_list->args, 0);
        if (arg_p->expr->type != TY_TYPE) {
            report_range_err_no_exit(&arg_p->expr->loc,
                                     "first argument in cast must be the destination type");
            report_simple_info("expected a type, but got %s",
                               get_string(get_type_string_id(arg_p->expr->type)));
        }

        ASTP(expr)->flags |= AST_FLAG_CALL_IS_CAST;
        ASTP(expr)->type   = arg_p->expr->value.t;

        /* @todo -- check that they types can actually cast */
    } else {
        ASSERT(0, "unahandled builtin special");
    }

    array_free(path);
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
    ast_t          *proc_origin;
    array_t         path;
    ast_proc_t     *proc;
    ast_t          *parm_decl;

    proc_ty = expr->left->type;

    if (proc_ty == TY_BUILTIN_SPECIAL) {
        check_builtin_special_call(expr, scope);
        return;
    }

    if (type_kind(proc_ty) != TY_PROC) {
        report_range_err_no_exit(&ASTP(expr)->loc,
                                 "attempting to call a value that is not a procedure");
        report_range_info(&expr->left->loc,
                          "value has type %s",
                          get_string(get_type_string_id(proc_ty)));
        return;
    }

    left_ident  = NULL;
    proc_origin = NULL;
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
                proc_origin = try_get_decl_and_path(left_ident, &path);

                if (left_ident->resolved_node == proc_origin) {
                    report_simple_info_no_exit("expected %s %d, but got %d",
                                               varg_ty != TY_NONE ? "at least" : "", n_params, n_args);
                    if (proc_origin->kind == AST_BUILTIN) {
                        report_simple_info("'%s' is a compiler builtin",
                                           get_string(((ast_builtin_t*)proc_origin)->name));
                    } else {
                        report_range_info_no_context(&proc_origin->loc,
                                                     "'%s' defined here:",
                                                     get_string(((ast_decl_t*)proc_origin)->name));
                    }
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
                proc_origin = try_get_decl_and_path(left_ident, &path);

                if (left_ident->resolved_node == proc_origin) {
                    report_simple_info_no_exit("expected %d, but got %d", n_params, n_args);

                    if (proc_origin->kind == AST_BUILTIN) {
                        report_simple_info("'%s' is a compiler builtin",
                                           get_string(((ast_builtin_t*)proc_origin)->name));
                    } else {
                        report_range_info_no_context(&proc_origin->loc,
                                                     "'%s' defined here:",
                                                     get_string(((ast_decl_t*)proc_origin)->name));
                    }
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

                proc_origin = try_get_decl_and_path(left_ident, &path);
                if (proc_origin->kind == AST_BUILTIN) {
                    if (left_ident->resolved_node == proc_origin) {
                        report_simple_info("'%s' is a compiler builtin",
                                            get_string(((ast_builtin_t*)proc_origin)->name));
                    } else {
                        report_declaration_path(path);
                    }
                } else {
                    proc        = (ast_proc_t*)((ast_decl_t*)proc_origin)->val_expr;
                    parm_decl   = *(ast_t**)array_item(proc->params, i);
                    if (left_ident->resolved_node == proc_origin) {
                        report_range_info_no_context(&parm_decl->loc, "parameter declaration here:");
                    } else {
                        report_range_info_no_context_no_exit(&parm_decl->loc, "parameter declaration here:");
                        report_declaration_path(path);
                    }
                }
            }

            return;
        }

        if (arg_p->name != STRING_ID_NULL) {
            if (left_ident == NULL) {
                report_range_err(&arg_p->expr->loc, "named arguments are not allowed in indirect calls");
                return;
            } else {
                if (proc_origin == NULL) {
                    proc_origin = try_get_decl_and_path(left_ident, &path);

                    if (proc_origin->kind == AST_BUILTIN) {
                        report_range_err_no_exit(&arg_p->expr->loc,
                                                 "using argument name '%s' for a call to a compiler builtin, which does not have named parameters",
                                                 get_string(arg_p->name));
                        if (left_ident->resolved_node == proc_origin) {
                            report_simple_info("'%s' is a compiler builtin",
                                               get_string(((ast_builtin_t*)proc_origin)->name));
                        } else {
                            report_declaration_path(path);
                        }
                        return;
                    }

                    proc = (ast_proc_t*)((ast_decl_t*)proc_origin)->val_expr;
                }

                parm_decl = *(ast_t**)array_item(proc->params, i);
                if (arg_p->name != ((ast_param_t*)parm_decl)->name) {
                    report_range_err_no_exit(&arg_p->expr->loc,
                                                "argument name '%s' does not match parameter name '%s'",
                                                get_string(arg_p->name),
                                                get_string(((ast_param_t*)parm_decl)->name));
                    if (left_ident->resolved_node == proc_origin) {
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
                    proc_origin = try_get_decl_and_path(left_ident, &path);

                    if (proc_origin->kind == AST_BUILTIN) {
                        if (left_ident->resolved_node == proc_origin) {
                            report_simple_info("'%s' is a compiler builtin",
                                               get_string(((ast_builtin_t*)proc_origin)->name));
                        } else {
                            report_declaration_path(path);
                        }
                        return;
                    }

                    proc        = (ast_proc_t*)((ast_decl_t*)proc_origin)->val_expr;
                    parm_decl   = *(ast_t**)array_last(proc->params);
                    if (left_ident->resolved_node == proc_origin) {
                        report_range_info_no_context(&parm_decl->loc, "variadic parameter list declared here:");
                    } else if (left_ident->resolved_node != proc_origin) {
                        report_range_info_no_context_no_exit(&parm_decl->loc, "variadic parameter list declared here:");
                        report_declaration_path(path);
                    }
                }
            }

            if (arg_p->name != STRING_ID_NULL) {
                if (proc == NULL) {
                    proc_origin = try_get_decl_and_path(left_ident, &path);

                    if (proc_origin->kind == AST_BUILTIN) {
                        report_range_err_no_exit(&arg_p->expr->loc,
                                                    "using argument name '%s' for a call to a compiler builtin, which does not have named parameters",
                                                    get_string(arg_p->name));
                        if (left_ident->resolved_node == proc_origin) {
                            report_simple_info("'%s' is a compiler builtin",
                                            get_string(((ast_builtin_t*)proc_origin)->name));
                        } else {
                            report_declaration_path(path);
                        }
                        return;
                    }

                    proc = (ast_proc_t*)((ast_decl_t*)proc_origin)->val_expr;
                }

                if (i == n_params) {
                    if (left_ident == NULL) {
                        report_range_err(&arg_p->expr->loc, "named arguments are not allowed in indirect calls");
                        return;
                    } else {
                        parm_decl = *(ast_t**)array_last(proc->params);
                        if (arg_p->name != ((ast_param_t*)parm_decl)->name) {
                            report_range_err_no_exit(&arg_p->expr->loc,
                                                        "argument name '%s' does not match variadic parameter list name '%s'",
                                                        get_string(arg_p->name),
                                                        get_string(((ast_param_t*)parm_decl)->name));
                            if (left_ident->resolved_node == proc_origin) {
                                report_range_info_no_context(&parm_decl->loc, "parameter declaration here:");
                            } else {
                                report_range_info_no_context_no_exit(&parm_decl->loc, "parameter declaration here:");
                                report_declaration_path(path);
                            }
                        }
                    }
                } else {
                    report_range_err_no_exit(&arg_p->expr->loc, "only the first argument for a variadic parameter list may be named");
                    if (left_ident->resolved_node == proc_origin) {
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
    ast_t        *resolved_node;
    ast_decl_t   *resolved_decl;
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

    resolved_node = try_get_decl_and_path(left_ident, &path);

    ASSERT(resolved_node != NULL, "did not get resolved node");
    ASSERT(ast_kind_is_decl(resolved_node->kind),
           "resolved_node is not a declaration");

    resolved_decl = (ast_decl_t*)resolved_node;

    ASSERT(resolved_decl->val_expr->kind == AST_MODULE,
           "resolved_decl is not a module declaration");

    module_scope = get_subscope_from_node(resolved_decl->scope,
                                          resolved_decl->val_expr);

    ASSERT(module_scope != NULL, "did not find module scope");

    right_ident = (ast_ident_t*)expr->right;

    found_node = find_in_scope(module_scope, right_ident->str_rep);

    if (found_node == NULL) {
        if (left_ident->resolved_node == ASTP(resolved_decl)) {
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

static void check_mul(ast_bin_expr_t *expr) {
    u32 t1;
    u32 t2;
    u32 tk1;
    u32 tk2;

    t1  = expr->left->type;
    t2  = expr->right->type;
    tk1 = type_kind(t1);
    tk2 = type_kind(t2);

    if (!(
           (tk1 == TY_GENERIC_INT   && tk2 == TY_GENERIC_INT)
        || (tk1 == TY_GENERIC_FLOAT && tk2 == TY_GENERIC_FLOAT))) {

        binop_bad_type_error(expr);
        return;
    }

    /* @todo check for width incompat */
    /* @todo How to handle this case? Do we promote the type? */
    ASTP(expr)->type = t1;
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
        case OP_MULT:
            check_mul(expr);
            break;

        case OP_EQU:
        case OP_NEQ:
        case OP_LSS:
        case OP_LEQ:
        case OP_GTR:
        case OP_GEQ:
            ASTP(expr)->type = TY_S64;
            break;

        case OP_PLUS_ASSIGN:
            check_add(expr);
            ASSERT(ASTP(expr)->type == expr->left->type, "type should not change in assignment");
            break;
        case OP_MINUS_ASSIGN:
            check_sub(expr);
            ASSERT(ASTP(expr)->type == expr->left->type, "type should not change in assignment");
            break;
        case OP_ASSIGN:
            if (expr->left->type != expr->right->type) {
                report_range_err(&ASTP(expr)->loc,
                                "left-hand-side operand has type %s, but you're trying to assign it a value of type %s",
                                get_string(get_type_string_id(expr->left->type)),
                                get_string(get_type_string_id(expr->right->type)));
                return;
            }
            ASTP(expr)->type  = expr->left->type;
            ASTP(expr)->value = expr->right->value;
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
        case OP_ARRAY:
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
        case OP_NOT:
            if (!type_kind_is_int(type_kind(expr->child->type))) {
                report_range_err_no_exit(&ASTP(expr)->loc,
                                         "right-hand-side operand of 'not' operator must be an integer");
                report_range_info_no_context(&expr->child->loc,
                                             "operand has type %s",
                                             get_string(get_type_string_id(expr->child->type)));
            }
            ASTP(expr)->type = expr->child->type;
            break;
        case OP_NEG:
            if (!type_kind_is_numeric(type_kind(expr->child->type))) {
                report_range_err_no_exit(&ASTP(expr)->loc,
                                         "right-hand-side operand of '-' operator must be numeric");
                report_range_info_no_context(&expr->child->loc,
                                             "operand has type %s",
                                             get_string(get_type_string_id(expr->child->type)));
            }
            ASTP(expr)->type = expr->child->type;
            break;
        default:
            report_loc_err_no_exit(ASTP(expr)->loc.beg, "UNHANDLED OPERATOR: %s", OP_STR(expr->op));
            ASSERT(0, "unhandled unary operator");
            return;
    }
}

static void check_struct(ast_struct_t *st, scope_t *scope, ast_decl_t *parent_decl) {
    scope_t  *new_scope;
    u32       n_params;
    u32      *param_types;
    int       i;
    ast_t   **it;

    ASTP(st)->value.a = ASTP(st);

    ASSERT(!(ASTP(st)->flags & AST_FLAG_POLYMORPH), "TODO");

    new_scope = get_subscope_from_node(scope, ASTP(st));
    ASSERT(new_scope != NULL, "didn't get scope");

    n_params    = array_len(st->params);
    param_types = alloca(sizeof(u32) * n_params);
    i           = 0;
    array_traverse(st->params, it) {
        check_node(*it, new_scope, NULL);
        param_types[i] = (*it)->type;
        i += 1;
    }

    ASTP(st)->type    = TY_TYPE;
    ASTP(st)->value.t = get_struct_type(st, parent_decl->name, scope);

    ASTP(parent_decl)->type  = ASTP(st)->type;
    ASTP(parent_decl)->value = ASTP(st)->value;

    /* @todo -- check fields. */
}

static void check_arg_list(ast_arg_list_t *arg_list, scope_t *scope) {
    arg_t *arg;

    ASTP(arg_list)->type = TY_NOT_TYPED;

    array_traverse(arg_list->args, arg) {
        check_node(arg->expr, scope, NULL);
    }
}

static void check_if(ast_if_t *_if, scope_t *scope) {
    scope_t *new_scope;

    ASTP(_if)->type = TY_NOT_TYPED;

    new_scope = get_subscope_from_node(scope, ASTP(_if));
    ASSERT(new_scope != NULL, "didn't get scope");

    check_node(_if->expr, new_scope, NULL);

    if (!type_kind_is_int(type_kind(_if->expr->type))) {
        report_range_err_no_exit(&_if->expr->loc, "'if' condition must have an integer type");
        report_simple_info("got %s", get_string(get_type_string_id(_if->expr->type)));
        return;
    }

    check_node(_if->then_block, new_scope, NULL);

    if (_if->els != NULL) {
        check_node(_if->els, scope, NULL);
    }
}

static void check_loop(ast_loop_t *loop, scope_t *scope) {
    scope_t *new_scope;

    in_loop = 1;

    ASTP(loop)->type = TY_NOT_TYPED;

    new_scope = get_subscope_from_node(scope, ASTP(loop));
    ASSERT(new_scope != NULL, "didn't get scope");

    if (loop->init != NULL) {
        check_node(loop->init, new_scope, NULL);
    }
    if (loop->cond != NULL) {
        check_node(loop->cond, new_scope, NULL);
        if (!type_kind_is_int(type_kind(loop->cond->type))) {
            report_range_err_no_exit(&loop->cond->loc, "loop condition must have an integer type");
            report_simple_info("got %s", get_string(get_type_string_id(loop->cond->type)));
            goto out;
        }
    }
    if (loop->post != NULL) {
        check_node(loop->post, new_scope, NULL);
    }

    check_node(loop->block, new_scope, NULL);

out:;
    in_loop = 0;
}

static void check_break(ast_continue_t *brk, scope_t *scope) {
    ASTP(brk)->type = TY_NOT_TYPED;

    if (!in_loop) {
        report_range_err(&ASTP(brk)->loc, "break statement only valid within a loop");
        return;
    }
}

static void check_continue(ast_continue_t *cont, scope_t *scope) {
    ASTP(cont)->type = TY_NOT_TYPED;

    if (!in_loop) {
        report_range_err(&ASTP(cont)->loc, "continue statement only valid within a loop");
        return;
    }
}

static void check_return(ast_return_t *ret, scope_t *scope) {
    ast_proc_t **proc_it;
    ast_proc_t  *proc;

    proc_it = array_last(proc_stack);

    if (proc_it == NULL) {
        report_range_err(&ASTP(ret)->loc, "return statement only valid in a procedure body");
        return;
    }

    proc = *proc_it;

    if (ret->expr != NULL) {
        check_node(ret->expr, scope, NULL);

        if (proc->ret_type_expr == NULL) {
            report_range_err(&ret->expr->loc,
                             "attempting to return %s in a procedure that does not return a value",
                             get_string(get_type_string_id(ret->expr->value.t)));
            return;
        }
        if (ret->expr->type != proc->ret_type_expr->value.t) {
            report_range_err(&ret->expr->loc,
                             "incorrect type of returned expression: expected %s, but got %s",
                             get_string(get_type_string_id(proc->ret_type_expr->value.t)),
                             get_string(get_type_string_id(ret->expr->type)));
            return;
        }
    } else if (proc->ret_type_expr != NULL) {
        report_range_err(&ASTP(ret)->loc,
                         "return statement missing expression in a procedure that returns %s", get_string(get_type_string_id(proc->ret_type_expr->value.t)));
        return;
    }

    ASTP(ret)->type = TY_NOT_TYPED;
}

static void check_defer(ast_defer_t *defer, scope_t *scope) {
    ASTP(defer)->type = TY_NOT_TYPED;

    check_node(defer->block, scope, NULL);
}

void check_node(ast_t *node, scope_t *scope, ast_decl_t *parent_decl) {
    ast_t **it;

    if (node->flags & AST_FLAG_CHECKED) { return; }
    node->flags |= AST_FLAG_CHECKED;

    switch (node->kind) {
#define X(_kind) case _kind:
        X_AST_DECLARATIONS
#undef X
            check_decl((ast_decl_t*)node, scope);

#if 0
#define X(_kind) case _kind:
        X_AST_DECLARATIONS
#undef X

            if (node->kind == AST_DECL_VAR) {
                check_var((ast_decl_t*)node, scope);
            } else {
                check_node(((ast_decl_t*)node)->val_expr, scope, (ast_decl_t*)node);
            }
#endif

            break;

        case AST_MODULE:
            array_traverse(((ast_module_t*)node)->children, it) {
                check_node(*it, get_subscope_from_node(scope, node), NULL);
            }
            break;
        case AST_PROC:
            check_proc((ast_proc_t*)node, scope, parent_decl);
            break;

        case AST_PARAM:
            check_param((ast_param_t*)node, scope);
            break;

        case AST_STRUCT:
            check_struct((ast_struct_t*)node, scope, parent_decl);
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

        case AST_INT:
            check_int((ast_int_t*)node, scope);
            break;
        case AST_FLOAT:
            check_float((ast_int_t*)node, scope);
            break;
        case AST_STRING:
            check_string((ast_string_t*)node, scope);
            break;

        case AST_IDENT:
            check_ident((ast_ident_t*)node, scope);
            break;

        case AST_ARG_LIST:
            check_arg_list((ast_arg_list_t*)node, scope);
            break;

        case AST_IF:
            check_if((ast_if_t*)node, scope);
            break;

        case AST_LOOP:
            check_loop((ast_loop_t*)node, scope);
            break;

        case AST_CONTINUE:
            check_continue((ast_continue_t*)node, scope);
            break;

        case AST_BREAK:
            check_break((ast_continue_t*)node, scope);
            break;

        case AST_RETURN:
            check_return((ast_return_t*)node, scope);
            break;

        case AST_DEFER:
            check_defer((ast_defer_t*)node, scope);
            break;

        case AST_BUILTIN:
            break;

        default:;
#ifdef SIMON_DO_ASSERTIONS
            report_range_err_no_exit(&node->loc, "INTERNAL ERROR: AST_%s unhandled in check_node()", ast_get_kind_str(node->kind));
            ASSERT(0, "unhandled AST node kind in check_node()");
#endif
    }

#ifdef SIMON_DO_ASSERTIONS
    if (node->type == TY_UNKNOWN) {
        report_range_err_no_exit(&node->loc, "INTERNAL ERROR: type not resolved in check_node()");
    }
    ASSERT(node->type != TY_UNKNOWN, "did not resolve type");
#endif
}
