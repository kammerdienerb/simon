#include "ast.h"
#include "globals.h"
#include "scope.h"
#include "ui.h"
#include "type.h"
#include "parse.h"
#include "array.h"
#include "memory.h"
#include "tls.h"

typedef struct {
    ast_t           *node;
    array_t         *monomorphs;
    check_context_t  cxt;
    src_range_t      range;
} poly_backlog_entry_t;

static array_t poly_backlog;

static void check_node(check_context_t cxt, ast_t *node);


static inline scope_t *get_scope(ast_t *node) {
    /* There are sanity checks to make sure that these nodes have their scope
     * pointer as their first field so that we can do this cheaply. */

    ASSERT(   node->kind == AST_MODULE
           || node->kind == AST_PROC
           || node->kind == AST_STRUCT
           || node->kind == AST_BLOCK
           || node->kind == AST_IF
           || node->kind == AST_LOOP,
           "node kind does not have a scope");

    return ((ast_scoped_t*)node)->scope;
}

static inline int num_args(ast_arg_list_t *arg_list) {
    return array_len(arg_list->args);
}

static inline ast_t *get_arg(ast_arg_list_t *arg_list, int idx) {
    ASSERT(idx < array_len(arg_list->args), "invalid arg index");
    return ((arg_t*)array_item(arg_list->args, idx))->expr;
}

#define AST_ALLOC(t) \
    (bump_alloc(&(get_tls()->bump_alloc), sizeof(t)))

#define CPY_WHOLE(dst, src) (memcpy((void*)(dst), (void*)(src), sizeof(__typeof(*(dst)))))

#define ALLOC_CPY(dst, src)           \
do {                                  \
    dst = AST_ALLOC(__typeof(*dst));  \
    CPY_WHOLE(dst, src);              \
    ASTP(dst)->type     = TY_UNKNOWN; \
    ASTP(dst)->value.a  = NULL;       \
} while (0)

#define CPY_FIELD(dst, src, nm, scp) ((dst)->nm = _copy_tree(((__typeof(dst))(src))->nm, (scp), collect_macro_expand_args));
#define CPY_SCOPE(dst, _parent_scope)                \
do {                                                 \
    (dst)->scope         = copy_scope((dst)->scope); \
    (dst)->scope->node   = ASTP((dst));              \
    insert_subscope((_parent_scope), (dst)->scope);  \
} while (0)

#define REPLACE_NODE(dst, src)                                         \
do {                                                                   \
    ASSERT(ast_size_table[(dst)->kind] >= ast_size_table[(src)->kind], \
           "dst node not big enough for replacement");                 \
    memcpy((void*)(dst), (void*)(src), ast_size_table[(src)->kind]);   \
} while (0)

static ast_t *_copy_tree(ast_t *node, scope_t *insert_scope, array_t *collect_macro_expand_args) {
    ast_t *exp;

    if (node == NULL) { return NULL; }

    switch (node->kind) {
#define X(_kind) case _kind:
        X_AST_DECLARATIONS
#undef X
        {
            ast_decl_t  *decl; ALLOC_CPY(decl, node);
            ast_t      **it;
            ast_t       *new;

            CPY_FIELD(decl, node, type_expr, insert_scope);
            CPY_FIELD(decl, node, val_expr,  insert_scope);

            decl->tags = array_make(ast_t*);
            array_traverse(((ast_decl_t*)node)->tags, it) {
                new = _copy_tree(*it, insert_scope, collect_macro_expand_args);
                array_push(decl->tags, new);
            }

            if (!insert_scope->in_proc) {
                add_symbol(insert_scope, decl->name, ASTP(decl));
            }
            decl->containing_scope = insert_scope;

            if (node->flags & AST_FLAG_MACRO_EXPAND_NAME
            &&  collect_macro_expand_args != NULL) {

                exp = ASTP(decl);
                array_push(*collect_macro_expand_args, exp);
            }

            return ASTP(decl);
        }
        case AST_MODULE: {
            ast_module_t  *mod; ALLOC_CPY(mod, node);
            ast_t        **it;
            ast_t         *new;

            CPY_SCOPE(mod, insert_scope);

            mod->children = array_make(ast_t*);
            array_traverse(((ast_module_t*)node)->children, it) {
                new = _copy_tree(*it, mod->scope, collect_macro_expand_args);
                array_push(mod->children, new);
            }

            return ASTP(mod);
        }
        case AST_PROC: {
            ast_proc_t  *proc; ALLOC_CPY(proc, node);
            ast_t      **it;
            ast_t       *new;

            CPY_SCOPE(proc, insert_scope);

            proc->params = array_make(ast_t*);
            array_traverse(((ast_proc_t*)node)->params, it) {
                new = _copy_tree(*it, proc->scope, collect_macro_expand_args);
                array_push(proc->params, new);
            }
            CPY_FIELD(proc, node, ret_type_expr, proc->scope);
            CPY_FIELD(proc, node, block, proc->scope);
            proc->monomorphs = array_make(monomorphed_t);

            return ASTP(proc);
        }
        case AST_STRUCT: {
            ast_struct_t  *st; ALLOC_CPY(st, node);
            ast_t        **it;
            ast_t         *new;

            CPY_SCOPE(st, insert_scope);

            st->params = array_make(ast_t*);
            array_traverse(((ast_struct_t*)node)->params, it) {
                new = _copy_tree(*it, st->scope, collect_macro_expand_args);
                array_push(st->params, new);
            }
            st->fields = array_make(ast_t*);
            array_traverse(((ast_struct_t*)node)->fields, it) {
                new = _copy_tree(*it, st->scope, collect_macro_expand_args);
                array_push(st->fields, new);
            }
            st->children = array_make(ast_t*);
            array_traverse(((ast_struct_t*)node)->children, it) {
                new = _copy_tree(*it, st->scope, collect_macro_expand_args);
                array_push(st->children, new);
            }
            st->monomorphs = array_make(monomorphed_t);

            return ASTP(st);
        }
        case AST_MACRO: goto unhandled;
        case AST_PARAM: {
            ast_param_t *param; ALLOC_CPY(param, node);

            CPY_FIELD(param, node, type_expr, insert_scope);
            CPY_FIELD(param, node, val, insert_scope);

            add_symbol(insert_scope, param->name, ASTP(param));

            if (node->flags & AST_FLAG_MACRO_EXPAND_NAME
            &&  collect_macro_expand_args != NULL) {

                exp = ASTP(param);
                array_push(*collect_macro_expand_args, exp);
            }

            return ASTP(param);
        }
        case AST_INT: {
            ast_int_t *i; ALLOC_CPY(i, node);
            return ASTP(i);
        }
        case AST_FLOAT: {
            ast_float_t *f; ALLOC_CPY(f, node);
            return ASTP(f);
        }
        case AST_STRING: {
            ast_string_t *s; ALLOC_CPY(s, node);
            return ASTP(s);
        }
        case AST_CHAR: {
            ast_char_t *c; ALLOC_CPY(c, node);
            return ASTP(c);
        }
        case AST_IDENT: {
            ast_ident_t *ident; ALLOC_CPY(ident, node);

            ident->resolved_node = NULL;

            if (ASTP(ident)->flags & AST_FLAG_POLY_IDENT) {
                add_symbol(insert_scope, ident->str_rep, ASTP(ident));
            }

            return ASTP(ident);
        }
        case AST_PROC_TYPE: {
            ast_proc_type_t  *proc_type; ALLOC_CPY(proc_type, node);
            ast_t           **it;
            ast_t            *new;

            proc_type->param_type_exprs = array_make(ast_t*);

            array_traverse(((ast_proc_type_t*)node)->param_type_exprs, it) {
                new = _copy_tree(*it, insert_scope, collect_macro_expand_args);
                array_push(proc_type->param_type_exprs, new);
            }

            CPY_FIELD(proc_type, node, ret_type_expr, insert_scope);

            return ASTP(proc_type);
        }
        case AST_UNARY_EXPR: {
            ast_unary_expr_t *expr; ALLOC_CPY(expr, node);

            CPY_FIELD(expr, node, child, insert_scope);

            return ASTP(expr);
        }
        case AST_BIN_EXPR: {
            ast_bin_expr_t *expr; ALLOC_CPY(expr, node);

            CPY_FIELD(expr, node, left,  insert_scope);
            CPY_FIELD(expr, node, right, insert_scope);

            return ASTP(expr);
        }
        case AST_BLOCK: {
            ast_block_t  *block; ALLOC_CPY(block, node);
            ast_t       **it;
            ast_t        *new;

            CPY_SCOPE(block, insert_scope);

            block->stmts = array_make(ast_t*);
            array_traverse(((ast_block_t*)node)->stmts, it) {
                new = _copy_tree(*it, block->scope, collect_macro_expand_args);
                array_push(block->stmts, new);
            }

            return ASTP(block);
        }
        case AST_ARG_LIST: {
            ast_arg_list_t *arg_list; ALLOC_CPY(arg_list, node);
            arg_t          *it;
            arg_t           new_arg;

            arg_list->args = array_make(arg_t);

            array_traverse(((ast_arg_list_t*)node)->args, it) {
                new_arg.name = it->name;
                new_arg.expr = _copy_tree(it->expr, insert_scope, collect_macro_expand_args);
                array_push(arg_list->args, new_arg);
            }

            return ASTP(arg_list);
        }
        case AST_IF: {
            ast_if_t *_if; ALLOC_CPY(_if, node);

            CPY_SCOPE(_if, insert_scope);

            CPY_FIELD(_if, node, expr,       _if->scope);
            CPY_FIELD(_if, node, then_block, _if->scope);
            CPY_FIELD(_if, node, els,        _if->scope);

            return ASTP(_if);
        }
        case AST_LOOP: {
            ast_loop_t *loop; ALLOC_CPY(loop, node);

            CPY_SCOPE(loop, insert_scope);

            CPY_FIELD(loop, node, init,  loop->scope);
            CPY_FIELD(loop, node, cond,  loop->scope);
            CPY_FIELD(loop, node, post,  loop->scope);
            CPY_FIELD(loop, node, block, loop->scope);

            return ASTP(loop);
        }
        case AST_RETURN: {
            ast_return_t *ret; ALLOC_CPY(ret, node);

            CPY_FIELD(ret, node, expr, insert_scope);

            return ASTP(ret);
        }
        case AST_DEFER: {
            ast_defer_t *defer; ALLOC_CPY(defer, node);

            CPY_FIELD(defer, node, block, insert_scope);

            return ASTP(defer);
        }
        case AST_BREAK:    { ast_break_t    *brk; ALLOC_CPY(brk, node); return ASTP(brk); }
        case AST_CONTINUE: { ast_continue_t *cnt; ALLOC_CPY(cnt, node); return ASTP(cnt); }
        case AST_COMPILE_ERROR: {
            ast_compile_error_t *err; ALLOC_CPY(err, node);
            return ASTP(err);
        }
        case AST_VARGS_BLOCK: {
            ast_vargs_block_t *vargs_block; ALLOC_CPY(vargs_block, node);

            CPY_FIELD(vargs_block, node, block, insert_scope);

            return ASTP(vargs_block);
        }
        case AST_MACRO_CALL: {
            ast_macro_call_t *macro_call; ALLOC_CPY(macro_call, node);

            CPY_FIELD(macro_call, node, ident,    insert_scope);
            CPY_FIELD(macro_call, node, arg_list, insert_scope);
            CPY_FIELD(macro_call, node, block,    insert_scope);

            return ASTP(macro_call);
        }
        case AST_MACRO_ARG_EXPAND:
        case AST_MACRO_BLOCK_ARG_EXPAND: {
            ast_macro_arg_expand_t *macro_arg_expand; ALLOC_CPY(macro_arg_expand, node);

            if (collect_macro_expand_args != NULL) {
                exp = ASTP(macro_arg_expand);
                array_push(*collect_macro_expand_args, exp);
            }

            return ASTP(macro_arg_expand);
        }
        default:
        unhandled:;
    }

#ifdef SIMON_DO_ASSERTIONS
    report_range_err_no_exit(&node->loc, "INTERNAL ERROR: AST_%s unhandled in _copy_tree()", ast_get_kind_str(node->kind));
    ASSERT(0, "unhandled AST node kind in _copy_tree()");
#endif

    return NULL;
}

static ast_t *copy_tree(ast_t *node, scope_t *insert_scope) {
    return _copy_tree(node, insert_scope, NULL);
}

static ast_t *copy_tree_collect_macro_expand_args(ast_t *node, scope_t *insert_scope, array_t *collect_macro_expand_args) {
    return _copy_tree(node, insert_scope, collect_macro_expand_args);
}

static void collect_macro_calls(ast_t *node, array_t *collect) {
    ast_t **it;
    arg_t  *arg_it;

    if (node == NULL) { return; }

    switch (node->kind) {
#define X(_kind) case _kind:
        X_AST_DECLARATIONS
#undef X
            collect_macro_calls(((ast_decl_t*)node)->type_expr, collect);
            collect_macro_calls(((ast_decl_t*)node)->val_expr, collect);
            break;

        case AST_MODULE:
            array_traverse(((ast_module_t*)node)->children, it) {
                collect_macro_calls(*it, collect);
            }
            break;

        case AST_PROC:
            array_traverse(((ast_proc_t*)node)->params, it) {
                collect_macro_calls(*it, collect);
            }
            collect_macro_calls(((ast_proc_t*)node)->ret_type_expr, collect);
            collect_macro_calls(((ast_proc_t*)node)->block, collect);
            break;

        case AST_STRUCT:
            array_traverse(((ast_struct_t*)node)->params, it) {
                collect_macro_calls(*it, collect);
            }
            array_traverse(((ast_struct_t*)node)->fields, it) {
                collect_macro_calls(*it, collect);
            }
            array_traverse(((ast_struct_t*)node)->children, it) {
                collect_macro_calls(*it, collect);
            }
            break;

        case AST_MACRO: goto unhandled;

        case AST_PARAM:
            collect_macro_calls(((ast_param_t*)node)->type_expr, collect);
            collect_macro_calls(((ast_param_t*)node)->val, collect);
            break;

        case AST_INT:    break;
        case AST_FLOAT:  break;
        case AST_STRING: break;
        case AST_CHAR:   break;
        case AST_IDENT:  break;

        case AST_UNARY_EXPR:
            collect_macro_calls(((ast_unary_expr_t*)node)->child, collect);
            break;

        case AST_BIN_EXPR:
            collect_macro_calls(((ast_bin_expr_t*)node)->left, collect);
            collect_macro_calls(((ast_bin_expr_t*)node)->right, collect);
            break;

        case AST_BLOCK:
            array_traverse(((ast_block_t*)node)->stmts, it) {
                collect_macro_calls(*it, collect);
            }
            break;

        case AST_ARG_LIST:
            array_traverse(((ast_arg_list_t*)node)->args, arg_it) {
                collect_macro_calls(arg_it->expr, collect);
            }
            break;

        case AST_IF:
            collect_macro_calls(((ast_if_t*)node)->expr,       collect);
            collect_macro_calls(((ast_if_t*)node)->then_block, collect);
            collect_macro_calls(((ast_if_t*)node)->els,        collect);
            break;

        case AST_LOOP:
            collect_macro_calls(((ast_loop_t*)node)->init,  collect);
            collect_macro_calls(((ast_loop_t*)node)->cond,  collect);
            collect_macro_calls(((ast_loop_t*)node)->post,  collect);
            collect_macro_calls(((ast_loop_t*)node)->block, collect);
            break;

        case AST_RETURN:
            collect_macro_calls(((ast_return_t*)node)->expr, collect);
            break;

        case AST_DEFER:
            collect_macro_calls(((ast_defer_t*)node)->block, collect);
            break;

        case AST_BREAK:    break;
        case AST_CONTINUE: break;

        case AST_COMPILE_ERROR: break;

        case AST_VARGS_BLOCK:
            collect_macro_calls(((ast_vargs_block_t*)node)->block, collect);
            break;

        case AST_MACRO_CALL:
            array_push(*collect, node);
            break;
        default:
        unhandled:;
#ifdef SIMON_DO_ASSERTIONS
            report_range_err_no_exit(&node->loc, "INTERNAL ERROR: AST_%s unhandled in collect_macro_calls()", ast_get_kind_str(node->kind));
            ASSERT(0, "unhandled AST node kind in collect_macro_calls()");
#endif
    }
}

static void push_breadcrumb_for_poly_constants(src_range_t *range, string_id decl_name_id, array_t *constants, int call) {
    char                  buff[512];
    const char           *lazy_comma;
    ast_poly_constant_t  *it;

    buff[0]    = 0;
    lazy_comma = "";
    array_traverse(*constants, it) {
        strncat(buff, lazy_comma, sizeof(buff) - strlen(buff) - 1);
        strncat(buff, get_string(it->name), sizeof(buff) - strlen(buff) - 1);
        strncat(buff, " = ", sizeof(buff) - strlen(buff) - 1);
        strncat(buff, get_string(value_to_string_id(ASTP(it)->value, ASTP(it)->type)), sizeof(buff) - strlen(buff) - 1);
        lazy_comma = ", ";
    }

    push_range_breadcrumb(range,
                          "... %s %s (which is polymorphic), where [ %s ] from these arguments:",
                          call ? "when calling" : "in",
                          get_string(decl_name_id),
                          buff);
}

#define EMBC(_node, ...)                \
do {                                    \
    if ((_node)->macro_decl != NULL) {  \
        push_macro_breadcrumb((_node)); \
    }                                   \
    __VA_ARGS__;                        \
    if ((_node)->macro_decl != NULL) {  \
        pop_breadcrumb();               \
    }                                   \
} while (0)

void check_poly_backlog(void) {
    poly_backlog_entry_t *backlog_entry_p;
    poly_backlog_entry_t  backlog_entry;
    monomorphed_t        *monomorphed;

    while (array_len(poly_backlog) > 0) {
        backlog_entry_p = array_last(poly_backlog);
        memcpy(&backlog_entry, backlog_entry_p, sizeof(backlog_entry));
        array_pop(poly_backlog);

        monomorphed = array_item(*backlog_entry.monomorphs, backlog_entry.cxt.monomorph_idx);

        if (monomorphed->specialization != NULL) { continue; }

        push_breadcrumb_for_poly_constants(&backlog_entry.range, backlog_entry.cxt.parent_decl->full_name, &monomorphed->constants, 0);
        check_node(backlog_entry.cxt, backlog_entry.node);
        pop_breadcrumb();
    }
}

static void expand_compile_error_macro(ast_macro_call_t *call) {
    ast_arg_list_t      *arg_list;
    ast_compile_error_t  error;
    ast_t               *arg;
    const char          *str;
    int                  len;
    char                *buff;

    arg_list = (ast_arg_list_t*)call->arg_list;
    if (num_args(arg_list) <= 0) {
        report_range_err(&ASTP(arg_list)->loc, "built in macro 'compile_error' requires at least one argument");
        return;
    }

    memset(&error, 0, sizeof(error));
    ASTP(&error)->kind    = AST_COMPILE_ERROR;
    ASTP(&error)->flags   = ASTP(call)->flags;
    ASTP(&error)->type    = TY_UNKNOWN;
    ASTP(&error)->value.a = NULL;
    ASTP(&error)->loc     = ASTP(call)->loc;

    arg = get_arg(arg_list, 0);
    if (arg->kind != AST_STRING) {
        report_range_err(&arg->loc, "argument to built in macro 'compile_error' must be a string literal");
        return;
    }

    str  = get_string(((ast_string_t*)arg)->str_rep);
    len  = strlen(str);
    buff = mem_alloc(1 + len);
    strcat(buff, str + 1);
    buff[len - 2] = 0;

    error.message = get_string_id(buff);

    REPLACE_NODE(ASTP(call), ASTP(&error));
}

static void expand_vargs_macro(ast_macro_call_t *call) {
    scope_t           *parent_scope;
    ast_proc_t        *proc;
    ast_decl_t        *decl;
    ast_vargs_block_t  vargs_block;

    if (!call->scope->in_proc) {
        report_range_err(&ASTP(call)->loc, "expansion of macro 'vargs' is only allowed within a procedure");
        return;
    }

    parent_scope = call->scope;

    while (parent_scope != NULL && parent_scope->kind != AST_PROC) {
        parent_scope = parent_scope->parent;
    }

    ASSERT(parent_scope != NULL, "did not find proc scope");

    proc = (ast_proc_t*)parent_scope->node;
    decl = proc->parent_decl;

    if (!(ASTP(proc)->flags & AST_FLAG_POLY_VARARGS)) {
        report_range_err_no_exit(&ASTP(call)->loc, "expansion of macro 'vargs' is only allowed within a procedure with polymorphic variadic arguments");
        if (!(ASTP(proc)->flags & AST_FLAG_POLY_VARARGS)) {
            report_range_info_no_context(&ASTP(decl)->loc, "procedure '%s' does not have variadic arguments", get_string(decl->name));
        }
        return;
    }

    memset(&vargs_block, 0, sizeof(vargs_block));
    ASTP(&vargs_block)->kind = AST_VARGS_BLOCK;
    ASTP(&vargs_block)->loc  = call->block->loc;
    vargs_block.block        = call->block;

    REPLACE_NODE(ASTP(call), ASTP(&vargs_block));
}

static void expand_code_to_string_macro(ast_macro_call_t *call) {
    ast_arg_list_t *arg_list;
    ast_t          *arg;
    u64             len;
    array_t         buff;
    int             bs;
    int             c;
    int             i;
    string_id       id;
    ast_string_t    string;

    arg_list = (ast_arg_list_t*)call->arg_list;
    if (num_args(arg_list) <= 0) {
        report_range_err(&ASTP(arg_list)->loc, "built in macro 'code_to_string' requires at least one argument");
        return;
    }

    arg = get_arg(arg_list, 0);
    validate_range(&arg->loc);
    len = arg->loc.end.buff_ptr - arg->loc.beg.buff_ptr;

    buff = array_make(char);
    bs   = '\\';
    c    = '"';
    array_push(buff, c);
    for (i = 0; i < len; i += 1) {
        c = arg->loc.beg.buff_ptr[i];
        if (c == '"') { array_push(buff, bs); }
        array_push(buff, c);
    }
    c = '"';
    array_push(buff, c);

    id = get_string_id_n(array_data(buff), array_len(buff));

    array_free(buff);

    memset(&string, 0, sizeof(string));
    ASTP(&string)->kind    = AST_STRING;
    ASTP(&string)->flags   = ASTP(call)->flags | AST_FLAG_CONSTANT;
    ASTP(&string)->type    = TY_U8;
    ASTP(&string)->value.s = id;
    ASTP(&string)->loc     = ASTP(call)->loc;
    string.str_rep         = id;

    REPLACE_NODE(ASTP(call), ASTP(&string));
}

static void check_ident(check_context_t cxt, ast_ident_t *ident);

void expand_macro(ast_macro_call_t *call) {
    ast_ident_t     *ident;
    check_context_t  cxt;
    ast_arg_list_t  *arg_list;
    src_range_t      call_loc;
    ast_t           *found_node;
    string_id        found_name;
    ast_macro_t     *macro;
    int              expected_kind;
    ast_t           *new_node;
    ast_t           *new_stmt;
    array_t          macro_expand_args;
    ast_t          **it;
    ast_t           *expand_arg;
    arg_t           *arg_p;
    string_id        name;
    src_range_t      expand_arg_loc;
    u32              idx;
    string_id       *param_name_it;
    array_t          macro_calls;
    const char      *err_str;

    ident = (ast_ident_t*)call->ident;

    if (ident->str_rep == COMPILE_ERROR_ID) {
        expand_compile_error_macro(call);
        return;
    } else if (ident->str_rep == VARGS_ID) {
        expand_vargs_macro(call);
        return;
    } else if (ident->str_rep == CODE_TO_STRING_ID) {
        expand_code_to_string_macro(call);
        return;
    }

    /* I'm not sure if this is the right time/way to do this... */
    memset(&cxt, 0, sizeof(cxt));
    cxt.scope = call->scope;
    check_ident(cxt, ident);

    arg_list   = (ast_arg_list_t*)call->arg_list;
    call_loc   = ASTP(call)->loc;
    found_node = ident->resolved_node;


    ASSERT(ast_kind_is_decl(found_node->kind) || found_node->kind == AST_PARAM, "ident does not resolve to a declaration");

    found_name = found_node->kind == AST_PARAM
                    ? ((ast_param_t*)found_node)->name
                    : ((ast_decl_t*)found_node)->name;
    if (found_node->kind != AST_DECL_MACRO) {
        report_range_err_no_exit(&call_loc,
                                 "'%s' is not a macro",
                                 get_string(found_name));
        report_range_info_no_context(&found_node->loc, "'%s' declared here:", found_name);
    }

    check_node(cxt, found_node);

    ASTP(call)->macro_decl = found_node;
    push_macro_breadcrumb(ASTP(call));

    array_traverse(macro_expand_stack, it) {
        if (*it == found_node) {
            report_range_err(&call_loc, "encountered a recursive macro situation");
        }
    }

    array_push(macro_expand_stack, found_node);

    macro = (ast_macro_t*)((ast_decl_t*)found_node)->val_expr;

    if (call->block != NULL && !macro->is_block_macro) {
        report_loc_err_no_exit(call->block->loc.beg,
                               "'%s' is not a block macro, but is being used like one", get_string(found_name));
        report_fixit_no_exit(ASTP(call)->loc.beg,
                             "use parentheses around arguments instead of square brackets...\a%s!(...)", get_string(found_name));
        report_fixit(call->block->loc.beg, "... and remove the block");
    }
    if (macro->is_block_macro && call->block == NULL) {
        report_range_err_no_exit(&ASTP(call)->loc,
                                 "'%s' is a block macro, but is not being used like one", get_string(found_name));
        report_fixit_no_exit(ASTP(call)->loc.beg,
                             "use square brackets around arguments instead of parentheses...\a%s![...]", get_string(found_name));
        report_fixit(ASTP(call)->loc.end, "... and add a block argument\a{ ... }");
    }
    if (array_len(arg_list->args) < array_len(macro->param_names)) {
        report_loc_err(ASTP(arg_list)->loc.end,
                       "too few arguments in macro expansion");
    }
    if (array_len(arg_list->args) > array_len(macro->param_names)) {
        arg_p = array_item(arg_list->args, array_len(macro->param_names));
        report_range_err(&arg_p->expr->loc,
                         "too many arguments in macro expansion");
    }

    expected_kind     = call->expected_kind;
    macro_expand_args = array_make(ast_t*);

    if (array_len(((ast_block_t*)macro->block)->stmts) == 1) {
        new_node = copy_tree_collect_macro_expand_args(
                    *(ast_t**)array_item(((ast_block_t*)macro->block)->stmts, 0),
                    call->scope->parent,
                    &macro_expand_args);
    } else {
        new_node                         = AST_ALLOC(ast_block_t);
        new_node->kind                   = AST_BLOCK;
        new_node->flags                 |= AST_FLAG_SYNTHETIC_BLOCK;
        ((ast_block_t*)new_node)->scope  = cxt.scope;
        ((ast_block_t*)new_node)->stmts  = array_make(ast_t*);

        array_traverse(((ast_block_t*)macro->block)->stmts, it) {
            new_stmt = copy_tree_collect_macro_expand_args(*it, call->scope, &macro_expand_args);
            array_push(((ast_block_t*)new_node)->stmts, new_stmt);
        }
    }

    new_node->loc        = call_loc;
    new_node->macro_decl = found_node;


    array_traverse(macro_expand_args, it) {
        expand_arg = *it;

        if (expand_arg->kind == AST_MACRO_BLOCK_ARG_EXPAND) {
            goto block;
        }

        expand_arg_loc.beg = expand_arg->loc.beg;

        switch (expand_arg->kind) {
#define X(k) case k:
                X_AST_DECLARATIONS
#undef X
                name               = ((ast_decl_t*)expand_arg)->name;
                expand_arg_loc.end = ((ast_decl_t*)expand_arg)->name_end;
                break;
            case AST_PARAM:
                name               = ((ast_param_t*)expand_arg)->name;
                expand_arg_loc.end = ((ast_param_t*)expand_arg)->name_end;
                break;
            case AST_MACRO_ARG_EXPAND:
                name               = ((ast_macro_arg_expand_t*)expand_arg)->name;
                expand_arg_loc.end = expand_arg->loc.end;
                break;

            default:
                ASSERT(0, "bad node kind for macro expand arg");
        }

        idx = 0;
        array_traverse(macro->param_names, param_name_it) {
            if (*param_name_it == name) {
                goto found;
            }
            idx += 1;
        }

        pop_breadcrumb();
        report_range_err(&expand_arg_loc, "macro '%s' does not define a parameter named '%s'", found_name, name);

found:;
        arg_p = array_item(arg_list->args, idx);

block:;
        switch (expand_arg->kind) {
/* #define X(k) case k: */
/*                 X_AST_DECLARATIONS */
/* #undef X */
/*                 break; */

            case AST_MACRO_ARG_EXPAND:
                REPLACE_NODE(expand_arg, copy_tree(arg_p->expr, call->scope));
                break;
            case AST_MACRO_BLOCK_ARG_EXPAND:
                REPLACE_NODE(expand_arg, copy_tree(call->block, call->scope));
                break;

            default:
                ASSERT(0, "bad node kind for macro expand arg");
        }
    }

    macro_calls = array_make(ast_t*);

    collect_macro_calls(new_node, &macro_calls);

    array_traverse(macro_calls, it) {
        expand_macro((ast_macro_call_t*)*it);
    }

    array_free(macro_calls);

    pop_breadcrumb();

    REPLACE_NODE(ASTP(call), new_node);

    switch (new_node->kind) {
#define X(k) case k:
                X_AST_ALL_EXPRS
#undef X
        err_str = "n expression";
                    break;
#define X(k) case k:
                X_AST_ONLY_STATEMENTS
#undef X
        err_str = " statement";
                    break;
#define X(k) case k:
                X_AST_DECLARATIONS
#undef X
        err_str = " declaration";
                    break;
        default:
            ASSERT(0, "should not get here");
    }

    switch (expected_kind) {
        case MACRO_EXPR:
            switch (new_node->kind) {
#define X(k) case k:
                X_AST_ALL_EXPRS
#undef X
                    break;

                default:
                    report_range_err_no_exit(&call_loc,
                                             "macro '%s' is being used in a context that expects an expression, but expands to a%s",
                                             get_string(found_name),
                                             err_str);
                    report_range_info(&ASTP(macro)->loc,
                                      "expansion to a%s, as defined in '%s' here:",
                                      err_str,
                                      get_string(found_name));
            }
            break;

        case MACRO_STMT:
            switch (new_node->kind) {
#define X(k) case k:
                X_AST_STATEMENTS
#undef X
                    break;

                default:
                    report_range_err_no_exit(&call_loc,
                                             "macro '%s' is being used in a context that expects a statement, but expands to a%s",
                                             get_string(found_name),
                                             err_str);
                    report_range_info(&ASTP(macro)->loc,
                                      "expansion to a%s, as defined in '%s' here:",
                                      err_str,
                                      get_string(found_name));
            }
            break;

        case MACRO_DECL:
            switch (new_node->kind) {
#define X(k) case k:
                X_AST_DECLARATIONS
#undef X
                    break;

                default:
                    report_range_err_no_exit(&call_loc,
                                             "macro '%s' is being used in a context that expects a declaration, but expands to a%s",
                                             get_string(found_name),
                                             err_str);
                    report_range_info(&ASTP(macro)->loc,
                                      "expansion to a%s, as defined in '%s' here:",
                                      err_str,
                                      get_string(found_name));
            }
            break;

        default:
            ASSERT(0, "bad expected macro kind");
    }

    ASTP(call)->macro_decl = found_node;
    push_macro_breadcrumb(ASTP(call));

    if (expected_kind == MACRO_DECL) {
        switch (cxt.scope->kind) {
            case AST_DECL_MACRO:
                array_push(((ast_module_t*)(((ast_decl_t*)cxt.scope->node)->val_expr))->children, new_node);
                goto install;
                break;
            case AST_DECL_STRUCT:
                if (new_node->kind == AST_DECL_VAR
                &&  new_node->flags & AST_FLAG_CONSTANT) {

                    new_node->kind = AST_DECL_STRUCT_FIELD;
                    array_push(((ast_struct_t*)(((ast_decl_t*)cxt.scope->node)->val_expr))->fields, new_node);
                } else {
                    array_push(((ast_struct_t*)(((ast_decl_t*)cxt.scope->node)->val_expr))->children, new_node);
                    goto install;
                }
                break;
            default:
install:;
                add_symbol(cxt.scope, ((ast_decl_t*)new_node)->name, new_node);
        }
    }

    pop_breadcrumb();

    array_free(macro_expand_args);

    array_pop(macro_expand_stack);
}

void check_all(void) {
    check_context_t   cxt;
    ast_t           **rootp;
    scope_t          *entry_scope;

    if (array_len(roots) == 0) {
        report_simple_err("no meaningful input provided");
        return;
    }

    poly_backlog = array_make(poly_backlog_entry_t);

    memset(&cxt, 0, sizeof(cxt));

    cxt.scope = global_scope;

    array_traverse(roots, rootp) {
        check_node(cxt, *rootp);
    }

    check_poly_backlog();

    array_free(poly_backlog);

    if (program_entry == NULL) {
        report_simple_err("at least one procedure must be tagged as 'program_entry'");
        return;
    }

    entry_scope = get_subscope_from_node(global_scope, program_entry->val_expr);
    if (entry_scope               == NULL
    ||  entry_scope->parent       == NULL
    ||  entry_scope->parent->kind != AST_GLOBAL_SCOPE) {

        EMBC(ASTP(program_entry), {
            report_range_err(&ASTP(program_entry)->loc,
                             "'program_entry' procedure must be in global scope");
        });
        return;
    }
}

void multiple_entry_error(ast_decl_t *new, ast_decl_t *old) {
    EMBC(ASTP(new), {
        report_range_err_no_exit(&ASTP(new)->loc, "only one procedure can be delcared as 'program_entry'");
        report_range_info(&ASTP(old)->loc, "another 'program_entry' procedure defined here:");
    });
}

int tag_is_string(ast_t *tag_expr, string_id id) {
    if (tag_expr->kind != AST_IDENT) { return 0; }
    return ((ast_ident_t*)tag_expr)->str_rep == id;
}

string_id value_to_string_id(value_t val, u32 type) {
    string_id   ret;
    char        buff[128];
    int         ast_kind;
    ast_decl_t *decl;

    ret = 0;

    switch (type_kind(type)) {
        case TY_POLY: goto unknown;
        case TY_GENERIC_INT:
            switch (type) {
                case TY_U8:
                case TY_U16:
                case TY_U32:
                case TY_U64:
                case TY_GENERIC_POSITIVE_INT:
                    snprintf(buff, sizeof(buff), "%"PRIu64, val.u);
                    ret = get_string_id(buff);
                    break;
                case TY_S8:
                case TY_S16:
                case TY_S32:
                case TY_S64:
                case TY_GENERIC_NEGATIVE_INT:
                    snprintf(buff, sizeof(buff), "%"PRIi64, val.i);
                    ret = get_string_id(buff);
                    break;
            }
            break;
        case TY_GENERIC_FLOAT:
            switch (type) {
                case TY_F32:
                case TY_F64:
                    snprintf(buff, sizeof(buff), "%f", val.f);
                    ret = get_string_id(buff);
                    break;
            }
            break;
        case TY_PTR:
            snprintf(buff, sizeof(buff), "%p", val.v);
            ret = get_string_id(buff);
            break;
        case TY_TYPE:
            ret = get_type_string_id(val.t);
            break;
        case TY_MODULE: ast_kind = AST_MODULE; decl = ((ast_module_t*)val.a)->parent_decl; goto ast;
        case TY_PROC:   ast_kind = AST_PROC;   decl = ((ast_proc_t*)val.a)->parent_decl;   goto ast;
        case TY_STRUCT: ast_kind = AST_STRUCT; decl = ((ast_struct_t*)val.a)->parent_decl; goto ast;
ast:;
/*             snprintf(buff, sizeof(buff), "%s at %p", ast_get_kind_str(ast_kind), val.a); */
            (void)ast_kind;
            snprintf(buff, sizeof(buff), "%s", get_string(decl->full_name));
            ret = get_string_id(buff);
            break;
        default:
unknown:;
            ret = get_string_id("???");
            break;
    }

    ASSERT(ret != 0, "oops");

    return ret;
}

int ast_kind_is_decl(int kind) {
    return
#define X(k) kind == (k) ||
    X_AST_DECLARATIONS
#undef X
    0;
}

int ast_kind_is_leaf_expr(int kind) {
    return
#define X(k) kind == (k) ||
    X_AST_LEAF_EXPRS
#undef X
    0;
}

static const char *ast_kind_to_name[] = {
#define X(kind, name) #kind,
X_AST
#undef X
};

const char *ast_get_kind_str(int kind) {
    return ast_kind_to_name[kind] + 4;
}

u64 ast_size_table[] = {
#define X(kind, name) [kind] = sizeof(ast_##name##_t),
X_AST
#undef X
};

static array_t get_declaration_path(ast_ident_t *ident) {
    array_t     path;
    ast_t      *resolved_node;
    ast_decl_t *decl;
    ast_t      *decl_ast;

    path = array_make(ast_t*);

    while (ident->resolved_node != NULL) {
        resolved_node = ident->resolved_node;

again:;
        switch (resolved_node->kind) {
            case AST_BUILTIN:
                array_push(path, resolved_node);
                goto out;
                break;
            case AST_POLYMORPHIC_CONSTANT:
                resolved_node = resolved_node->value.a;
                goto again;
                break;
            case AST_PROC:
                decl = (ast_decl_t*)((ast_proc_t*)resolved_node)->parent_decl;
                break;
            case AST_STRUCT:
                decl = (ast_decl_t*)((ast_struct_t*)resolved_node)->parent_decl;
                break;
            case AST_MODULE:
                decl = (ast_decl_t*)((ast_module_t*)resolved_node)->parent_decl;
                break;
            default:
                ASSERT(ast_kind_is_decl(resolved_node->kind),
                    "resolved_node is not a declaration");
                decl = (ast_decl_t*)resolved_node;
                break;
        }

/*         ASSERT(decl->val_expr != NULL, "decl has no val"); */
        if (decl->val_expr == NULL) {
            array_clear(path);
            return path;
        }

        decl_ast = ASTP(decl);
        array_push(path, decl_ast);

        if (ASTP(decl)->kind      != AST_DECL_VAR
        ||  decl->val_expr->kind  != AST_IDENT) {

            break;
        }

        ident = (ast_ident_t*)decl->val_expr;
    }
out:;

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


static monomorphed_t *get_monomorph(check_context_t cxt, ast_t *node, array_t *polymorphs, array_t *params, poly_arg_t *args, u32 n_args, src_range_t args_loc, u32 *idx_out);
static u32 get_proc_monomorph_type(check_context_t cxt, ast_proc_t *proc, poly_arg_t *args, u32 n_args, src_range_t args_loc, u32 *idx_out);
static u32 get_proc_monomorph_type_and_push_breadcrumb_for_call(check_context_t cxt, ast_proc_t *proc, poly_arg_t *args, u32 n_args, src_range_t args_loc, u32 *idx_out);
static u32 get_struct_monomorph_type(check_context_t cxt, ast_struct_t *st, poly_arg_t *args, u32 n_args, src_range_t args_loc, u32 *idx_out);


typedef void (*tag_check_fn)(check_context_t cxt, ast_decl_t*, ast_t*, ast_arg_list_t *args);

enum {
    TAG_IDENT,
    TAG_CALL,
};

typedef struct {
    string_id    name;
    tag_check_fn check_fn;
    u32          arity;
    u32          form;
} Tag_Info;

#define GET_TAG_ARG(_args, _idx) (((arg_t*)array_item((_args)->args, (_idx)))->expr)

void check_tag_program_entry(check_context_t cxt, ast_decl_t *decl, ast_t *tag, ast_arg_list_t *arg_list) {
    ast_proc_t *proc;

    proc = (ast_proc_t*)(ASTP(decl)->kind == AST_DECL_PROC ? decl->val_expr : NULL);

    if (proc == NULL) {
        report_range_err(&ASTP(decl)->loc, "'%s' is tagged 'program_entry', but is not a procedure", get_string(decl->name));
        return;
    }

    if (program_entry != NULL && program_entry != decl) {
        multiple_entry_error(decl, program_entry);
        return;
    }

    program_entry = decl;
}

void check_tag_extern(check_context_t cxt, ast_decl_t *decl, ast_t *tag, ast_arg_list_t *arg_list) {
    if (ASTP(decl)->kind != AST_DECL_PROC && ASTP(decl)->kind != AST_DECL_VAR) {
        report_range_err(&ASTP(decl)->loc, "'%s' is tagged 'extern', but is not a procedure or a variable", get_string(decl->name));
        return;
    }

    ASTP(decl)->flags |= AST_FLAG_IS_EXTERN;

    if (ASTP(decl)->kind == AST_DECL_PROC) {
        decl->val_expr->flags |= AST_FLAG_IS_EXTERN;
    }
}

void check_tag_specialization(check_context_t cxt, ast_decl_t *decl, ast_t *tag, ast_arg_list_t *arg_list) {
    ast_t         *arg;
    ast_ident_t   *arg_ident;
    ast_decl_t    *arg_decl;
    ast_proc_t    *proc;
    ast_proc_t    *arg_proc;
    u32            n_pargs;
    poly_arg_t    *pargs;
    int            i;
    ast_param_t   *param;
    poly_arg_t    *parg;
    u32            idx;
    monomorphed_t *monomorphed;

    arg = GET_TAG_ARG(arg_list, 0);

    if (arg->kind != AST_IDENT) {
        report_range_err(&arg->loc, "tag 'specialization' argument must be an identifier");
    }

    arg_ident = (ast_ident_t*)arg;

    ASSERT(arg_ident->resolved_node != NULL, "ident not resolved");
    ASSERT(ast_kind_is_decl(arg_ident->resolved_node->kind), "ident does not resolve to a declaration");

    arg_decl = (ast_decl_t*)arg_ident->resolved_node;

    if (!(ASTP(arg_decl)->flags & AST_FLAG_POLYMORPH)) {
        EMBC(ASTP(arg_decl), {
            report_range_err_no_exit(&arg->loc, "can't make '%s' a specialization of something not polymorphic", get_string(decl->name));
            report_range_info_no_context(&ASTP(arg_decl)->loc, "'%s', which is not polymorphic, is declared here", get_string(arg_decl->full_name));
        });
        return;
    }

    if (ASTP(decl)->flags & AST_FLAG_POLYMORPH) {
        EMBC(ASTP(decl), {
            report_range_err_no_exit(&ASTP(decl)->loc, "specializations may not be polymorphic");
            report_range_info_no_context(&tag->loc, "'%s' tagged as a specialization of '%s' here", get_string(decl->name), get_string(arg_decl->full_name));
        });
        return;
    }

    if (ASTP(decl)->kind == AST_DECL_PROC && ASTP(decl)->kind != AST_DECL_PROC) {
        EMBC(ASTP(decl), {
            report_range_err_no_exit(&ASTP(decl)->loc, "specialization of a procedure must also be a procedure");
            report_range_info_no_context(&tag->loc, "'%s' tagged as a specialization of procedure '%s' here", get_string(decl->name), get_string(arg_decl->full_name));
        });
        return;
    }

    if (ASTP(decl)->kind == AST_DECL_STRUCT && ASTP(decl)->kind != AST_DECL_STRUCT) {
        EMBC(ASTP(decl), {
            report_range_err_no_exit(&ASTP(decl)->loc, "specialization of a struct must also be a struct");
            report_range_info_no_context(&tag->loc, "'%s' tagged as a specialization of struct '%s' here", get_string(decl->name), get_string(arg_decl->full_name));
        });
        return;
    }


    if (decl->val_expr->kind == AST_PROC) {
        proc     = (ast_proc_t*)decl->val_expr;
        arg_proc = (ast_proc_t*)arg_decl->val_expr;
        n_pargs  = array_len(proc->params);
        pargs    = alloca(sizeof(*pargs) * n_pargs);

        for (i = 0; i < n_pargs; i += 1) {
            param           = *(ast_param_t**)array_item(proc->params, i);
            parg            = pargs + i;
            parg->value     = ASTP(param)->value;
            parg->type      = ASTP(param)->type;
            parg->node      = ASTP(param);
            parg->has_value = param->val != NULL;
        }

        push_range_breadcrumb(&tag->loc,
                              "when attempting to make '%s' a specialization of '%s'",
                              get_string(decl->full_name),
                              get_string(arg_decl->full_name));

        cxt.parent_decl  = arg_decl;
        cxt.flags       |= CHECK_FLAG_SPECIALIZATION;
        get_proc_monomorph_type(cxt, arg_proc, pargs, n_pargs, proc->params_loc, &idx);

        pop_breadcrumb();

        monomorphed = array_item(arg_proc->monomorphs, idx);

        if (monomorphed->specialization != NULL) {
            EMBC(monomorphed->specialization, {
                report_range_err_no_exit(&tag->loc, "specialization of '%s' with these parameters is already specified", get_string(arg_decl->full_name));
                report_range_info_no_context(&monomorphed->specialization->loc, "previous specialization here:");
            });
        }

        monomorphed->specialization = ASTP(decl);

    } else if (decl->val_expr->kind == AST_STRUCT) {
        ASSERT(0, "unimplemented");
    } else {
        ASSERT(0, "bad node kind in check_specialization");
    }
}

void check_tag_bitfield_struct(check_context_t cxt, ast_decl_t *decl, ast_t *tag, ast_arg_list_t *arg_list) {
    ast_struct_t *st;
    ast_t        *arg;

    st = (ast_struct_t*)(ASTP(decl)->kind == AST_DECL_STRUCT ? decl->val_expr : NULL);

    if (st == NULL) {
        report_range_err(&((ast_bin_expr_t*)tag)->left->loc, "tag 'bitfield_struct' only applies to structs");
        return;
    }

    arg = GET_TAG_ARG(arg_list, 0);

    if (arg->type != TY_TYPE) {
        report_range_err_no_exit(&arg->loc, "tag 'bitfield_struct' argument must have type type");
        report_simple_info("got %s", get_string(get_type_string_id(arg->type)));
        return;
    }

    if (type_kind(arg->value.t) != TY_GENERIC_INT) {
        report_range_err_no_exit(&arg->loc, "tag 'bitfield_struct' argument must be an unsigned integer type");
        report_simple_info("have type %s", get_string(get_type_string_id(arg->value.t)));
        return;
    }

    switch (arg->value.t) {
        case TY_S8:
        case TY_S16:
        case TY_S32:
        case TY_S64:
            report_range_err_no_exit(&arg->loc, "tag 'bitfield_struct' argument must be an unsigned integer type");
            report_simple_err("have type %s", get_string(get_type_string_id(arg->value.t)));
            return;
            break;
        case TY_U8:  st->bitfield_struct_bits = 8;  break;
        case TY_U16: st->bitfield_struct_bits = 16; break;
        case TY_U32: st->bitfield_struct_bits = 32; break;
        case TY_U64: st->bitfield_struct_bits = 64; break;
        default:
            ASSERT(0, "huh?");
            break;
    }
}

void check_tag_bitfield(check_context_t cxt, ast_decl_t *decl, ast_t *tag, ast_arg_list_t *arg_list) {
    ast_struct_t *st;
    ast_t        *arg;
    u64           i;
    u64           bits[2];

    st = (ast_struct_t*)cxt.unit_decl->val_expr;

    if (!st->bitfield_struct_bits) {
        report_range_err(&tag->loc,
                            "field '%s' is tagged with 'bitfield', but its parent struct '%s' is not a 'bitfield_struct'",
                            get_string(decl->name),
                            get_string(cxt.unit_decl->name));
        return;
    }

    for (i = 0; i < 2; i += 1) {
        arg = GET_TAG_ARG(arg_list, i);

        if (arg->kind != AST_INT
        ||  arg->value.s < 0
        ||  arg->value.s >= st->bitfield_struct_bits) {

            report_range_err_no_exit(&arg->loc, "tag 'bitfield' argument must be a valid bit position");
            report_simple_info("valid bit positions are 0-%d", st->bitfield_struct_bits - 1);
            return;
        }

        bits[i] = arg->value.u;
    }

    if (bits[1] < bits[0]) {
        report_range_err(&arg->loc, "second argument of tag 'bitfield' must be greater than or equal to the first");
        return;
    }

    for (i = 0; i < st->bitfield_struct_bits; i += 1) {
        if (i >= bits[0] && i <= bits[1]) {
            decl->bitfield_mask |= 1ULL << i;
        }
    }

    decl->bitfield_shift = bits[0];
}

void init_tags(void) {

    tag_infos = array_make(Tag_Info);

#define ADD_TAG_INFO(_id, _check_fn, _arity, _form)              \
{                                                                \
    Tag_Info _info = { (_id), (_check_fn), (_arity), (_form) };  \
    array_push(tag_infos, _info);                                \
}

    ADD_TAG_INFO(PROGRAM_ENTRY_ID,   check_tag_program_entry,   0, TAG_IDENT);
    ADD_TAG_INFO(EXTERN_ID,          check_tag_extern,          0, TAG_IDENT);
    ADD_TAG_INFO(SPECIALIZATION_ID,  check_tag_specialization,  1, TAG_CALL);
    ADD_TAG_INFO(BITFIELD_STRUCT_ID, check_tag_bitfield_struct, 1, TAG_CALL);
    ADD_TAG_INFO(BITFIELD_ID,        check_tag_bitfield,        2, TAG_CALL);
}


static void check_tag(check_context_t cxt, ast_decl_t *decl, ast_t *tag) {
    ast_ident_t    *ident;
    ast_bin_expr_t *expr;
    ast_arg_list_t *arg_list;
    u8              form;
    string_id       id;
    src_range_t     loc;
    Tag_Info       *info;

    ident    = NULL;
    expr     = NULL;
    arg_list = NULL;

    if (tag->kind == AST_IDENT) {
        form  = TAG_IDENT;
        ident = (ast_ident_t*)tag;
        id    = ident->str_rep;
        loc   = ASTP(ident)->loc;
    } else if (tag->kind == AST_BIN_EXPR) {
        expr = (ast_bin_expr_t*)tag;
        if (expr->left->kind != AST_IDENT
        ||  expr->op != OP_CALL
        ||  expr->right->kind != AST_ARG_LIST) {

            goto bad_form;
        }

        form     = TAG_CALL;
        ident    = (ast_ident_t*)expr->left;
        id       = ident->str_rep;
        loc      = ASTP(ident)->loc;
        arg_list = (ast_arg_list_t*)expr->right;
    } else {
bad_form:;
        report_range_err_no_exit(&tag->loc, "invalid tag expression");
        report_simple_info("tags may be of the form `tag` or `tag(arguments)`");
        return;
    }

    array_traverse(tag_infos, info) {
        if (info->name == id) { goto found; }
    }

    report_range_err(&loc, "unrecognized tag '%s'", get_string(id));
    return;

found:;

    if (form != info->form) {
        report_range_err(&tag->loc,
                         "incorrect form for tag '%s' (expected %s)",
                         get_string(id),
                         info->form == TAG_IDENT ? "bare identifier" : "procedure call style");
        return;
    }

    if (arg_list != NULL) {
        if (array_len(arg_list->args) != info->arity) {
            report_range_err(&ASTP(arg_list)->loc,
                             "tag '%s' expects exactly %d argument%s",
                             get_string(id),
                             info->arity,
                             info->arity == 1 ? "" : "s");
            return;
        }

        if (id == SPECIALIZATION_ID) {
            cxt.flags |= CHECK_FLAG_ALLOW_REF_POLY_PROC;
        }
        check_node(cxt, ASTP(arg_list));
        cxt.flags &= ~CHECK_FLAG_ALLOW_REF_POLY_PROC;
    }

    info->check_fn(cxt, decl, tag, arg_list);
}

static void check_tags(check_context_t cxt, ast_decl_t *decl, array_t *tags) {
    ast_t **it;

    array_traverse(*tags, it) {
        check_tag(cxt, decl, *it);
    }
}

static void check_for_decl_cycle(ast_t *node) {
    ast_t       **it;
    int           idx;
    ast_ident_t  *ident;
    int           last;

    ASSERT(ast_kind_is_decl(node->kind) || node->kind == AST_PARAM, "bad node kind");

#define CYCLE_NODE_NAME_ID(_node)                            \
    (ast_kind_is_decl((_node)->kind)                         \
        ? (((ast_decl_t*)(_node))->containing_scope->in_proc \
            ? ((ast_decl_t*)(_node))->name                   \
            : ((ast_decl_t*)(_node))->full_name)             \
        : ((ast_param_t*)(_node))->name)

    array_rtraverse(cycle_check_path, it) {
        if (*it == node) {
            report_range_err_no_context_no_exit(
                &node->loc,
                "declaration of '%s' introduces a dependency cycle!",
                get_string(CYCLE_NODE_NAME_ID(node)));

            idx = it - (ast_t**)array_data(cycle_check_path);

            array_traverse_from(cycle_check_path, it, idx + 1) {
                if ((*it)->kind == AST_IDENT) { goto found_ident1; }
            }
            ASSERT(0, "did not find subsequent ident in path");

found_ident1:;
            ident = (ast_ident_t*)*it;
            idx   = it - (ast_t**)array_data(cycle_check_path);
            ASSERT(ident->resolved_node != NULL, "should be resolved by now...");

            last = it == array_last(cycle_check_path);
            _report_range_info_no_context(last,
                &(*it)->loc,
                "'%s' depends on '%s'%s",
                get_string(CYCLE_NODE_NAME_ID(node)),
                get_string(CYCLE_NODE_NAME_ID(ident->resolved_node)),
                last ? " here, thus creating a loop!" : ":");
            if (last) { return; }




            for (;;) {
                it = array_item(cycle_check_path, idx + 1);
                ASSERT(ast_kind_is_decl((*it)->kind) || (*it)->kind == AST_PARAM,
                       "something not a decl after ident in path");
                node = *it;

                array_traverse_from(cycle_check_path, it, idx + 1) {
                    if ((*it)->kind == AST_IDENT) { goto found_ident2; }
                }
                ASSERT(0, "did not find subsequent ident in path");

found_ident2:;
                ident = (ast_ident_t*)*it;
                idx   = it - (ast_t**)array_data(cycle_check_path);
                ASSERT(ident->resolved_node != NULL, "should be resolved by now...");

                last = it == array_last(cycle_check_path);
                _report_range_info_no_context(last,
                    &(*it)->loc,
                    "... '%s' itself depends on '%s'%s",
                    get_string(CYCLE_NODE_NAME_ID(node)),
                    get_string(CYCLE_NODE_NAME_ID(ident->resolved_node)),
                    last ? " here, thus creating a loop!" : ":");
                if (last) { return; }
            }

            return;
        }
    }
}

#define CYCLE_PATH_PUSH(_node)              \
do {                                        \
    ast_t *__node = (_node);                \
    (array_push(cycle_check_path, __node)); \
} while (0)

#define CYCLE_PATH_POP() (array_pop(cycle_check_path))

static void check_decl(check_context_t cxt, ast_decl_t *decl) {
    int do_cycle_check;
    u32 val_t;
    u32 decl_t;

    do_cycle_check = cxt.parent_decl == NULL || ASTP(cxt.parent_decl)->kind != AST_DECL_STRUCT_FIELD;

    if (ASTP(decl)->kind == AST_DECL_MACRO) {
        if (do_cycle_check) {
            check_for_decl_cycle(ASTP(decl));
        }
        ASTP(decl)->type = TY_MACRO;
        return;
    }

    if (do_cycle_check) {
        check_for_decl_cycle(ASTP(decl));
        CYCLE_PATH_PUSH(ASTP(decl));
    }

    cxt.parent_decl = decl;

    ASSERT(decl->type_expr || decl->val_expr,
           "decl misssing both type and val");

    /*
     * For var/constant/field declarations, we can go ahead and check the tags.
     * Otherwise, things like structs and procs may need to do some other
     * stuff before they're ready to deal with tags. Let them decide when.
     */
    if (ASTP(decl)->kind == AST_DECL_VAR
    ||  ASTP(decl)->kind == AST_DECL_STRUCT_FIELD) {

        check_tags(cxt, decl, &decl->tags);
    }

    if (decl->type_expr != NULL) {
        check_node(cxt, decl->type_expr);

        if (decl->type_expr->type != TY_TYPE) {
            EMBC(decl->type_expr, {
                report_range_err_no_exit(&decl->type_expr->loc,
                                         "expression declaring type of '%s' is not a type",
                                         get_string(decl->name));
                report_simple_info("got %s instead of type", get_string(get_type_string_id(decl->type_expr->type)));
            });
        }

        decl_t = decl->type_expr->value.t;
    }

    if (decl->val_expr != NULL) {
        check_node(cxt, decl->val_expr);
        val_t = decl->val_expr->type;

        if (val_t == TY_NOT_TYPED) {
            EMBC(decl->val_expr, {
                report_range_err(&decl->val_expr->loc,
                                 "initialization expression of '%s' does not produce a value",
                                 get_string(decl->name));

            });
        }

        if (ASTP(decl)->flags & AST_FLAG_CONSTANT
        &&  !(decl->val_expr->flags & AST_FLAG_CONSTANT)) {
            if (ASTP(decl)->kind == AST_DECL_VAR) {
                EMBC(decl->val_expr, {
                    report_range_err_no_exit(&decl->val_expr->loc,
                                             "'%s' is declared as a constant, but has a non-constant initialization",
                                             get_string(decl->name));
                    report_fixit(ASTP(decl)->loc.beg,
                                "if you meant for '%s' to be a variable, use this syntax:\a%s :=    OR    %s: <type> =",
                                get_string(decl->name),
                                get_string(decl->name),
                                get_string(decl->name));
                });
            } else {
                EMBC(decl->val_expr, {
                    report_range_err(&decl->val_expr->loc,
                                     "'%s' is declared as a constant, but has a non-constant initialization",
                                     get_string(decl->name));
                });
            }
        }

        if (decl->type_expr == NULL && type_kind(val_t) == _TY_TYPE_LIST) {
            EMBC(decl->val_expr, {
                report_range_err(&decl->val_expr->loc,
                       "expression which represents a list of types is not allowed here");
            });
        }

        ASTP(decl)->value = decl->val_expr->value;
    }

    if (decl->type_expr == NULL) {
        decl_t = val_t;
    }

    if (cxt.scope->in_proc && decl_t == TY_PROC) {
        EMBC(ASTP(decl), {
            report_range_err(&ASTP(decl)->loc, "procedures may not be defined within another procedure");
        });
        return;
    }

    if (decl->type_expr != NULL && decl->val_expr != NULL) {
        if (!types_are_compatible(decl_t, val_t)) {
            EMBC(decl->val_expr, {
                report_range_err_no_exit(&decl->val_expr->loc,
                                         "initialization of '%s' does not match declared type of %s",
                                         get_string(decl->name),
                                         get_string(get_type_string_id(decl_t)));
                report_range_info_no_context(&decl->type_expr->loc,
                                             "expected %s, but got %s",
                                             get_string(get_type_string_id(decl_t)),
                                             get_string(get_type_string_id(val_t)));
            });
        }

        if (TYPE_IS_GENERIC(val_t)) {
            realize_generic(decl_t, decl->val_expr);
            val_t = decl->val_expr->type;
        }
    }

    if (ASTP(decl)->kind == AST_DECL_VAR
    &&  !(ASTP(decl)->flags & AST_FLAG_CONSTANT)
    &&  decl->val_expr != NULL) {

        if (TYPE_IS_GENERIC(val_t)) {
            if (TYPE_IS_GENERIC(decl_t)) {
                decl_t = TY_S64;
            }
            realize_generic(decl_t, decl->val_expr);
        }
    }

    ASTP(decl)->type = decl_t;

    if (!(ASTP(decl)->flags & AST_FLAG_CONSTANT)) {
        switch (decl_t) {
            case TY_MODULE:
            case TY_TYPE:
            case TY_MACRO:
                EMBC(ASTP(decl), {
                    report_range_err_no_exit(&ASTP(decl)->loc,
                                             "declaration of runtime variable '%s' has type '%s', which is only available at compile time",
                                             get_string(decl->name), get_string(get_type_string_id(decl_t)));
                    report_fixit(ASTP(decl)->loc.beg,
                                 "if you meant for '%s' to be a constant, use this syntax:\a%s ::",
                                 get_string(decl->name),
                                 get_string(decl->name));
                });
                break;
            default:;
        }
    }

    if (do_cycle_check) {
        CYCLE_PATH_POP();
    }

    if (!(cxt.flags & CHECK_FLAG_POLY_BACKLOG)
    &&  ASTP(decl)->kind == AST_DECL_VAR
    &&  !decl->containing_scope->in_proc) {

        array_push(all_vars, decl);
    }

    if (cxt.scope->in_proc) {
        add_symbol(cxt.scope, decl->name, ASTP(decl));
    }
}

u32 find_poly_vargs_type(scope_t *scope) {
    ast_t   *found;
    scope_t *found_in_scope;

    found = search_up_scopes_return_scope(scope, ELLIPSIS_ID, &found_in_scope);

    ASSERT(found                     != NULL, "did not find ...");
    ASSERT(found_in_scope            != NULL, "how could this even happen if we found the node???");
    ASSERT(found->kind               == AST_POLYMORPHIC_CONSTANT, "must be a poly constant");
    ASSERT(found_in_scope->kind      == AST_POLYMORPHIC_CONSTANTS_SCOPE, "must have come from poly constants");
    ASSERT(found->type               == TY_TYPE, "must have type type");
    ASSERT(type_kind(found->value.t) == _TY_TYPE_LIST, "must be a type list");

    return found->value.t;
}

static u32 current_poly_vargs_type(check_context_t cxt) {
    return find_poly_vargs_type(cxt.scope);
}

static void check_proc(check_context_t cxt, ast_proc_t *proc) {
    scope_t      *new_scope;
    u32           n_params;
    type_t        list_type;
    u32          *param_types;
    int           i;
    ast_t       **it;
    int           j;
    u32           ret_type;
    src_point_t   pt;

    new_scope = proc->scope;

    cxt.proc      = proc;
    cxt.unit_decl = cxt.parent_decl;

    cxt.scope = new_scope;

    n_params = array_len(proc->params);

    if (n_params > 0
    &&  cxt.flags & CHECK_FLAG_MONOMORPH
    &&  (*(ast_t**)array_last(proc->params))->flags & AST_FLAG_POLY_VARARGS) {

        list_type  = get_type_t(current_poly_vargs_type(cxt));
        n_params  -= 1;
        n_params  += list_type.list_len;
    }

    param_types = alloca(sizeof(u32) * n_params);
    i           = 0;
    array_traverse(proc->params, it) {
        check_node(cxt, *it);
        if (cxt.flags & CHECK_FLAG_MONOMORPH && (*it)->flags & AST_FLAG_POLY_VARARGS) {
            for (j = 0; j < list_type.list_len; j += 1) {
                param_types[i] = list_type.id_list[j];
                i += 1;
            }
            break;
        }
        param_types[i] = (*it)->type;
        i += 1;
    }

    if (proc->ret_type_expr != NULL) {
        if (ASTP(proc)->flags & AST_FLAG_POLYMORPH) {
            ret_type = TY_POLY;
        } else {
            check_node(cxt, proc->ret_type_expr);
            if (proc->ret_type_expr->type != TY_TYPE
            &&  proc->ret_type_expr->type != TY_POLY) {
                EMBC(proc->ret_type_expr, {
                    report_range_err_no_exit(&proc->ret_type_expr->loc,
                                             "expression must be a type since it declares the return type of procedure '%s'",
                                             get_string(cxt.parent_decl->name));
                    report_simple_info("got %s instead", get_string(get_type_string_id(proc->ret_type_expr->type)));
                });
                return;
            }

            ret_type = proc->ret_type_expr->value.t;
        }
    } else {
        ret_type = TY_NOT_TYPED;
    }

    ASTP(proc)->type = get_proc_type(n_params, param_types, ret_type);

    check_tags(cxt, cxt.parent_decl, &(cxt.parent_decl)->tags);

    if (ASTP(proc)->flags & AST_FLAG_POLYMORPH
    &&  !(cxt.flags & CHECK_FLAG_MONOMORPH)) {

        ASTP(proc)->flags |= AST_FLAG_CHECKED;
        array_push(all_procs, cxt.parent_decl);
        return;
    }
    if (cxt.flags & CHECK_FLAG_POLY_PROC_TYPE_ONLY) { return; }

    /* @bad?, @refactor
    ** We have to bubble this type up to the declaration so that identifier lookups
    ** that occur before we return from this routine can get the right type.
    ** I'm not sure if this should just be done everywhere like this (shouldn't
    ** be _that_ many spots), or if something a little smarter should be done.
    */
    if (!(cxt.flags & CHECK_FLAG_MONOMORPH)) {
        ASTP(cxt.parent_decl)->type  = ASTP(proc)->type;
        ASTP(cxt.parent_decl)->value = ASTP(proc)->value;
    }

    if (proc->block != NULL) {
        check_node(cxt, proc->block);

        if (ret_type != TY_NOT_TYPED && !(proc->block->flags & AST_FLAG_CF_MUST_RETURN)) {
            pt = ((ast_block_t*)proc->block)->end_brace_loc;
            report_loc_err(pt,
                           "procedure '%s' should return a value of type '%s', but control flow does not guarantee that a value is returned",
                           get_string(cxt.parent_decl->name),
                           get_string(get_type_string_id(ret_type)));
        }
    } else {
        ASSERT(ASTP(proc)->flags & AST_FLAG_IS_EXTERN,
            "proc is not extern, but has no body");
    }

    ASTP(proc)->flags |= AST_FLAG_CHECKED;

    /*
    ** We don't want to add more entries from the backlog.
    ** All appropriate declarations should already have been
    ** added by the time we're scraping up monomorphs.
    */
    if (!(cxt.flags & CHECK_FLAG_MONOMORPH)) {
        array_push(all_procs, cxt.parent_decl);
    }
}

static void check_param(check_context_t cxt, ast_param_t *param) {
    ast_t   *found;
    scope_t *found_in_scope;

    param->containing_scope = cxt.scope;

    check_for_decl_cycle(ASTP(param));
    CYCLE_PATH_PUSH(ASTP(param));

    cxt.flags |= CHECK_FLAG_IN_PARAM;

    if (ASTP(param)->flags & AST_FLAG_POLY_VARARGS) {
        if (cxt.flags & CHECK_FLAG_MONOMORPH) {
            ASTP(param)->type = current_poly_vargs_type(cxt);
        } else {
            ASTP(param)->type = get_vargs_type(TY_POLY);
        }
        goto out;
    }

    check_node(cxt, param->type_expr);

    if (param->type_expr->type == TY_POLY) {
        param->type_expr->type     = TY_TYPE;
        param->type_expr->value.t  = TY_POLY;
    }

    if (param->val != NULL) {
        check_node(cxt, param->val);
    }

    ASTP(param)->type = param->type_expr->value.t;

    if (ASTP(param)->flags & AST_FLAG_POLYMORPH) {
        if (ASTP(param)->type == TY_TYPE) {
            if (cxt.flags & CHECK_FLAG_MONOMORPH) {
                found = search_up_scopes_return_scope(cxt.scope, param->name, &found_in_scope);

                ASSERT(found                != NULL, "did not find polymorphic type param in constants");
                ASSERT(found_in_scope       != NULL, "how could this even happen if we found the node???");
                ASSERT(found->kind          == AST_POLYMORPHIC_CONSTANT, "must be a poly constant");
                ASSERT(found_in_scope->kind == AST_POLYMORPHIC_CONSTANTS_SCOPE, "must have come from poly constants");
                ASSERT(found->type          == TY_TYPE, "must have type type");

                ASTP(param)->value.t = found->value.t;
            } else {
                ASTP(param)->value.t = TY_POLY;
            }
        }
        ASTP(param)->flags |= AST_FLAG_CONSTANT;
    }

    if (param->val != NULL) {
        if (param->val->type != ASTP(param)->type) {
            EMBC(param->val, {
                report_range_err(&param->val->loc,
                                 "default value for '%s' has type %s, which does not match declared type of %s",
                                 get_string(param->name),
                                 get_string(get_type_string_id(param->val->type)),
                                 get_string(get_type_string_id(ASTP(param)->type)));
            });
        }
        ASTP(param)->value = param->val->value;
    }

out:;
    CYCLE_PATH_POP();
}

static void check_int(check_context_t cxt, ast_int_t *integer) {
    const char *s;

    s = get_string(integer->str_rep);

    if (s[0] == '0' && s[1] == 'x') {
/*         ASTP(integer)->flags   |= AST_FLAG_HEX_INT; */
        ASTP(integer)->value.u  = strtoull(s, NULL, 16);
        ASTP(integer)->type     = TY_GENERIC_POSITIVE_INT;
    } else if (s[0] == '-') {
        ASTP(integer)->value.s = -strtoll(s + 1, NULL, 10);
        ASTP(integer)->type     = TY_GENERIC_NEGATIVE_INT;
    } else {
        ASTP(integer)->value.u = strtoll(s, NULL, 10);
        ASTP(integer)->type     = TY_GENERIC_POSITIVE_INT;
    }
}

static void check_float(check_context_t cxt, ast_int_t *f) {
    ASTP(f)->type    = TY_GENERIC_FLOAT;
    ASTP(f)->value.f = strtod(get_string(f->str_rep), NULL);
}

static void check_string(check_context_t cxt, ast_string_t *string) {
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
                    case '\\':
                        break;
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

    ASTP(string)->value.s  = get_string_id_n(buff, new_len);
    ASTP(string)->type     = get_slice_type(TY_U8);
}

static void check_char(check_context_t cxt, ast_char_t *ch) {
    const char *contents;
    char        c;

    contents = get_string(ch->str_rep);

    ASSERT(contents[0] == '\'', "char doesn't start with quote");

    c = contents[1];

    if (c == '\\') {
        switch (contents[2]) {
            case '\\':
                break;
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
                report_range_err(&ASTP(ch)->loc, "unknown escape character '%c'", c);
                return;
        }
    }

    ASTP(ch)->value.u  = c;
    ASTP(ch)->type     = TY_U8;
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

static void check_builtin_special_call(check_context_t cxt, ast_bin_expr_t *expr) {
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

    expr->call_decl = builtin_origin;

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
            EMBC(arg_p->expr, {
                report_range_err_no_exit(&arg_p->expr->loc,
                                         "first argument in cast must be the destination type");
                report_simple_info("expected a type, but got %s",
                                   get_string(get_type_string_id(arg_p->expr->type)));
            });
        }

        ASTP(expr)->type = arg_p->expr->value.t;

        /* @todo -- do real value casting in expr->value */
        arg_p = array_item(arg_list->args, 1);
        ASTP(expr)->value = arg_p->expr->value;

        if (arg_p->expr->flags & AST_FLAG_CONSTANT) {
            ASTP(expr)->flags |= AST_FLAG_CONSTANT;
        }

        /* @todo -- check that the types can actually cast */

    } else if (builtin->name == _BUILTIN_VARG_ID) {
        if (!(cxt.flags & CHECK_FLAG_IN_VARGS)) {
            report_range_err(&ASTP(expr)->loc, "_builtin_varg() may only be used in a \\VARGS context");
/*             report_range_err_no_exit(&ASTP(expr)->loc, "_builtin_varg() may only be used in a \\VARGS context"); */
/*             report_fixit_no_exit(ASTP(expr)->loc.beg, "start a \\VARGS context\a\\VARGS"); */
/*             report_fixit(ASTP(expr)->loc.end, "end the \\VARGS context\a\\ENDVARGS"); */
        }

        if (array_len(arg_list->args) > 0) {
            arg_p = array_item(arg_list->args, 0);
            report_range_err_no_exit(&arg_p->expr->loc,
                                     "too many arguments for _builtin_varg()");
            report_simple_info("expected 0, but got %d", array_len(arg_list->args));
        }

        ASTP(expr)->type = cxt.varg_ty;
    } else if (builtin->name == _BUILTIN_SLICE_FROM_ID) {


        if (array_len(arg_list->args) < 2) {
            report_loc_err_no_exit(expr->right->loc.end,
                                   "too few arguments for _builtin_slice_from");
            report_simple_info("expected 2, but got %d", array_len(arg_list->args));
        }
        if (array_len(arg_list->args) > 2) {
            arg_p = array_item(arg_list->args, 2);
            report_range_err_no_exit(&arg_p->expr->loc,
                                     "too many arguments for _builtin_slice_from");
            report_simple_info("expected 2, but got %d", array_len(arg_list->args));
        }

        arg_p = array_item(arg_list->args, 0);
        if (type_kind(arg_p->expr->type) != TY_PTR) {
            EMBC(arg_p->expr, {
                report_range_err_no_exit(&arg_p->expr->loc,
                                         "first argument of _builtin_slice_from must be a pointer");
                report_simple_info("expected a pointer type, but got %s",
                                   get_string(get_type_string_id(arg_p->expr->type)));
            });
        }

        arg_p = array_item(arg_list->args, 1);
        if (TYPE_IS_GENERIC(arg_p->expr->type)) {
            realize_generic(TY_S64, arg_p->expr);
        }
        if (arg_p->expr->type != TY_S64) {
            EMBC(arg_p->expr, {
                report_range_err_no_exit(&arg_p->expr->loc,
                                         "second argument of _builtin_slice_from must be of type s64");
                report_simple_info("expected s64, but got %s",
                                   get_string(get_type_string_id(arg_p->expr->type)));
            });
        }


        arg_p = array_item(arg_list->args, 0);

        ASTP(expr)->flags &= ~AST_FLAG_CONSTANT;
        ASTP(expr)->type   = get_slice_type(get_under_type(arg_p->expr->type));
    } else {
        ASSERT(0, "unhandled builtin special");
    }

    array_free(path);
}

static void solve_poly_type_expr(array_t *constants, ast_t *type_expr, poly_arg_t *arg) {
    ast_t               *texpr;
    u32                  at;
    u32                  pt;
    ast_poly_constant_t  constant;
/*     ast_t               *child; */

    texpr = type_expr;
    at    = arg->type;

    for (;;) {
        pt = texpr->value.t;

        if (ast_kind_is_leaf_expr(texpr->kind)) {
            ASSERT(texpr->kind == AST_IDENT, "not an ident");

            memset(&constant, 0, sizeof(constant));
            constant.ast.kind    = AST_POLYMORPHIC_CONSTANT;
            constant.ast.flags   = AST_FLAG_CONSTANT;
            constant.ast.type    = TY_TYPE;
            constant.ast.value.t = at;
            constant.ast.loc     = texpr->loc;
            constant.name        = ((ast_ident_t*)texpr)->str_rep;

            array_push(*constants, constant);
/*             report_range_info_no_context_no_exit( */
/*                 &texpr->loc, "CASE 3: %s resolved to %s", */
/*                 get_string(constant.name), */
/*                 get_string(value_to_string_id(constant.value, constant.type))); */

            break;
        }


        if (texpr->kind == AST_BIN_EXPR && ((ast_bin_expr_t*)texpr)->op == OP_CALL) {
            EMBC(type_expr, {
                report_range_err_no_exit(&type_expr->loc,
                                        "invalid polymorphic type expression");
                report_simple_info("parameterization not allowed here");
            });
            return;
        } else {
            if (type_kind(at) != type_kind(pt)) { goto err; }

            switch (type_kind(pt)) {
                case TY_PTR:
                case TY_SLICE:
                    texpr = ((ast_unary_expr_t*)texpr)->child;
                    at    = get_under_type(at);
                    break;
                default:
                    if (at != pt) {
err:;
                        EMBC(arg->node, {
                            report_range_err_no_exit(&arg->node->loc,
                                                     "incorrect argument type: expected %s, but got %s",
                                                     get_string(get_type_string_id(type_expr->value.t)),
                                                     get_string(get_type_string_id(arg->type)));
                            report_range_info_no_context(&type_expr->loc,
                                                         "when solving for polymorphic parameters in a type pattern");
                        });
                        return;
                    }
            }
        }
    }
}

static void verify_polymorphic_args(u32 cxt_flags, array_t *params, poly_arg_t *args, u32 n_args, src_range_t params_loc, src_range_t args_loc) {
    int                    is_poly_varg;
    src_range_t            loc;
    int                    i;
    ast_t                **it;
    ast_param_t           *param;
    poly_arg_t            *arg;

    is_poly_varg = !!(ASTP(*(ast_param_t**)array_last(*params))->flags & AST_FLAG_POLY_VARARGS);

    if (array_len(*params) - is_poly_varg > n_args) {
        report_loc_err_no_exit(args_loc.end, "too few arguments to solve for polymorphic constants");
        report_simple_info_no_exit("expected %s%d argument%s, but only got %d",
                                    is_poly_varg ? "at least " : "",
                                    array_len(*params) - is_poly_varg,
                                    array_len(*params) - is_poly_varg > 1 ? "s" : "",
                                    n_args);
        report_range_info_no_context(&params_loc, "polymorphic parameters defined here:");
    }

    if (!is_poly_varg && n_args > array_len(*params)) {
        loc.beg = args[array_len(*params)].node->loc.beg;
        loc.end = args_loc.end;
        report_range_err_no_exit(&loc,
                                 "too many arguments to solve for polymorphic constants");
        report_simple_info_no_exit("expected %s%d argument%s, but got %d",
                                    is_poly_varg ? "at least " : "",
                                    array_len(*params),
                                    array_len(*params) > 1 ? "s" : "",
                                    n_args);
        report_range_info_no_context(&params_loc, "polymorphic parameters defined here:");
    }

    i = 0;
    array_traverse(*params, it) {
        param = (ast_param_t*)*it;
        arg   = args + i;

        if (ASTP(param)->flags & AST_FLAG_POLYMORPH && !(arg->node->flags & AST_FLAG_CONSTANT)) {
            EMBC(arg->node, {
                report_range_err_no_exit(arg->node->kind == AST_PARAM ? &(((ast_param_t*)arg->node)->type_expr->loc) : &arg->node->loc,
                                         "argument to polymorphic parameter '%s' is not a constant expression",
                                          get_string(param->name),
                                          get_string(get_type_string_id(ASTP(param)->type)));
                report_range_info_no_context(&param->type_expr->loc,
                                        	 "%sparameter '%s' delcared here:",
                                             (ASTP(param)->flags & AST_FLAG_POLYMORPH) ? "polymorphic " : "",
                                             get_string(param->name));
            });
            return;
        }

        if (ASTP(param)->flags & AST_FLAG_POLY_VARARGS) {
            break;
        }

        /*
         * All parameters of a struct must be polymorphic, so this
         * should not be hit if we're solving a struct. For procedures,
         * there is extra extensive arg/param type checking in
         * check_call() that will take care of these. By skipping them
         * here, we give procedure calls the opportunity to provide
         * better error messages for non-polymorphic parameters, even
         * if the procedure itself is polymorphic. We need to handle
         * all params of a struct though.
         */
        if (!(ASTP(param)->flags & AST_FLAG_POLYMORPH)
        &&  !(cxt_flags & CHECK_FLAG_SPECIALIZATION)) {
            continue;
        }

/*         if (ASTP(param)->type != TY_POLY */
/*         &&  !(param->type_expr->flags & AST_FLAG_POLYMORPH) */
        if (!type_is_poly(ASTP(param)->type)
        &&  !types_are_compatible(ASTP(param)->type, arg->type)) {

            EMBC(arg->node, {
                report_range_err_no_exit(arg->node->kind == AST_PARAM ? &(((ast_param_t*)arg->node)->type_expr->loc) : &arg->node->loc,
                                         "incorrect argument type: expected %s, but got %s",
                                         get_string(get_type_string_id(ASTP(param)->type)),
                                         get_string(get_type_string_id(arg->type)));
                report_range_info_no_context(&param->type_expr->loc,
                                        	 "%sparameter '%s' delcared as type %s here:",
                                             (ASTP(param)->flags & AST_FLAG_POLYMORPH) ? "polymorphic " : "",
                                             get_string(param->name),
                                             get_string(get_type_string_id(ASTP(param)->type)));
            });

        }

        i += 1;
    }
}

static void extract_polymorph_constants_from_arg(array_t *constants, int i, ast_param_t *param, poly_arg_t *args, u32 n_args) {
    u32                  n_vargs;
    u32                 *varg_types;
    int                  j;
    u32                  varg_list_ty;
    poly_arg_t          *arg;
    ast_poly_constant_t  constant;

    if (ASTP(param)->flags & AST_FLAG_POLY_VARARGS) {
        n_vargs    = n_args - i;
        varg_types = alloca(sizeof(u32) * n_vargs);

        for (j = 0; j < n_vargs; j += 1) {
            arg = args + i + j;

            if (TYPE_IS_GENERIC(arg->type)) {
                force_generic_realization(arg->node);
                arg->type = arg->node->type;
            }

            varg_types[j] = arg->type;
        }

        varg_list_ty = get_type_list_type(n_vargs, varg_types);

        memset(&constant, 0, sizeof(constant));
        constant.ast.kind    = AST_POLYMORPHIC_CONSTANT;
        constant.ast.flags   = AST_FLAG_CONSTANT;
        constant.ast.type    = TY_TYPE;
        constant.ast.value.t = varg_list_ty;
        constant.ast.loc     = ASTP(param)->loc;
        constant.name        = ELLIPSIS_ID;

        array_push(*constants, constant);
/*         report_range_info_no_context_no_exit( */
/*             &ASTP(param)->loc, "CASE 3: %s resolved to %s", */
/*             get_string(constant.name), */
/*             get_string(value_to_string_id(constant.value, constant.type))); */
    } else if (ASTP(param)->flags & AST_FLAG_POLYMORPH) {

        arg = args + i;

        if (TYPE_IS_GENERIC(arg->type)) {
            if (!type_is_poly(ASTP(param)->type)
            &&  type_kind_is_int(type_kind(ASTP(param)->type))) {
                realize_generic(ASTP(param)->type, arg->node);
            } else {
                force_generic_realization(arg->node);
            }
            arg->type = arg->node->type;
        }


        memset(&constant, 0, sizeof(constant));
        constant.ast.kind  = AST_POLYMORPHIC_CONSTANT;
        constant.ast.flags = AST_FLAG_CONSTANT;
        constant.ast.type  = arg->type;
        constant.ast.value = arg->value;
        constant.ast.loc   = ASTP(param)->loc;
        constant.name      = param->name;

        ASSERT(!type_is_poly(constant.ast.type), "did not solve");
        ASSERT(constant.ast.type != TY_TYPE || !type_is_poly(constant.ast.value.t), "did not solve");

        array_push(*constants, constant);
/*         report_range_info_no_context_no_exit( */
/*             &ASTP(param)->loc, "CASE 1: %s resolved to %s", */
/*             get_string(constant.name), */
/*             get_string(value_to_string_id(constant.value, constant.type))); */
    }

    if (!(ASTP(param)->flags    & AST_FLAG_POLY_VARARGS)
    &&  param->type_expr->flags & AST_FLAG_POLYMORPH) {

        arg = args + i;

        if (TYPE_IS_GENERIC(arg->type)) {
            force_generic_realization(arg->node);
            arg->type = arg->node->type;
        }

        solve_poly_type_expr(constants, param->type_expr, arg);
    }

}

static array_t get_polymorph_constants(u32 cxt_flags, array_t *params, poly_arg_t *args, u32 n_args, src_range_t params_loc, src_range_t args_loc) {
    array_t       constants;
    int           i;
    ast_t       **it;
    ast_param_t  *param;

/*     report_range_info_no_context_no_exit(&ASTP(arg_list)->loc, */
/*                                          "====================== POLYMORPH ======================"); */

    verify_polymorphic_args(cxt_flags, params, args, n_args, params_loc, args_loc);


    constants = array_make(ast_poly_constant_t);

    i = 0;
    array_traverse(*params, it) {
        param = (ast_param_t*)*it;
        extract_polymorph_constants_from_arg(&constants, i, param, args, n_args);
        i += 1;
    }

#if 0
    {
        polymorph_constant_t *it;
        printf(    "CONSTANTS === name ================ type  =============== value ==============\n");
        array_traverse(constants, it) {
            printf("              %-20s  %-20s  %-20s\n",
                   get_string(it->name),
                   get_string(get_type_string_id(it->type)),
                   get_string(value_to_string_id(it->value, it->type)));
        }
        printf(    "==============================================================================\n");
    }
#endif

    return constants;
}

static monomorphed_t *get_already_monomorphed(array_t *monomorphs, array_t *constants, u32 *idx_out) {
    u32                  idx;
    monomorphed_t       *it;
    u32                  i;
    ast_poly_constant_t *c1;
    ast_poly_constant_t *c2;

    idx = 0;
    array_traverse(*monomorphs, it) {
        if (array_len(it->constants) != array_len(*constants)) { goto next; }

        for (i = 0; i < array_len(it->constants); i += 1) {
            c1 = array_item(it->constants, i);
            c2 = array_item(*constants, i);

            /* @performance
             * Can we assume that names of constants don't need to be compared?
             * If so, it might be good to remove that check and use a bitwise OR
             * of the other two (cheap) compares to help the branch predictor out
             * a bit.
             */
            if (ASTP(c2)->type    != ASTP(c1)->type
            ||  ASTP(c2)->value.u != ASTP(c1)->value.u
            ||  c2->name          != c1->name) {

                goto next;
            }
        }

        if (idx_out != NULL) {
            *idx_out = idx;
        }

        return it;

next:;
        idx += 1;
    }

    return NULL;
}

static monomorphed_t *get_monomorph(check_context_t cxt, ast_t *node, array_t *monomorphs, array_t *params, poly_arg_t *args, u32 n_args, src_range_t args_loc, u32 *idx_out) {
    int                    kind;
    src_range_t            params_loc;
    array_t                constants;
    monomorphed_t         *monomorphed_p;
    monomorphed_t          monomorphed;
    scope_t               *scope;
    ast_t                 *poly_constants_scope_node;
    scope_t               *constants_scope;
    ast_poly_constant_t   *constant_it;
    scope_t              **scope_it;
    ast_t                **node_it;
    int                    i;
    poly_backlog_entry_t   backlog_entry;

    kind       = node->kind;
    params_loc = kind == AST_PROC ? ((ast_proc_t*)node)->params_loc : ((ast_struct_t*)node)->params_loc;
    constants  = get_polymorph_constants(cxt.flags, params, args, n_args, params_loc, args_loc);

    monomorphed_p = get_already_monomorphed(monomorphs, &constants, idx_out);

    if (monomorphed_p == NULL) {
        scope = get_scope(node);

        monomorphed.constants      = constants;
        monomorphed.node           = copy_tree(node, scope->parent);
        monomorphed.specialization = NULL;
        monomorphed_p              = array_push(*monomorphs, monomorphed);

        monomorphed.node->flags &= ~AST_FLAG_POLYMORPH;
        monomorphed.node->flags &= ~AST_FLAG_CHECKED;
        monomorphed.node->flags |= AST_FLAG_MONOMORPH;

        /* Create a new scope tree with a new block above the node's scope which
         * contains the polymorphic constants for this monomorphism. */
        scope = get_scope(monomorphed.node);

        poly_constants_scope_node          = AST_ALLOC(ast_t);
        poly_constants_scope_node->kind    = AST_POLYMORPHIC_CONSTANTS_SCOPE;
        poly_constants_scope_node->flags   = 0;
        poly_constants_scope_node->type    = TY_NOT_TYPED;
        poly_constants_scope_node->value.a = NULL;
        poly_constants_scope_node->loc     = params_loc;

        constants_scope = create_scope(scope->parent, poly_constants_scope_node->kind, poly_constants_scope_node);
        insert_subscope(scope->parent, constants_scope);

        array_traverse(constants, constant_it) {
            add_symbol(constants_scope, constant_it->name, ASTP(constant_it));
        }

        /* Remove the node's scope from it's parent and insert it into the constants scope. */
        i = array_len(scope->parent->subscopes) - 1;
        array_rtraverse(scope->parent->subscopes, scope_it) {
            if (*scope_it == scope) {
                array_delete(scope->parent->subscopes, i);
                break;
            }
            i -= 1;
        }
        insert_subscope(constants_scope, scope);


        /* Remove param declarations that refer to polymorphic constants. Those are in
           a dedicated scope that we just created. */
again:;
        i = 0;
        array_traverse(scope->nodes, node_it) {
            if ((*node_it)->flags & AST_FLAG_POLY_IDENT
            ||  ((*node_it)->kind == AST_PARAM && (*node_it)->flags & AST_FLAG_POLYMORPH)) {

                array_delete(scope->nodes,   i);
                array_delete(scope->symbols, i);
                goto again;
            }
            i += 1;
        }


        /* Set up the context for typechecking. */
        if (kind == AST_PROC) {
            cxt.proc = (ast_proc_t*)monomorphed.node;
        }
        cxt.scope          = constants_scope;
        cxt.monomorph_idx  = array_len(*monomorphs) - 1;
        cxt.flags         |=   CHECK_FLAG_MONOMORPH
                             | (kind == AST_PROC ? CHECK_FLAG_POLY_PROC_TYPE_ONLY : 0)
                             | CHECK_FLAG_FORCE_RECHECK;

        push_breadcrumb_for_poly_constants(&args_loc, cxt.parent_decl->full_name, &constants, 0);
        check_node(cxt, monomorphed.node);
        if (node->kind == AST_STRUCT) {
            verify_polymorphic_args(cxt.flags, &((ast_struct_t*)monomorphed.node)->params, args, n_args, params_loc, args_loc);
        }
        pop_breadcrumb();

        if (idx_out != NULL) {
            *idx_out = cxt.monomorph_idx;
        }

        if (kind == AST_PROC) {
            ((ast_proc_t*)monomorphed.node)->mono_idx = cxt.monomorph_idx;
            monomorphed.type = monomorphed_p->type = monomorphed.node->type;
        } else {
            ((ast_struct_t*)monomorphed.node)->mono_idx = cxt.monomorph_idx;
            monomorphed.type = monomorphed_p->type = monomorphed.node->value.t;
        }

        /* About the backlog:
         * Ideally, we'd just typecheck each polymorph right here and call it a day.
         * This works fine for structs, but there's a problem with procedures: specializations.
         * If a polymorphic procecure is specialized (we may not know at this point), we don't
         * want to typecheck the whole body. The main use case of this is for procedures that
         * are _meant to be specialized_, and it's an error if one isn't provided.
         * To accomplish this, we need to wait until all non-polymorphic declarations are
         * typechecked so that we know whether or not it has been specialized.
         *
         * So, we just get the type of the procedure (by using CHECK_FLAG_POLY_PROC_TYPE_ONLY)
         * and keep a running list of polymorphs of a procedure that need to be typechecked later.
         * If it happens that it was specialized, we just skip over the main typechecking for the
         * procedure.
         */

        if (kind == AST_PROC) {
            /* Note that we keep the CHECK_FLAG_FORCE_RECHECK bit set for the backlog entry. */
            cxt.flags &= ~(CHECK_FLAG_POLY_PROC_TYPE_ONLY |
                           CHECK_FLAG_IN_DEFER            |
                           CHECK_FLAG_IN_LOOP             |
                           CHECK_FLAG_IN_PARAM            |
                           CHECK_FLAG_IN_VARGS);

            cxt.flags |= CHECK_FLAG_POLY_BACKLOG;

            backlog_entry.node       = monomorphed.node;
            backlog_entry.monomorphs = monomorphs;
            backlog_entry.cxt        = cxt;
            backlog_entry.range      = args_loc;
            array_push(poly_backlog, backlog_entry);
        }
    } else {
        if (node->kind == AST_STRUCT) {
            push_breadcrumb_for_poly_constants(&args_loc, cxt.parent_decl->full_name, &constants, 0);
            if (node->kind == AST_STRUCT) {
                verify_polymorphic_args(cxt.flags, &((ast_struct_t*)monomorphed_p->node)->params, args, n_args, params_loc, args_loc);
            }
            pop_breadcrumb();
        }
        array_free(constants);
    }

    return monomorphed_p;
}

static u32 get_proc_monomorph_type(check_context_t cxt, ast_proc_t *proc, poly_arg_t *args, u32 n_args, src_range_t args_loc, u32 *idx_out) {
    return get_monomorph(cxt, ASTP(proc), &proc->monomorphs, &proc->params, args, n_args, args_loc, idx_out)->type;
}

static u32 get_proc_monomorph_type_and_push_breadcrumb_for_call(check_context_t cxt, ast_proc_t *proc, poly_arg_t *args, u32 n_args, src_range_t args_loc, u32 *idx_out) {
    monomorphed_t *mono;
    mono = get_monomorph(cxt, ASTP(proc), &proc->monomorphs, &proc->params, args, n_args, args_loc, idx_out);
    push_breadcrumb_for_poly_constants(&args_loc, proc->parent_decl->full_name, &mono->constants, 1);
    return mono->type;
}

static u32 get_struct_monomorph_type(check_context_t cxt, ast_struct_t *st, poly_arg_t *args, u32 n_args, src_range_t args_loc, u32 *idx_out) {
    return get_monomorph(cxt, ASTP(st), &st->monomorphs, &st->params, args, n_args, args_loc, idx_out)->type;
}

static void check_call(check_context_t cxt, ast_bin_expr_t *expr) {
    u32              proc_ty;
    ast_arg_list_t  *arg_list;
    u32              n_args;
    ast_ident_t     *left_ident;
    ast_decl_t      *struct_decl;
    poly_arg_t      *poly_args;
    u32              i;
    poly_arg_t      *parg;
    u32              idx;
    u32              n_params;
    arg_t           *arg_p;
    u32              param_type;
    u32              arg_type;
    u32              varg_ty;
    u32              last_ty;
    ast_t           *proc_origin;
    array_t          path;
    ast_proc_t      *proc;
    ast_t           *parm_decl;
    u32              poly_proc_ty;

    proc_ty  = expr->left->type;
    arg_list = (ast_arg_list_t*)expr->right;

    check_node(cxt, expr->right);

    n_args = array_len(arg_list->args);

    if (proc_ty == TY_BUILTIN_SPECIAL) {
        check_builtin_special_call(cxt, expr);
        return;
    }

    left_ident = NULL;
    if (expr->left->kind == AST_IDENT) {
        left_ident = (ast_ident_t*)expr->left;
    }

    if (proc_ty == TY_TYPE
    &&  type_kind(expr->left->value.t) == TY_STRUCT) {

        struct_decl = struct_type_to_decl(expr->left->value.t);

        if (!(ASTP(struct_decl)->flags & AST_FLAG_POLYMORPH)) {
            EMBC(expr->left, {
                report_range_err_no_exit(&ASTP(expr)->loc,
                                         "attempting to pass arguments to something that does not have parameters");
                report_range_info_no_context_no_exit(&expr->left->loc,
                                                     "expression has type %s, and value %s",
                                                     get_string(get_type_string_id(proc_ty)),
                                                     get_string(get_type_string_id(expr->left->value.t)));
                report_range_info_no_context(&ASTP(struct_decl)->loc,
                                             "%s is not polymorphic, so it does not take arguments",
                                             get_string(struct_decl->full_name));
            });
        }

        cxt.parent_decl = struct_decl;

        poly_args = alloca(sizeof(*poly_args) * n_args);
        for (i = 0; i < n_args; i += 1) {
            parg        = poly_args + i;
            parg->value = ((arg_t*)array_item(arg_list->args, i))->expr->value;
            parg->type  = ((arg_t*)array_item(arg_list->args, i))->expr->type;
            parg->node  = ((arg_t*)array_item(arg_list->args, i))->expr;

            if (parg->type == TY_TYPE && type_is_poly(parg->value.t)) {
                ASTP(expr)->type    = TY_TYPE;
                ASTP(expr)->value.t = TY_POLY;
                goto skip;
            }
        }

        ASTP(expr)->type    = TY_TYPE;
        ASTP(expr)->value.t = get_struct_monomorph_type(cxt, (ast_struct_t*)struct_decl->val_expr, poly_args, n_args, ASTP(arg_list)->loc, &idx);

skip:;
        if (left_ident != NULL) {
            left_ident->mono_idx = idx;
        }
        ASTP(expr)->flags |= AST_FLAG_CONSTANT;

        return;
    } else if (expr->left->flags & AST_FLAG_POLYMORPH) {
        ASTP(expr)->type    = TY_TYPE;
        ASTP(expr)->value.t = TY_POLY;

        ASTP(expr)->flags |= AST_FLAG_CONSTANT;

        return;
    }

    if (type_kind(proc_ty) != TY_PROC) {
        EMBC(expr->left, {
            report_range_err_no_exit(&ASTP(expr)->loc,
                                     "attempting to pass arguments to something that does not have parameters");
            report_range_info_no_context(&expr->left->loc,
                                         "expression has type %s",
                                         get_string(get_type_string_id(proc_ty)));
        });
        return;
    }

    proc_origin = NULL;
    proc        = NULL;
    if (left_ident != NULL) {
        proc_origin = try_get_decl_and_path(left_ident, &path);
        if (proc_origin != NULL) {
            expr->call_decl = proc_origin;
            if (proc_origin->kind == AST_DECL_PROC) {
                proc = (ast_proc_t*)((ast_decl_t*)proc_origin)->val_expr;
            }
        }
    }

    n_params = get_num_param_types(proc_ty);

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
                report_simple_info_no_exit("expected %s%d, but got %d",
                                           varg_ty != TY_NONE ? "at least " : "", n_params, n_args);
                report_simple_info("indirect call with procedure type %s",
                                   get_string(get_type_string_id(proc_ty)));
            } else {
                if (left_ident->resolved_node == proc_origin) {
                    report_simple_info_no_exit("expected %s%d, but got %d",
                                               varg_ty != TY_NONE ? "at least " : "", n_params, n_args);
                    if (proc_origin->kind == AST_BUILTIN) {
                        report_simple_info("'%s' is a compiler builtin",
                                           get_string(((ast_builtin_t*)proc_origin)->name));
                    } else {
                        report_range_info_no_context(&proc_origin->loc,
                                                     "'%s' defined here:",
                                                     get_string(((ast_decl_t*)proc_origin)->name));
                    }
                } else {
                    report_simple_info_no_exit("expected %s%d, but got %d",
                                               varg_ty != TY_NONE ? "at least " : "", n_params, n_args);
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

    if (proc != NULL
    &&  ASTP(proc)->flags & AST_FLAG_POLYMORPH) {
        cxt.parent_decl  = (ast_decl_t*)proc_origin;

        poly_args = alloca(sizeof(*poly_args) * n_args);
        for (i = 0; i < n_args; i += 1) {
            parg        = poly_args + i;
            parg->value = ((arg_t*)array_item(arg_list->args, i))->expr->value;
            parg->type  = ((arg_t*)array_item(arg_list->args, i))->expr->type;
            parg->node  = ((arg_t*)array_item(arg_list->args, i))->expr;
        }

        poly_proc_ty     = get_proc_monomorph_type_and_push_breadcrumb_for_call(cxt, proc, poly_args, n_args, ASTP(arg_list)->loc, &idx);
        expr->left->type = poly_proc_ty;
        proc_ty          = poly_proc_ty;

        if (left_ident != NULL) {
            left_ident->mono_idx = idx;
        }

        n_params = get_num_param_types(proc_ty);

        varg_ty = TY_NONE;
        if (n_params >= 1) {
            last_ty = get_param_type(proc_ty, n_params - 1);
            if (type_kind(last_ty) == TY_VARGS) {
                varg_ty   = get_under_type(last_ty);
                n_params -= 1;
            }
        }
    }

    for (i = 0; i < n_params; i += 1) {
        param_type = get_param_type(proc_ty, i);
        arg_p      = array_item(arg_list->args, i);
        arg_type   = arg_p->expr->type;

        if (!types_are_compatible(param_type, arg_type)) {
            if (left_ident == NULL) {
                EMBC(arg_p->expr, {
                    report_range_err_no_exit(&arg_p->expr->loc,
                                             "incorrect argument type: expected %s, but got %s",
                                             get_string(get_type_string_id(param_type)),
                                             get_string(get_type_string_id(arg_type)));
                    report_simple_info("indirect call with procedure type %s",
                                       get_string(get_type_string_id(proc_ty)));
                });
            } else {
                EMBC(arg_p->expr, {
                    report_range_err_no_exit(&arg_p->expr->loc,
                                             "incorrect argument type: expected %s, but got %s",
                                             get_string(get_type_string_id(param_type)),
                                             get_string(get_type_string_id(arg_type)));

                    if (proc_origin->kind == AST_BUILTIN) {
                        if (left_ident->resolved_node == proc_origin) {
                            report_simple_info("'%s' is a compiler builtin",
                                               get_string(((ast_builtin_t*)proc_origin)->name));
                        } else {
                            report_declaration_path(path);
                        }
                    } else {
                        parm_decl = *(ast_t**)array_item(proc->params, i);

                        if (left_ident->resolved_node == proc_origin) {
                            report_range_info_no_context(&parm_decl->loc, "parameter declared here:");
                        } else {
                            report_range_info_no_context_no_exit(&parm_decl->loc, "parameter declared here:");
                            report_declaration_path(path);
                        }
                    }
                });
            }

            return;
        }

        if (TYPE_IS_GENERIC(arg_type)) {
            realize_generic(param_type, arg_p->expr);
        }

        if (proc != NULL && i < array_len(proc->params) - !!(ASTP(proc)->flags & AST_FLAG_POLY_VARARGS)) {
            parm_decl = *(ast_t**)array_item(proc->params, i);
            arg_p->expr->flags |= parm_decl->flags & AST_FLAG_POLYMORPH;
        }

        if (arg_p->name != STRING_ID_NULL) {
            if (proc == NULL) {
                report_range_err(&arg_p->expr->loc, "named arguments are not allowed in indirect calls");
                return;
            } else if (parm_decl == NULL) {
                report_range_err(&arg_p->expr->loc, "using argument name '%s' for a parameter that does not carry a name",
                                 get_string(arg_p->name));
                return;
            } else {
                if (proc_origin == NULL) {
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
                }

                if (arg_p->name != ((ast_param_t*)parm_decl)->name) {
                    report_range_err_no_exit(&arg_p->expr->loc,
                                             "argument name '%s' does not match parameter name '%s'",
                                             get_string(arg_p->name),
                                             get_string(((ast_param_t*)parm_decl)->name));
                    if (left_ident->resolved_node == proc_origin) {
                        report_range_info_no_context(&parm_decl->loc, "parameter declared here:");
                    } else {
                        report_range_info_no_context_no_exit(&parm_decl->loc, "parameter declared here:");
                        report_declaration_path(path);
                    }
                }
            }
        }
    }

    if (proc != NULL) {
        array_free(path);
        if (ASTP(proc)->flags & AST_FLAG_POLYMORPH) {
            pop_breadcrumb();
        }
    }

    ASTP(expr)->type = get_ret_type(expr->left->type);
}

static void check_expr_unsatisfied_poly(check_context_t cxt, ast_t *expr) {
    ast_t       *resolved;
    const char  *resolved_name;
    src_range_t *loc;

    if (expr->kind != AST_IDENT)           { return; }
    if (expr->flags & AST_FLAG_POLY_IDENT) { return; }

    resolved = ((ast_ident_t*)expr)->resolved_node;

    if (resolved->kind == AST_BUILTIN)         { return; }
    if (resolved->flags & AST_FLAG_POLY_IDENT) { return; }

/*
 * I'd rather only do all these branches and stuff if we know we're going to produce an
 * error. This function is probably pretty hot.
 */
#define GET_ERROR_MESSAGE_INFO()                                                         \
    loc = &resolved->loc;                                                                \
    if (ast_kind_is_decl(resolved->kind)) {                                              \
        resolved_name = get_string(((ast_decl_t*)resolved)->full_name);                  \
        if (resolved->kind == AST_DECL_STRUCT) {                                         \
            loc = &((ast_struct_t*)((ast_decl_t*)resolved)->val_expr)->params_loc;       \
        } else if (resolved->kind == AST_DECL_PROC) {                                    \
            loc = &((ast_proc_t*)((ast_decl_t*)resolved)->val_expr)->params_loc;         \
        }                                                                                \
    } else if (resolved->kind == AST_PARAM) {                                            \
        resolved_name = get_string(((ast_param_t*)resolved)->name);                      \
    } else if (resolved->kind == AST_POLYMORPHIC_CONSTANT) {                             \
        resolved_name = get_string(((ast_poly_constant_t*)resolved)->name);              \
    } else {                                                                             \
        ASSERT(0, "resolved_node of ident not an ident, decl, param, or poly constant"); \
        return;                                                                          \
    }

    if (expr->type == TY_TYPE && type_is_poly(expr->value.t)) {
        EMBC(expr, {
            GET_ERROR_MESSAGE_INFO();
            report_range_err_no_exit(&expr->loc,
                                    "struct '%s' is polymorphic, so it requires arguments to satisfy its polymorphic parameters",
                                    resolved_name);
            report_fixit_no_exit(expr->loc.end, "provide arguments for polymorphic parameters\a(...)");
            report_range_info_no_context(loc, "'%s' defined with these polymorphic parameters:", resolved_name);
        });

    } else if (!(cxt.flags & CHECK_FLAG_ALLOW_REF_POLY_PROC)
           &&  type_kind(expr->type) == TY_PROC
           &&  type_is_poly(expr->type)) {

        EMBC(expr, {
        GET_ERROR_MESSAGE_INFO();
            report_range_err_no_exit(&expr->loc,
                                    "procedure '%s' is polymorphic, so it requires arguments to satisfy its polymorphic parameters",
                                    resolved_name);
            /* @todo
            * We should probably provide some way to reference a monomorphism of a procedure without actually calling it...
            */
    /*         report_fixit_no_exit(expr->loc.end, "you may provide arguments for polymorphic parameters by calling the procedure" */
    /*                                             ", but keep in mind that this is probably different than what you had in mind\a(...)"); */
            report_range_info_no_context(loc, "'%s' defined with these polymorphic parameters:", resolved_name);
        });
    }
}

static void check_ident(check_context_t cxt, ast_ident_t *ident) {
    scope_t *resolved_node_scope;

#if 0
    if (ident->str_rep == UNDERSCORE_ID) {
        report_range_err_no_exit(&ASTP(ident)->loc, "'_' can be assigned to, but not referenced");
        report_simple_info("'_' acts as an assignment sink for values meant to be unreferenceable");
        return;
    }
#endif

    if (ident->resolved_node == NULL) {
        ident->resolved_node = search_up_scopes_return_scope(cxt.scope, ident->str_rep, &resolved_node_scope);

        if (ident->resolved_node == NULL) {
            report_range_err(&ASTP(ident)->loc,
                            "use of undeclared identifier '%s'", get_string(ident->str_rep));
            return;
        }

        if (ident->resolved_node->flags & AST_FLAG_CONSTANT) {
            ASTP(ident)->flags |= AST_FLAG_CONSTANT;
        }

        if (ident->resolved_node->kind != AST_POLYMORPHIC_CONSTANT
        &&  ASTP(ident)->flags & AST_FLAG_POLYMORPH) {

            ASTP(ident)->type     = TY_POLY;
            ASTP(ident)->value.t  = TY_POLY;
            ASTP(ident)->flags   |= AST_FLAG_CONSTANT;
        } else {
            if (ident->resolved_node->kind == AST_POLYMORPHIC_CONSTANT) {
                switch (type_kind(ident->resolved_node->type)) {
                    case TY_PROC:
                        ident->resolved_node = ASTP(((ast_proc_t*)ident->resolved_node->value.a)->parent_decl);
                        break;
                    case TY_MODULE:
                        ident->resolved_node = ASTP(((ast_module_t*)ident->resolved_node->value.a)->parent_decl);
                        break;
                }
            }

            if (ident->resolved_node->type == TY_UNKNOWN) {
                if (ident->resolved_node == ASTP(cxt.parent_decl)) {
                    CYCLE_PATH_PUSH(ASTP(ident));
                    check_for_decl_cycle(ASTP(cxt.parent_decl));
                    CYCLE_PATH_POP();
                    return;
                }

                cxt.scope  = resolved_node_scope;
                cxt.flags &= ~CHECK_FLAG_MONOMORPH;
                CYCLE_PATH_PUSH(ASTP(ident));
                check_node(cxt, ident->resolved_node);
                CYCLE_PATH_POP();
            }

            ASTP(ident)->type  = ident->resolved_node->type;
            ASTP(ident)->value = ident->resolved_node->value;
        }
    }

    if (ASTP(ident)->flags & AST_FLAG_EXPR_TOP) {
        check_expr_unsatisfied_poly(cxt, ASTP(ident));
    }

    ASSERT(ident->resolved_node != NULL, "did not resolve ident?");

    if ((ident->resolved_node->kind == AST_DECL_VAR || ident->resolved_node->kind == AST_PARAM)
    &&  !(ident->resolved_node->flags & (AST_FLAG_POLYMORPH | AST_FLAG_CONSTANT))) {

        ASTP(ident)->flags |= AST_FLAG_EXPR_CAN_BE_LVAL;
    }
}

static void check_proc_type(check_context_t cxt, ast_proc_type_t *proc_type) {
    u32    *param_types;
    u32     n;
    ast_t **it;
    ast_t  *expr;
    u32     ret_type;
    u32     ty;

    param_types = alloca(sizeof(*param_types) * array_len(proc_type->param_type_exprs));
    n           = 0;
    array_traverse(proc_type->param_type_exprs, it) {
        expr = *it;
        check_node(cxt, expr);
        if (expr->type != TY_TYPE) {
            EMBC(expr, {
                report_range_err_no_exit(&expr->loc,
                                         "parameter type expression must have type type");
                report_range_info_no_context(&expr->loc,
                                             "expression has type %s",
                                             get_string(get_type_string_id(expr->type)));
            });
            return;
        }
        param_types[n] = expr->value.t;
        n += 1;
    }

    if (proc_type->ret_type_expr == NULL) {
        ret_type = TY_NOT_TYPED;
    } else {
        expr = proc_type->ret_type_expr;
        check_node(cxt, expr);
        if (expr->type != TY_TYPE) {
            EMBC(expr, {
                report_range_err_no_exit(&expr->loc,
                                         "return type expression must have type type");
                report_range_info_no_context(&expr->loc,
                                             "expression has type %s",
                                             get_string(get_type_string_id(expr->type)));
            });
            return;
        }
        ret_type = expr->value.t;
    }

    ty = get_proc_type(n, param_types, ret_type);

    ASTP(proc_type)->type     = TY_TYPE;
    ASTP(proc_type)->flags   |= AST_FLAG_CONSTANT;
    ASTP(proc_type)->value.t  = ty;
}

static void check_namespace_dot(check_context_t cxt, ast_bin_expr_t *expr) {
    /* @note:
    ** expr->left must have been checked by this point.
    */

    ast_module_t *mod;
    ast_struct_t *st;
    const char   *resolved_name;
    const char   *s_kind;
    scope_t      *search_scope;
    ast_ident_t  *right_ident;
    ast_t        *found_node;
    ast_ident_t   new_ident;
    const char   *lname;
    u32           llen;
    const char   *rname;
    u32           rlen;
    u32           new_name_len;
    char         *new_name_buff;


    check_expr_unsatisfied_poly(cxt, expr->left);

    if (expr->left->type == TY_MODULE) {
        ASSERT(expr->left->value.a->kind == AST_MODULE,
               "module type does not have module value");

        mod = (ast_module_t*)expr->left->value.a;
        ASSERT(mod != NULL, "missing module value");

        search_scope  = mod->scope;
        resolved_name = get_string(mod->parent_decl->full_name);
        s_kind        = "module";
    } else if (expr->left->type == TY_TYPE) {
        st = struct_type_to_definition(expr->left->value.t);
        ASSERT(st != NULL, "missing struct value");

        search_scope  = st->scope;
        resolved_name = get_string(get_type_string_id(expr->left->value.t));
        s_kind        = "struct";
    } else {
        ASSERT(0, "not a type I can handle in check_namespace_dot()");
        return;
    }

    right_ident = (ast_ident_t*)expr->right;
    found_node  = find_in_scope(search_scope, right_ident->str_rep);

    if (found_node == NULL) {
        EMBC(expr->right, {
            report_range_err(&expr->right->loc,
                             "nothing named '%s' in %s '%s'",
                             get_string(right_ident->str_rep),
                             s_kind,
                             resolved_name);
        });
        return;
    }

    if (found_node->kind == AST_DECL_STRUCT_FIELD) {
        ASSERT(expr->left->type == TY_TYPE, "found a field in a non-struct???");
        EMBC(expr->right, {
            report_range_err(&ASTP(expr)->loc,
                             "struct '%s' has a field called '%s', "
                             "but it must be accessed from an instance of type '%s', rather than the type itself",
                             resolved_name,
                             get_string(right_ident->str_rep),
                             resolved_name);
        });
        return;
    }

    cxt.scope  = search_scope;
    cxt.flags &= ~CHECK_FLAG_MONOMORPH;

    memset(&new_ident, 0, sizeof(new_ident));
    ASTP(&new_ident)->kind  = AST_IDENT;
    ASTP(&new_ident)->flags = ASTP(expr)->flags | (found_node->flags & AST_FLAG_CONSTANT);
    ASTP(&new_ident)->type  = found_node->type;
    ASTP(&new_ident)->value = found_node->value;
    ASTP(&new_ident)->loc   = ASTP(expr)->loc;

    lname         = resolved_name;
    llen          = strlen(lname);
    rname         = get_string(right_ident->str_rep);
    rlen          = strlen(rname);
    new_name_len  = llen + 1 + rlen;
    new_name_buff = alloca(new_name_len);
    memcpy(new_name_buff, lname, llen);
    new_name_buff[llen] = '.';
    memcpy(new_name_buff + llen + 1, rname, rlen);

    new_ident.str_rep       = get_string_id_n(new_name_buff, new_name_len);
    new_ident.resolved_node = found_node;
    new_ident.mono_idx      = -1;
    new_ident.varg_idx      = -1;

    REPLACE_NODE(ASTP(expr), ASTP(&new_ident));

    CYCLE_PATH_PUSH(ASTP(expr));
    check_node(cxt, found_node);
    CYCLE_PATH_POP();
}

static void check_dot(check_context_t cxt, ast_bin_expr_t *expr) {
    u32           tk;
    u32           vtk;
    u32           st_ty;
    u32           field_ty;
    ast_decl_t   *st_decl;
    ast_struct_t *st;

    if (expr->right->kind != AST_IDENT) {
        EMBC(expr->right, {
            report_range_err(&expr->right->loc,
                             "the '.' operator must be followed by an identifier");
        });
        return;
    }

    tk = type_kind(expr->left->type);

    switch (tk) {
        case TY_POLY:
            ASTP(expr)->type = TY_POLY;
            break;
        case TY_MODULE:
            check_namespace_dot(cxt, expr);
            break;
        case TY_TYPE:
            vtk = type_kind(expr->left->value.t);
            if (vtk != TY_STRUCT && vtk != TY_STRUCT_MONO) { goto does_not_apply; }
            check_namespace_dot(cxt, expr);
            break;
        case TY_STRUCT:
        case TY_STRUCT_MONO:
            st_ty = expr->left->type;
            field_ty = get_struct_field_type(st_ty, ((ast_ident_t*)expr->right)->str_rep);
            if (field_ty == TY_UNKNOWN) {
                EMBC(expr->right, {
                    report_range_err(&expr->right->loc,
                                     "type %s does not have a field named '%s'",
                                     get_string(get_type_string_id(st_ty)),
                                     get_string(((ast_ident_t*)expr->right)->str_rep));
                });
            }
            ASTP(expr)->type = field_ty;

            st_decl = struct_type_to_decl(st_ty);
            ASSERT(st_decl != NULL, "did not get struct decl");

            st = (ast_struct_t*)st_decl->val_expr;

            if (st->bitfield_struct_bits) { ASTP(expr)->flags |= AST_FLAG_BITFIELD_DOT; }

            ASTP(expr)->flags |= (expr->left->flags & AST_FLAG_EXPR_CAN_BE_LVAL);

            break;
        case TY_PTR:
            st_ty = get_under_type(expr->left->type);

            if (type_kind(st_ty) != TY_STRUCT
            &&  type_kind(st_ty) != TY_STRUCT_MONO) {

                goto does_not_apply;
            }

            field_ty = get_struct_field_type(st_ty, ((ast_ident_t*)expr->right)->str_rep);
            if (field_ty == TY_UNKNOWN) {
                EMBC(expr->right, {
                    report_range_err(&expr->right->loc,
                                     "type %s does not have a field named '%s'",
                                     get_string(get_type_string_id(st_ty)),
                                     get_string(((ast_ident_t*)expr->right)->str_rep));
                });
                return;
            }
            ASTP(expr)->type = field_ty;

            ASTP(expr)->flags |= AST_FLAG_EXPR_CAN_BE_LVAL;

            break;
        does_not_apply:;
        default:
            EMBC(expr->left, {
                report_range_err(&ASTP(expr)->loc,
                                 "the '.' operator does not apply to left-hand-side operand %s%s",
                                 tk == TY_TYPE ? "" : "of type ",
                                 get_string(get_type_string_id(tk == TY_TYPE ? expr->left->value.t : expr->left->type)));
            });
            return;
    }
}

static void binop_bad_type_error(ast_bin_expr_t *expr) {
    u32 lt;
    u32 rt;

    lt = expr->left->type;
    rt = expr->right->type;

    EMBC(expr->left, {
        EMBC(expr->right, {
            report_range_err(&ASTP(expr)->loc,
                            "operator '%s' does not apply to types %s and %s",
                            OP_STR(expr->op),
                            get_string(get_type_string_id(lt)),
                            get_string(get_type_string_id(rt)));
        });
    });
}

#define BIN_EXPR_CONST(l, r) \
    (((l)->flags & AST_FLAG_CONSTANT) & ((r)->flags & AST_FLAG_CONSTANT))

static void check_add(check_context_t cxt, ast_bin_expr_t *expr) {
    u32 t1;
    u32 t2;
    u32 tk1;
    u32 tk2;
    u64 tk_both;

    t1  = expr->left->type;
    t2  = expr->right->type;
    tk1 = type_kind(t1);
    tk2 = type_kind(t2);

    ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);

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

            if (ASTP(expr)->flags & AST_FLAG_CONSTANT) {
                if (INT_TYPE_IS_SIGNED(ASTP(expr)->type)) {
                    ASTP(expr)->value.i = expr->left->value.i + expr->right->value.i;
                } else {
                    ASTP(expr)->value.u = expr->left->value.u + expr->right->value.u;
                }
            }

            break;
        case TKINDPAIR_FLT_FLT:
            /* @todo check for width incompat */
            /* @todo How to handle this case? Do we promote the type? */
            ASTP(expr)->type = t1;

            if (ASTP(expr)->flags & AST_FLAG_CONSTANT) {
                ASTP(expr)->value.f = expr->left->value.f + expr->right->value.f;
            }

            break;
        case TKINDPAIR_PTR_INT:
            ASTP(expr)->type   = (tk1 == TY_PTR ? t1 : t2);
            ASTP(expr)->flags &= ~AST_FLAG_CONSTANT;
            break;
        default:
            binop_bad_type_error(expr);
            return;
    }
}

static void check_sub(check_context_t cxt, ast_bin_expr_t *expr) {
    u32 t1;
    u32 t2;
    u32 tk1;
    u32 tk2;
    u64 tk_both;

    t1  = expr->left->type;
    t2  = expr->right->type;
    tk1 = type_kind(t1);
    tk2 = type_kind(t2);

    ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);

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

            if (ASTP(expr)->flags & AST_FLAG_CONSTANT) {
                if (INT_TYPE_IS_SIGNED(ASTP(expr)->type)) {
                    ASTP(expr)->value.i = expr->left->value.i - expr->right->value.i;
                } else {
                    ASTP(expr)->value.u = expr->left->value.u - expr->right->value.u;
                }
            }

            break;
        case TKINDPAIR_FLT_FLT:
            /* @todo check for width incompat */
            /* @todo How to handle this case? Do we promote the type? */
            ASTP(expr)->type = t1;

            if (ASTP(expr)->flags & AST_FLAG_CONSTANT) {
                ASTP(expr)->value.f = expr->left->value.f - expr->right->value.f;
            }

            break;
        case TKINDPAIR_PTR_INT:
            ASTP(expr)->type   = (tk1 == TY_PTR ? t1 : t2);
            ASTP(expr)->flags &= ~AST_FLAG_CONSTANT;
            break;
        default:
            binop_bad_type_error(expr);
            break;
    }
}

static void check_mul(check_context_t cxt, ast_bin_expr_t *expr) {
    u32 t1;
    u32 t2;
    u32 tk1;
    u32 tk2;
    u64 tk_both;

    t1  = expr->left->type;
    t2  = expr->right->type;
    tk1 = type_kind(t1);
    tk2 = type_kind(t2);

    ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);

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

            if (ASTP(expr)->flags & AST_FLAG_CONSTANT) {
                if (INT_TYPE_IS_SIGNED(ASTP(expr)->type)) {
                    ASTP(expr)->value.i = expr->left->value.i * expr->right->value.i;
                } else {
                    ASTP(expr)->value.u = expr->left->value.u * expr->right->value.u;
                }
            }
            break;
        case TKINDPAIR_FLT_FLT:
            /* @todo check for width incompat */
            /* @todo How to handle this case? Do we promote the type? */
            ASTP(expr)->type = t1;

            if (ASTP(expr)->flags & AST_FLAG_CONSTANT) {
                ASTP(expr)->value.f = expr->left->value.f * expr->right->value.f;
            }

            break;
        default:
            binop_bad_type_error(expr);
            break;
    }
}

static void check_div(check_context_t cxt, ast_bin_expr_t *expr) {
    u32 t1;
    u32 t2;
    u32 tk1;
    u32 tk2;
    u64 tk_both;

    t1  = expr->left->type;
    t2  = expr->right->type;
    tk1 = type_kind(t1);
    tk2 = type_kind(t2);

    ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);

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

            if (ASTP(expr)->flags & AST_FLAG_CONSTANT) {
                if (expr->left->value.u == 0) {
                    EMBC(expr->right, {
                        report_range_err_no_exit(&ASTP(expr)->loc, "division by zero in a constant expression");
                        report_range_info(&expr->right->loc, "expression evaluates to zero");
                    });
                }

                if (INT_TYPE_IS_SIGNED(ASTP(expr)->type)) {
                    ASTP(expr)->value.i = expr->left->value.i / expr->right->value.i;
                } else {
                    ASTP(expr)->value.u = expr->left->value.u / expr->right->value.u;
                }
            }
            break;
        case TKINDPAIR_FLT_FLT:
            /* @todo check for width incompat */
            /* @todo How to handle this case? Do we promote the type? */
            ASTP(expr)->type = t1;

            if (ASTP(expr)->flags & AST_FLAG_CONSTANT) {
                ASTP(expr)->value.f = expr->left->value.f / expr->right->value.f;
            }

            break;
        default:
            binop_bad_type_error(expr);
            break;
    }
}

static void check_mod(check_context_t cxt, ast_bin_expr_t *expr) {
    u32 t1;
    u32 t2;
    u32 tk1;
    u32 tk2;
    u64 tk_both;

    t1  = expr->left->type;
    t2  = expr->right->type;
    tk1 = type_kind(t1);
    tk2 = type_kind(t2);

    ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);

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

            if (ASTP(expr)->flags & AST_FLAG_CONSTANT) {
                if (expr->left->value.u == 0) {
                    EMBC(expr->right, {
                        report_range_err_no_exit(&ASTP(expr)->loc, "modulus by zero in a constant expression");
                        report_range_info(&expr->right->loc, "expression evaluates to zero");
                    });
                }

                if (INT_TYPE_IS_SIGNED(ASTP(expr)->type)) {
                    ASTP(expr)->value.i = expr->left->value.i % expr->right->value.i;
                } else {
                    ASTP(expr)->value.u = expr->left->value.u % expr->right->value.u;
                }
            }
            break;
        default:
            binop_bad_type_error(expr);
            break;
    }
}

static void check_cmp(check_context_t cxt, ast_bin_expr_t *expr) {
    u32 t1;
    u32 t2;
    u32 tk1;
    u32 tk2;
    u64 tk_both;
    int ls;
    int rs;

    t1 = expr->left->type;
    t2 = expr->right->type;

    if (t1 != t2) { goto bad; }

    tk1 = type_kind(t1);
    tk2 = type_kind(t2);

    ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);

    /*
    ** tk_both is the combo of tk1 and tk2 in sorted order so that we can do the
    ** below checks without considering right/left.
    */
    tk_both = tk1 < tk2
                ? (((u64)tk1) << 32ULL) + tk2
                : (((u64)tk2) << 32ULL) + tk1;

    switch (tk_both) {
        case TKINDPAIR_PTR_PTR:
            ASTP(expr)->flags &= ~AST_FLAG_EXPR_CAN_BE_LVAL;
            break;
        case TKINDPAIR_INT_INT:
            ls = INT_TYPE_IS_SIGNED(t1);
            rs = INT_TYPE_IS_SIGNED(t2);

            if (ls != rs) { goto bad; }

            if (ASTP(expr)->flags & AST_FLAG_CONSTANT) {
                if (INT_TYPE_IS_SIGNED(ASTP(expr)->type)) {
                    switch (expr->op) {
                        case OP_EQU: ASTP(expr)->value.i = expr->left->value.i == expr->right->value.i; break;
                        case OP_NEQ: ASTP(expr)->value.i = expr->left->value.i != expr->right->value.i; break;
                        case OP_LSS: ASTP(expr)->value.i = expr->left->value.i <  expr->right->value.i; break;
                        case OP_LEQ: ASTP(expr)->value.i = expr->left->value.i <= expr->right->value.i; break;
                        case OP_GTR: ASTP(expr)->value.i = expr->left->value.i >  expr->right->value.i; break;
                        case OP_GEQ: ASTP(expr)->value.i = expr->left->value.i >= expr->right->value.i; break;
                        default: ASSERT(0, "bad op");
                    }
                } else {
                    switch (expr->op) {
                        case OP_EQU: ASTP(expr)->value.i = expr->left->value.u == expr->right->value.u; break;
                        case OP_NEQ: ASTP(expr)->value.i = expr->left->value.u != expr->right->value.u; break;
                        case OP_LSS: ASTP(expr)->value.i = expr->left->value.u <  expr->right->value.u; break;
                        case OP_LEQ: ASTP(expr)->value.i = expr->left->value.u <= expr->right->value.u; break;
                        case OP_GTR: ASTP(expr)->value.i = expr->left->value.u >  expr->right->value.u; break;
                        case OP_GEQ: ASTP(expr)->value.i = expr->left->value.u >= expr->right->value.u; break;
                        default: ASSERT(0, "bad op");
                    }
                }
            }
            break;
        case TKINDPAIR_FLT_FLT:
            if (ASTP(expr)->flags & AST_FLAG_CONSTANT) {
                switch (expr->op) {
                    case OP_EQU: ASTP(expr)->value.i = expr->left->value.f == expr->right->value.f; break;
                    case OP_NEQ: ASTP(expr)->value.i = expr->left->value.f != expr->right->value.f; break;
                    case OP_LSS: ASTP(expr)->value.i = expr->left->value.f <  expr->right->value.f; break;
                    case OP_LEQ: ASTP(expr)->value.i = expr->left->value.f <= expr->right->value.f; break;
                    case OP_GTR: ASTP(expr)->value.i = expr->left->value.f >  expr->right->value.f; break;
                    case OP_GEQ: ASTP(expr)->value.i = expr->left->value.f >= expr->right->value.f; break;
                    default: ASSERT(0, "bad op");
                }
            }
            break;
        case TKINDPAIR_TYP_TYP:
            ASSERT(ASTP(expr)->flags & AST_FLAG_CONSTANT, "type comparison should always have constant operands");
            switch (expr->op) {
                case OP_EQU:
                    ASTP(expr)->value.i = expr->left->value.t == expr->right->value.t;
                    break;
                case OP_NEQ:
                    ASTP(expr)->value.i = expr->left->value.t != expr->right->value.t;
                    break;
                default: goto bad;
                    break;
            }
            break;
        case TKINDPAIR_MOD_MOD:
            ASSERT(ASTP(expr)->flags & AST_FLAG_CONSTANT,
                   "type comparison should always have constant operands");
            switch (expr->op) {
                case OP_EQU: ASTP(expr)->value.i = expr->left->value.a == expr->right->value.a; break;
                case OP_NEQ: ASTP(expr)->value.i = expr->left->value.a != expr->right->value.a; break;
                default: goto bad;
                    break;
            }
            break;
        default:
bad:;
            binop_bad_type_error(expr);
            return;
    }

    ASTP(expr)->type = TY_S64;
}

static void check_logical(check_context_t cxt, ast_bin_expr_t *expr) {
    u32 t1;
    u32 t2;
    u32 tk1;
    u32 tk2;

    t1  = expr->left->type;
    t2  = expr->right->type;
    tk1 = type_kind(t1);
    tk2 = type_kind(t2);

    if (tk1 != TY_GENERIC_INT && tk2 != TY_GENERIC_INT) {
        binop_bad_type_error(expr);
        return;
    }

    ASTP(expr)->type = TY_S64;
}

static void operand_not_typed_error(ast_t *expr, ast_t *operand) {
    int op;

    op = expr->kind == AST_UNARY_EXPR
            ? ((ast_unary_expr_t*)expr)->op
            : ((ast_bin_expr_t*)expr)->op;

    EMBC(operand, {
        report_range_err_no_exit(&operand->loc,
                                 "invalid operand to %s '%s' expression",
                                 OP_IS_UNARY(op) ? "unary" : "binary",
                                 OP_STR(op));
        report_simple_info("the expression does not have a type or value");
    });
}

static void check_bin_expr(check_context_t cxt, ast_bin_expr_t *expr) {
    u32 tkl;
    u32 tkr;
    u32 array_length;

    /*
    ** OP_DOT is special because the right operand could fail in symbol resolution
    ** if checked alone.
    */

    check_node(cxt, expr->left);

    if (expr->left->type == TY_NOT_TYPED) {
        operand_not_typed_error(ASTP(expr), expr->left);
        return;
    }

    if (expr->op == OP_DOT) {
        ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);
        check_dot(cxt, expr);
        return;
    }

    if (expr->op != OP_CALL) {
        check_node(cxt, expr->right);
    }

    if (expr->left->type == TY_NOT_TYPED) {
        operand_not_typed_error(ASTP(expr), expr->right);
        return;
    }

    if (types_are_compatible(expr->left->type, expr->right->type)) {
        if (!TYPE_IS_GENERIC(expr->left->type)
        &&  TYPE_IS_GENERIC(expr->right->type)) {

            realize_generic(expr->left->type, expr->right);
        } else if (TYPE_IS_GENERIC(expr->left->type)
            &&  !TYPE_IS_GENERIC(expr->right->type)) {

            realize_generic(expr->right->type, expr->left);
        }
    }

    tkl = type_kind(expr->left->type);
    tkr = type_kind(expr->right->type);

    switch (expr->op) {
        case OP_DOT: /* Handled above. */ break;

        case OP_CALL:
            check_call(cxt, expr);
            break;

        case OP_SUBSCRIPT:
            if (tkl == TY_SLICE || tkl == TY_ARRAY) {
                ASTP(expr)->type = get_under_type(expr->left->type);
            } else {
                EMBC(expr->left, {
                    report_range_err_no_exit(&ASTP(expr)->loc,
                                             "left-hand-side operand of '[]' operator must be a slice or array type");
                    report_range_info_no_context(&expr->left->loc,
                                                 "operand has type %s",
                                                 get_string(get_type_string_id(expr->left->type)));
                });
                return;
            }

            if (!type_kind_is_int(tkr)) {
                EMBC(expr->right, {
                    report_range_err_no_exit(&ASTP(expr)->loc,
                                             "right-hand-side operand of '[]' operator must be an integer type");
                    report_range_info_no_context(&expr->right->loc,
                                             "operand has type %s",
                                             get_string(get_type_string_id(expr->right->type)));
                });
            }

            if (TYPE_IS_GENERIC(expr->right->type)) {
                push_range_breadcrumb(&ASTP(expr)->loc, "forcing integer literal to s64 since it is being used as a subscript index");
                realize_generic(TY_S64, expr->right);
                pop_breadcrumb();
            }

            if (tkl == TY_ARRAY && (expr->right->flags & AST_FLAG_CONSTANT)) {
                if (INT_TYPE_IS_SIGNED(expr->right->type)) {
                    if (expr->right->value.i > UINT32_MAX) {
                        EMBC(expr->right, {
                            report_range_err_no_exit(&expr->right->loc, "index operand in array subscript expression is too large");
                            report_simple_info("operand has value %"PRIi64", but the maximum is %u",
                                            expr->right->value.i, UINT32_MAX);
                        });
                    }
                    if (expr->right->value.i < 0) {
                        EMBC(expr->right, {
                            report_range_err_no_exit(&expr->right->loc, "index operand in array subscript expression is less than zero");
                            report_simple_info("operand has value %"PRIi64, expr->right->value.i);
                        });
                    }
                } else {
                    if (expr->right->value.u > UINT32_MAX) {
                        EMBC(expr->right, {
                            report_range_err_no_exit(&expr->right->loc, "index operand in array subscript expression is too large");
                            report_simple_info("operand has value %"PRIu64", but the maximum is %u",
                                            expr->right->value.u, UINT32_MAX);
                        });
                    }
                }

                array_length = get_array_length(expr->left->type);

                if (expr->right->value.u >= array_length) {
                    EMBC(expr->right, {
                        report_range_err_no_exit(&expr->right->loc, "array subscript expression will result in out of bounds access");
                        report_simple_info("index has value %"PRIu64", but the maximum is %u",
                                           expr->right->value.u, array_length - 1);
                    });
                }
            }

            ASTP(expr)->flags |= (expr->left->flags & AST_FLAG_EXPR_CAN_BE_LVAL);

            break;

        case OP_PLUS:
            check_add(cxt, expr);
            break;
        case OP_MINUS:
            check_sub(cxt, expr);
            break;
        case OP_MULT:
            if (tkr == TY_TYPE && tkl == TY_GENERIC_INT) {
                if (INT_TYPE_IS_SIGNED(expr->left->type)) {
                    EMBC(expr->left, {
                        report_range_err_no_exit(&expr->left->loc, "size operand in array type expression must be unsigned");
                        report_simple_info("operand has type %s",
                                           get_string(get_type_string_id(expr->left->type)));
                    });
                }

                ASSERT(expr->right->flags & AST_FLAG_CONSTANT, "type operand should be constant");
                if (!(expr->left->flags & AST_FLAG_CONSTANT)) {
                    EMBC(expr->left, {
                        report_range_err(&expr->left->loc, "size operand in array type expression must be known at compile time");
                    });
                }

                if (expr->left->value.u > UINT32_MAX) {
                    EMBC(expr->left, {
                        report_range_err_no_exit(&expr->left->loc, "size operand in array type expression is too large");
                        report_range_info_no_context(&expr->right->loc,
                                                "operand has value %"PRIu64", but the maximum is %u",
                                                expr->left->value.u, UINT32_MAX);
                    });
                } else if (expr->left->value.u == 0) {
                    EMBC(expr->left, {
                        report_range_err(&expr->left->loc, "size operand in array type expression is zero, which is not allowed");
                    });
                }

                ASTP(expr)->flags |= AST_FLAG_CONSTANT;
                ASTP(expr)->type   = TY_TYPE;
                ASTP(expr)->value.t = get_array_type(expr->right->value.t, (u32)expr->left->value.u);
            } else {
                check_mul(cxt, expr);
            }
            break;
        case OP_DIV:
            check_div(cxt, expr);
            break;
        case OP_MOD:
            check_mod(cxt, expr);
            break;

        case OP_EQU:
        case OP_NEQ:
        case OP_LSS:
        case OP_LEQ:
        case OP_GTR:
        case OP_GEQ:
            check_cmp(cxt, expr);
            ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);
            break;

        case OP_PLUS_ASSIGN:
            check_add(cxt, expr);
            ASSERT(ASTP(expr)->type == expr->left->type, "type should not change in assignment");
            goto lval_check;
            break;
        case OP_MINUS_ASSIGN:
            check_sub(cxt, expr);
            ASSERT(ASTP(expr)->type == expr->left->type, "type should not change in assignment");
            goto lval_check;
            break;
        case OP_MULT_ASSIGN:
            check_mul(cxt, expr);
            ASSERT(ASTP(expr)->type == expr->left->type, "type should not change in assignment");
            goto lval_check;
            break;
        case OP_DIV_ASSIGN:
            check_div(cxt, expr);
            ASSERT(ASTP(expr)->type == expr->left->type, "type should not change in assignment");
            goto lval_check;
            break;
        case OP_MOD_ASSIGN:
            check_mod(cxt, expr);
            ASSERT(ASTP(expr)->type == expr->left->type, "type should not change in assignment");
            goto lval_check;
            break;
        case OP_ASSIGN:
            if (expr->left->type != expr->right->type) {
                EMBC(expr->left, {
                    EMBC(expr->right, {
                        report_range_err(&ASTP(expr)->loc,
                                         "left-hand-side operand has type %s, but you're trying to assign it a value of type %s",
                                         get_string(get_type_string_id(expr->left->type)),
                                         get_string(get_type_string_id(expr->right->type)));
                    });
                });
                return;
            }
            ASTP(expr)->type  = expr->left->type;
            ASTP(expr)->value = expr->right->value;

lval_check:;
            if (!(expr->left->flags & AST_FLAG_EXPR_CAN_BE_LVAL)) {
                EMBC(expr->left, {
                    if (expr->op == OP_ASSIGN) {
                        report_range_err_no_exit(&ASTP(expr)->loc,
                                                 "left-hand-side of assignment operator can not be assigned to");
                        report_fixit(expr->op_loc, "did you mean to write an equality comparison?\a==");
                    } else {
                        report_range_err(&ASTP(expr)->loc,
                                         "left-hand-side of assignment operator can not be assigned to");
                    }
                });
            }

            ASTP(expr)->flags &= ~AST_FLAG_CONSTANT;

            break;

        case OP_AND:
            check_logical(cxt, expr);
            ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);
            if (ASTP(expr)->flags & AST_FLAG_CONSTANT) {
                ASTP(expr)->value.s = expr->left->value.s && expr->right->value.s;
            }
            break;
        case OP_OR:
            check_logical(cxt, expr);
            ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);
            if (ASTP(expr)->flags & AST_FLAG_CONSTANT) {
                ASTP(expr)->value.s = expr->left->value.s || expr->right->value.s;
            }
            break;

        case OP_BSHL:
        case OP_BSHR:
        case OP_BAND:
        case OP_BXOR:
        case OP_BOR:
            if (type_kind(expr->left->type)  != TY_GENERIC_INT
            ||  type_kind(expr->right->type) != TY_GENERIC_INT) {

                binop_bad_type_error(expr);
            }
            ASTP(expr)->type   = expr->left->type;
            ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);

            if (ASTP(expr)->flags & AST_FLAG_CONSTANT) {
                switch (expr->op) {
                    case OP_BSHL: ASTP(expr)->value.u = expr->left->value.u << expr->right->value.u; break;
                    case OP_BSHR: ASTP(expr)->value.u = expr->left->value.u >> expr->right->value.u; break;
                    case OP_BAND: ASTP(expr)->value.u = expr->left->value.u && expr->right->value.u; break;
                    case OP_BXOR: ASTP(expr)->value.u = expr->left->value.u ^  expr->right->value.u; break;
                    case OP_BOR:  ASTP(expr)->value.u = expr->left->value.u |  expr->right->value.u; break;
                }
            }

            break;

        default:
            ASSERT(0, "unhandled binary operator");
            return;
    }

    if (expr->op != OP_CALL) {
        check_expr_unsatisfied_poly(cxt, expr->left);
        check_expr_unsatisfied_poly(cxt, expr->right);
    }
}

static void check_unary_expr(check_context_t cxt, ast_unary_expr_t *expr) {
    check_node(cxt, expr->child);

    if (expr->child->type == TY_NOT_TYPED) {
        operand_not_typed_error(ASTP(expr), expr->child);
        return;
    }

    if (TYPE_IS_GENERIC(expr->child->type)) {
        force_generic_realization(expr->child);
    }

    switch (expr->op) {
        case OP_ADDR:
            if (expr->child->type == TY_TYPE || expr->child->type == TY_POLY) {
                ASTP(expr)->type    = TY_TYPE;
                ASTP(expr)->value.t = get_ptr_type(expr->child->value.t);

                ASSERT(expr->child->flags & AST_FLAG_CONSTANT, "type not constant");
                ASTP(expr)->flags |= AST_FLAG_CONSTANT;
            } else {
                if (!(expr->child->flags & AST_FLAG_EXPR_CAN_BE_LVAL)) {
                    EMBC(expr->child, {
                        report_range_err(&ASTP(expr)->loc,
                                        "can't take the address of a non-addressable expression");
                    });
                }
                ASTP(expr)->type = get_ptr_type(expr->child->type);
            }
            break;
        case OP_SLICE:
            if (expr->child->type == TY_TYPE || expr->child->type == TY_POLY) {
                ASTP(expr)->type    = TY_TYPE;
                ASTP(expr)->value.t = get_slice_type(expr->child->value.t);

                ASSERT(expr->child->flags & AST_FLAG_CONSTANT, "type not constant");
                ASTP(expr)->flags |= AST_FLAG_CONSTANT;
            } else {
                EMBC(expr->child, {
                    report_range_err_no_exit(&ASTP(expr)->loc,
                                             "right-hand-side operand of '[' slice operator must be a type");
                    report_range_info_no_context(&expr->child->loc,
                                                 "operand has type %s",
                                                 get_string(get_type_string_id(expr->child->type)));
                });
            }
            break;
        case OP_DEREF:
            if (expr->child->type == TY_TYPE) {
                ASTP(expr)->type    = TY_TYPE;
                if (type_kind(expr->child->value.t) != TY_PTR) {
                    EMBC(expr->child, {
                        report_range_err_no_exit(&ASTP(expr)->loc,
                                                 "right-hand-side operand of '@' operator must be a pointer");
                        report_range_info_no_context(&expr->child->loc,
                                                     "operand is the type value %s",
                                                     get_string(get_type_string_id(expr->child->value.t)));
                    });
                }
                ASTP(expr)->value.t = get_under_type(expr->child->value.t);
            } else {
                if (type_kind(expr->child->type) != TY_PTR) {
                    EMBC(expr->child, {
                        report_range_err_no_exit(&ASTP(expr)->loc,
                                                 "right-hand-side operand of '@' operator must be a pointer");
                        report_range_info_no_context(&expr->child->loc,
                                                     "operand has type %s",
                                                     get_string(get_type_string_id(expr->child->type)));
                    });
                }
                ASTP(expr)->type   = get_under_type(expr->child->type);
                ASTP(expr)->flags |= AST_FLAG_EXPR_CAN_BE_LVAL;
            }
            break;
        case OP_NOT:
            if (!type_kind_is_int(type_kind(expr->child->type))) {
                EMBC(expr->child, {
                    report_range_err_no_exit(&ASTP(expr)->loc,
                                             "right-hand-side operand of 'not' operator must be an integer");
                    report_range_info_no_context(&expr->child->loc,
                                                 "operand has type %s",
                                                 get_string(get_type_string_id(expr->child->type)));
                });
            }
            ASTP(expr)->type   = expr->child->type;
            ASTP(expr)->flags |= expr->child->flags & AST_FLAG_CONSTANT;

            if (INT_TYPE_IS_SIGNED(expr->child->type)) {
                ASTP(expr)->value.i = !expr->child->value.i;
            } else {
                ASTP(expr)->value.u = !expr->child->value.u;
            }

            break;
        case OP_NEG:
            if (!type_kind_is_numeric(type_kind(expr->child->type))) {
                EMBC(expr->child, {
                    report_range_err_no_exit(&ASTP(expr)->loc,
                                             "right-hand-side operand of '-' operator must be numeric");
                    report_range_info_no_context(&expr->child->loc,
                                                 "operand has type %s",
                                                 get_string(get_type_string_id(expr->child->type)));
                });
            }

            if (type_kind_is_int(type_kind(expr->child->type))
            &&  !INT_TYPE_IS_SIGNED(expr->child->type)) {
                EMBC(expr->child, {
                    report_range_err_no_exit(&ASTP(expr)->loc,
                                             "right-hand-side operand of '-' operator must be signed");
                    report_range_info_no_context(&expr->child->loc,
                                                 "operand has type %s",
                                                 get_string(get_type_string_id(expr->child->type)));
                });
            }

            ASTP(expr)->type   = expr->child->type;
            ASTP(expr)->flags |= expr->child->flags & AST_FLAG_CONSTANT;

            ASTP(expr)->value.i = -expr->child->value.i;

            break;
        case OP_SIZEOF:
            if (type_kind(expr->child->type) != TY_TYPE) {
                EMBC(expr->child, {
                    report_range_err_no_exit(&ASTP(expr)->loc,
                                             "right-hand-side operand of 'sizeof' operator must be a type");
                    report_range_info_no_context(&expr->child->loc,
                                                 "operand has type %s",
                                                 get_string(get_type_string_id(expr->child->type)));
                });
            }
            ASTP(expr)->type     = TY_S64;
            ASTP(expr)->flags   |= expr->child->flags & AST_FLAG_CONSTANT;
            ASTP(expr)->value.s  = (i64)type_size(expr->child->value.t, NULL);
            break;
        case OP_TYPEOF:
            ASTP(expr)->type     = TY_TYPE;
            ASTP(expr)->flags   |= expr->child->flags & AST_FLAG_CONSTANT;
            ASTP(expr)->value.t  = expr->child->type;
            break;
        case OP_LENOF:
            if (type_kind(expr->child->type) != TY_ARRAY && type_kind(expr->child->type) != TY_SLICE) {
                EMBC(expr->child, {
                    report_range_err_no_exit(&ASTP(expr)->loc,
                                             "right-hand-side operand of 'lenof' operator must be an array or slice");
                    report_range_info_no_context(&expr->child->loc,
                                                 "operand has type %s",
                                                 get_string(get_type_string_id(expr->child->type)));
                });
            }
            ASTP(expr)->type   = TY_S64;
            ASTP(expr)->flags |= expr->child->flags;

            if (type_kind(expr->child->type) == TY_ARRAY) {
                ASTP(expr)->value.u = get_array_length(expr->child->type);
                ASTP(expr)->flags |= AST_FLAG_CONSTANT;
            }

            break;
        default:
            report_loc_err_no_exit(ASTP(expr)->loc.beg, "UNHANDLED OPERATOR: %s", OP_STR(expr->op));
            ASSERT(0, "unhandled unary operator");
            return;
    }

    check_expr_unsatisfied_poly(cxt, expr->child);
}

static void check_expr(check_context_t cxt, ast_t *expr) {
    if (expr->kind == AST_BIN_EXPR) {
        check_bin_expr(cxt, (ast_bin_expr_t*)expr);
    } else if (expr->kind == AST_UNARY_EXPR) {
        check_unary_expr(cxt, (ast_unary_expr_t*)expr);
    } else {
        ASSERT(0, "not an expression");
    }

    if (expr->flags & AST_FLAG_EXPR_TOP) {
        check_expr_unsatisfied_poly(cxt, expr);
    }
}

static void check_struct(check_context_t cxt, ast_struct_t *st) {
    scope_t  *new_scope;
    u32       n_params;
    u32      *param_types;
    int       i;
    ast_t   **it;

    new_scope = st->scope;

    cxt.unit_decl = cxt.parent_decl;
    cxt.scope     = new_scope;

    ASTP(st)->type = ASTP(cxt.parent_decl)->type = TY_TYPE;

    n_params    = array_len(st->params);
    param_types = alloca(sizeof(u32) * n_params);
    i           = 0;
    array_traverse(st->params, it) {
        check_node(cxt, *it);
        param_types[i] = (*it)->type;
        i += 1;
    }

    check_tags(cxt, cxt.parent_decl, &(cxt.parent_decl)->tags);

    if (!(cxt.flags & CHECK_FLAG_MONOMORPH)) {
        ASTP(st)->value.t = get_struct_type(cxt.parent_decl);
    } else {
        ASTP(st)->value.t = get_struct_mono_type(cxt.parent_decl, cxt.monomorph_idx);
    }

    /* @bad?, @refactor
    ** We have to bubble this type up to the declaration so that identifier lookups
    ** that occur before we return from this routine can get the right type.
    ** I'm not sure if this should just be done everywhere like this (shouldn't
    ** be _that_ many spots), or if something a little smarter should be done.
    */
    if (!(cxt.flags & CHECK_FLAG_MONOMORPH)) {
        ASTP(cxt.parent_decl)->type  = ASTP(st)->type;
        ASTP(cxt.parent_decl)->value = ASTP(st)->value;
    }

    if ((ASTP(cxt.parent_decl)->flags & AST_FLAG_POLYMORPH)
    &&  !(cxt.flags & CHECK_FLAG_MONOMORPH)) {

        array_push(all_types, cxt.parent_decl);
        goto skip_children;
    }

    cxt.flags &= ~CHECK_FLAG_MONOMORPH;

    array_traverse(st->fields, it) {
        check_node(cxt, *it);
    }

    array_traverse(st->children, it) {
        check_node(cxt, *it);
    }

    ASTP(st)->flags |= AST_FLAG_CHECKED;

    if (!(ASTP(cxt.parent_decl)->flags & AST_FLAG_POLYMORPH)) {
        array_push(all_types, cxt.parent_decl);
    }

skip_children:;
}

static void check_block(check_context_t cxt, ast_block_t *block) {
    int     cf_must_return;
    int     cf_must_skip_loop;
    u32     idx;
    u32     j;
    ast_t **it;
    ast_t  *next;

    ASTP(block)->type = TY_NOT_TYPED;
    cxt.scope  = ((ast_block_t*)ASTP(block))->scope;

    cf_must_return    = 0;
    cf_must_skip_loop = 0;

    idx = 0;
    array_traverse(((ast_block_t*)ASTP(block))->stmts, it) {
        check_node(cxt, *it);

        next = NULL;

        for (j = idx + 1; next == NULL && j < array_len(((ast_block_t*)ASTP(block))->stmts); j += 1) {
            if ((*(ast_t**)array_item(((ast_block_t*)ASTP(block))->stmts, j))->kind != AST_DEFER) {
                next = *(ast_t**)array_item(((ast_block_t*)ASTP(block))->stmts, j);
            }
        }

        switch ((*it)->kind) {
            case AST_BLOCK:
            case AST_IF:
            case AST_LOOP:
            case AST_DEFER:
            case AST_RETURN:
                cf_must_return |= (*it)->flags & AST_FLAG_CF_MUST_RETURN;
                if (cf_must_return && (*it)->kind != AST_DEFER && next != NULL) {
                    switch (next->kind) {
                        case AST_BLOCK:
                            if (array_len(((ast_block_t*)next)->stmts) > 0) {
                                report_loc_err(next->loc.beg, "this statement is unreachable because previous statement always returns");
                            }
                            break;
                        default:
                            report_range_err(&next->loc, "this statement is unreachable because previous statement always returns");
                            break;
                    }
                }
                break;
        }

        switch ((*it)->kind) {
            case AST_BLOCK:
            case AST_IF:
            case AST_LOOP:
            case AST_DEFER:
            case AST_RETURN:
            case AST_BREAK:
            case AST_CONTINUE:
                cf_must_skip_loop |= (*it)->flags & AST_FLAG_CF_MUST_SKIP_LOOP_BODY;
                if (cxt.flags & CHECK_FLAG_IN_LOOP && cf_must_skip_loop && (*it)->kind != AST_DEFER && next != NULL) {
                    switch (next->kind) {
                        case AST_BLOCK:
                            if (array_len(((ast_block_t*)next)->stmts) > 0) {
                                report_loc_err(next->loc.beg, "this statement is unreachable because previous statement always exits the loop body");
                            }
                            break;
                        default:
                            report_range_err(&next->loc, "this statement is unreachable because previous statement always exits the loop body");
                            break;
                    }
                }
                break;
        }
        idx += 1;
    }

    ASTP(block)->flags |= cf_must_return | cf_must_skip_loop;
}

static void check_arg_list(check_context_t cxt, ast_arg_list_t *arg_list) {
    int     i;
    arg_t  *arg;
    type_t  list_type;
    int     j;
    arg_t   new_arg;

    ASTP(arg_list)->type = TY_NOT_TYPED;

    array_traverse(arg_list->args, arg) {
        check_node(cxt, arg->expr);
        if (arg->expr->type == TY_NOT_TYPED) {
            EMBC(arg->expr, {
                report_range_err(&arg->expr->loc,
                                 "invalid use of expression which does not have a type");
                return;
            });
        }
    }

again:;
    i = 0;
    array_traverse(arg_list->args, arg) {
        if (type_kind(arg->expr->type) == _TY_TYPE_LIST) {
            array_delete(arg_list->args, i);

            list_type = get_type_t(arg->expr->type);

            for (j = 0; j < list_type.list_len; j += 1) {
                new_arg.name = arg->name;
                new_arg.expr = copy_tree(arg->expr, cxt.scope);

                check_node(cxt, new_arg.expr);

                new_arg.expr->type = list_type.id_list[j];

                if (new_arg.expr->kind == AST_IDENT) {
                    ((ast_ident_t*)new_arg.expr)->varg_idx = j;
                }

                array_insert(arg_list->args, i + j, new_arg);
            }

            goto again;
        }
        i += 1;
    }
}

static void check_if(check_context_t cxt, ast_if_t *_if) {
    scope_t *orig_scope;
    scope_t *new_scope;
    int      cf_must_return;
    int      cf_must_skip_loop;

    ASTP(_if)->type = TY_NOT_TYPED;

    new_scope = _if->scope;

    orig_scope = cxt.scope;
    cxt.scope  = new_scope;

    check_node(cxt, _if->expr);

    if (!type_kind_is_int(type_kind(_if->expr->type))) {
        EMBC(_if->expr, {
            report_range_err_no_exit(&_if->expr->loc, "'if' condition must have an integer type");
            report_simple_info("got %s", get_string(get_type_string_id(_if->expr->type)));
        });
        return;
    }

    check_node(cxt, _if->then_block);

    cf_must_return    = _if->then_block->flags & AST_FLAG_CF_MUST_RETURN;
    cf_must_skip_loop = _if->then_block->flags & AST_FLAG_CF_MUST_SKIP_LOOP_BODY;

    if (_if->els != NULL) {
        cxt.scope = orig_scope;
        check_node(cxt, _if->els);

        cf_must_return &= _if->els->flags & AST_FLAG_CF_MUST_RETURN;
        if (cf_must_return) {
            ASTP(_if)->flags |= cf_must_return;
        }
        cf_must_skip_loop &= _if->els->flags & AST_FLAG_CF_MUST_SKIP_LOOP_BODY;
        if (cf_must_skip_loop) {
            ASTP(_if)->flags |= cf_must_skip_loop;
        }
    }
}

static void check_loop(check_context_t cxt, ast_loop_t *loop) {
    scope_t *new_scope;

    cxt.flags |= CHECK_FLAG_IN_LOOP;
    cxt.flags &= ~CHECK_FLAG_DEFER_IN_LOOP;

    ASTP(loop)->type = TY_NOT_TYPED;

    new_scope = loop->scope;

    cxt.scope = new_scope;

    if (loop->init != NULL) {
        check_node(cxt, loop->init);
    }
    if (loop->cond != NULL) {
        check_node(cxt, loop->cond);
        if (!type_kind_is_int(type_kind(loop->cond->type))) {
            EMBC(loop->cond, {
                report_range_err_no_exit(&loop->cond->loc, "loop condition must have an integer type");
                report_simple_info("got %s", get_string(get_type_string_id(loop->cond->type)));
            });
            return;
        }
    }
    if (loop->post != NULL) {
        check_node(cxt, loop->post);
    }

    check_node(cxt, loop->block);
}

static void check_break(check_context_t cxt, ast_continue_t *brk) {
    ASTP(brk)->type = TY_NOT_TYPED;

    if (!(cxt.flags & CHECK_FLAG_IN_LOOP)) {
        report_range_err(&ASTP(brk)->loc, "break statement only allowed within a loop");
        return;
    }

    if (cxt.flags & CHECK_FLAG_DEFER_IN_LOOP) {
        report_range_err(&ASTP(brk)->loc, "break statements are not allowed in a defer block");
        return;
    }

    ASTP(brk)->flags |= AST_FLAG_CF_MUST_SKIP_LOOP_BODY;
}

static void check_continue(check_context_t cxt, ast_continue_t *cont) {
    ASTP(cont)->type = TY_NOT_TYPED;

    if (!(cxt.flags & CHECK_FLAG_IN_LOOP)) {
        report_range_err(&ASTP(cont)->loc, "continue statement only allowed within a loop");
        return;
    }

    if (cxt.flags & CHECK_FLAG_DEFER_IN_LOOP) {
        report_range_err(&ASTP(cont)->loc, "break statements are not allowed in a defer block");
        return;
    }

    ASTP(cont)->flags |= AST_FLAG_CF_MUST_SKIP_LOOP_BODY;
}

static void check_return(check_context_t cxt, ast_return_t *ret) {
    if (cxt.proc == NULL) {
        report_range_err(&ASTP(ret)->loc, "return statement only allowed in a procedure body");
        return;
    }

    if (cxt.flags & CHECK_FLAG_IN_DEFER) {
        report_range_err(&ASTP(ret)->loc, "return statements are not allowed in a defer block");
        return;
    }

    if (ret->expr != NULL) {
        check_node(cxt, ret->expr);

        if (cxt.proc->ret_type_expr == NULL) {
            report_range_err(&ret->expr->loc,
                             "attempting to return %s in a procedure that does not return a value",
                             get_string(get_type_string_id(ret->expr->value.t)));
            return;
        }

        if (!types_are_compatible(cxt.proc->ret_type_expr->value.t, ret->expr->type)
        &&  cxt.proc->ret_type_expr->value.t != TY_POLY) {
            EMBC(ret->expr, {
                report_range_err(&ret->expr->loc,
                                 "incorrect type of returned expression: expected %s, but got %s",
                                 get_string(get_type_string_id(cxt.proc->ret_type_expr->value.t)),
                                 get_string(get_type_string_id(ret->expr->type)));
            });
            return;
        }
    } else if (cxt.proc->ret_type_expr != NULL) {
        report_range_err(&ASTP(ret)->loc,
                         "return statement missing expression in a procedure that returns %s", get_string(get_type_string_id(cxt.proc->ret_type_expr->value.t)));
        return;
    }

    ASTP(ret)->type   = TY_NOT_TYPED;
    ASTP(ret)->flags |= AST_FLAG_CF_MUST_RETURN | AST_FLAG_CF_MUST_SKIP_LOOP_BODY;
}

static void check_defer(check_context_t cxt, ast_defer_t *defer) {
    ASTP(defer)->type = TY_NOT_TYPED;

    cxt.flags |= CHECK_FLAG_IN_DEFER;

    if (cxt.flags & CHECK_FLAG_IN_LOOP) {
        cxt.flags |= CHECK_FLAG_DEFER_IN_LOOP;
    }

    check_node(cxt, defer->block);

    ASTP(defer)->flags |= defer->block->flags & AST_FLAG_CF_MUST_RETURN;
    ASTP(defer)->flags |= defer->block->flags & AST_FLAG_CF_MUST_SKIP_LOOP_BODY;
}

static void check_vargs_block(check_context_t cxt, ast_vargs_block_t *vargs_block) {
    ast_proc_t *proc;
    u32         vargs_ty;
    type_t      list_type;
    u32         i;
    ast_t      *new;

    ASTP(vargs_block)->type = TY_NOT_TYPED;

    ASSERT(cxt.proc != NULL, "should be in a proc");

    proc = cxt.proc;
    ASSERT(ASTP(proc)->flags & AST_FLAG_POLY_VARARGS, "I think we only want to do this for poly vargs");

    cxt.scope  = ((ast_block_t*)vargs_block->block)->scope;
    cxt.flags |= CHECK_FLAG_IN_VARGS;

    ASSERT(cxt.scope->parent != NULL, "this should never be global");

    vargs_ty = current_poly_vargs_type(cxt);

    /* @note: will this always work correctly when we have macro expansions within a vargs block? */

    vargs_block->new_blocks = array_make(ast_t*);
    list_type               = get_type_t(vargs_ty);

    for (i = 0; i < list_type.list_len; i += 1) {
        cxt.varg_ty = list_type.id_list[i];
        new = copy_tree(vargs_block->block, cxt.scope->parent);
        check_node(cxt, new);
        array_push(vargs_block->new_blocks, new);
    }
}


static void check_node(check_context_t cxt, ast_t *node) {
    ast_t **it;

    if (node->kind == AST_IGNORE_NODE) { return; }

    /* @bad @todo
    ** This isn't really correct, but we're doing so much unneeded work by rechecking things.
    */
/*     if (node->type != TY_UNKNOWN && !(cxt.flags & CHECK_FLAG_FORCE_RECHECK)) { return; } */
    if (node->flags & AST_FLAG_CHECKED) { return; }

    if (node->macro_decl != NULL) {
        push_macro_breadcrumb(node);
    }

    switch (node->kind) {
#define X(_kind) case _kind:
        X_AST_DECLARATIONS
#undef X
            check_decl(cxt, (ast_decl_t*)node);
            break;
        case AST_MODULE:
            /*
             * Prevent some recursion issues where nodes in the module reference
             * the module and asking for the type would recurse infinitely.
             */
            node->type                  = TY_MODULE;
            ASTP(cxt.parent_decl)->type = TY_MODULE;

            check_tags(cxt, cxt.parent_decl, &(cxt.parent_decl)->tags);

            cxt.scope = get_subscope_from_node(cxt.scope, node);
            ASSERT(cxt.scope != NULL, "did not get subscope");
            array_traverse(((ast_module_t*)node)->children, it) {
                if (!(cxt.flags & CHECK_FLAG_MONOMORPH) || !((*it)->flags & AST_FLAG_POLYMORPH)) {
                    check_node(cxt, *it);
                }
            }

            break;
        case AST_MACRO:
            /* @todo */
            node->type = TY_MACRO;
            break;
        case AST_PROC:
            check_proc(cxt, (ast_proc_t*)node);
            break;
        case AST_PARAM:
            check_param(cxt, (ast_param_t*)node);
            break;
        case AST_STRUCT:
            check_struct(cxt, (ast_struct_t*)node);
            break;
        case AST_BLOCK:
            check_block(cxt, (ast_block_t*)node);
            break;
        case AST_BIN_EXPR:
        case AST_UNARY_EXPR:
            check_expr(cxt, node);
            break;
        case AST_INT:
            check_int(cxt, (ast_int_t*)node);
            break;
        case AST_FLOAT:
            check_float(cxt, (ast_int_t*)node);
            break;
        case AST_STRING:
            check_string(cxt, (ast_string_t*)node);
            break;
        case AST_CHAR:
            check_char(cxt, (ast_char_t*)node);
            break;
        case AST_IDENT:
            check_ident(cxt, (ast_ident_t*)node);
            break;
        case AST_PROC_TYPE:
            check_proc_type(cxt, (ast_proc_type_t*)node);
            break;
        case AST_ARG_LIST:
            check_arg_list(cxt, (ast_arg_list_t*)node);
            break;
        case AST_IF:
            check_if(cxt, (ast_if_t*)node);
            break;
        case AST_LOOP:
            check_loop(cxt, (ast_loop_t*)node);
            break;
        case AST_CONTINUE:
            check_continue(cxt, (ast_continue_t*)node);
            break;
        case AST_BREAK:
            check_break(cxt, (ast_continue_t*)node);
            break;
        case AST_RETURN:
            check_return(cxt, (ast_return_t*)node);
            break;
        case AST_DEFER:
            check_defer(cxt, (ast_defer_t*)node);
            break;
        case AST_BUILTIN:
            break;
        case AST_COMPILE_ERROR:
            report_range_err(&node->loc, "%s", get_string(((ast_compile_error_t*)node)->message));
            break;
        case AST_VARGS_BLOCK:
            check_vargs_block(cxt, (ast_vargs_block_t*)node);
            break;
        default:;
#ifdef SIMON_DO_ASSERTIONS
            report_simple_err_no_exit("INTERNAL ERROR: AST_%s unhandled in check_node()", ast_get_kind_str(node->kind));
            print_node(node);
            common_exit(1);
#endif
    }

#ifdef SIMON_DO_ASSERTIONS
    if (node->type == TY_UNKNOWN) {
        report_simple_err_no_exit("INTERNAL ERROR: type not resolved in check_node()");
        print_node(node);
        common_exit(1);
    }
    ASSERT(node->type != TY_UNKNOWN, "did not resolve type");
#endif

    if (node->macro_decl != NULL) {
        pop_breadcrumb();
    }
}
