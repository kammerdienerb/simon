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
    array_t         *polymorphs;
    check_context_t  cxt;
    src_range_t      range;
} poly_backlog_entry_t;

static array_t poly_backlog;
static array_t cycle_check_path;

static void check_node(check_context_t cxt, ast_t *node);


#define AST_ALLOC(t) \
    (bump_alloc(&(get_tls()->bump_alloc), sizeof(t)))

#define CPY_WHOLE(dst, src) (memcpy((void*)(dst), (void*)(src), sizeof(__typeof(*(dst)))))

#define ALLOC_CPY(dst, src)               \
do {                                      \
    dst = AST_ALLOC(__typeof(*dst));      \
    CPY_WHOLE(dst, src);                  \
    ASTP(dst)->flags |= AST_FLAG_IS_COPY; \
} while (0)

#define CPY_FIELD(dst, src, nm) ((dst)->nm = copy_tree(((__typeof(dst))(src))->nm));


static ast_t *copy_tree(ast_t *node) {

    if (node == NULL) { return NULL; }

    switch (node->kind) {
#define X(_kind) case _kind:
        X_AST_DECLARATIONS
#undef X
        {
            ast_decl_t  *decl; ALLOC_CPY(decl, node);
            ast_t      **it;
            ast_t       *new;

            CPY_FIELD(decl, node, type_expr);
            CPY_FIELD(decl, node, val_expr);

            decl->tags = array_make(ast_t*);
            array_traverse(((ast_decl_t*)node)->tags, it) {
                new = copy_tree(*it);
                array_push(decl->tags, new);
            }

            return ASTP(decl);
        }
        case AST_STATIC_IF:           goto unhandled;
        case AST_STATIC_ASSERT: {
            ast_static_assert_t *asrt; ALLOC_CPY(asrt, node);

            CPY_FIELD(asrt, node, expr);

            return ASTP(asrt);
        }
        case AST_STATIC_COMMENT: { ast_static_comment_t *sc; ALLOC_CPY(sc, node); return ASTP(sc); }
        case AST_STATIC_ERROR:   { ast_static_error_t   *se; ALLOC_CPY(se, node); return ASTP(se); }
        case AST_STATIC_VARGS: {
            ast_static_vargs_t *vargs; ALLOC_CPY(vargs, node);

            CPY_FIELD(vargs, node, block);

            return ASTP(vargs);
        }
        case AST_MODULE: {
            ast_module_t  *mod; ALLOC_CPY(mod, node);
            ast_t        **it;
            ast_t         *new;

            mod->children = array_make(ast_t*);
            array_traverse(((ast_module_t*)node)->children, it) {
                new = copy_tree(*it);
                array_push(mod->children, new);
            }

            return ASTP(mod);
        }
        case AST_PROC: {
            ast_proc_t  *proc; ALLOC_CPY(proc, node);
            ast_t      **it;
            ast_t       *new;

            proc->params = array_make(ast_t*);
            array_traverse(((ast_proc_t*)node)->params, it) {
                new = copy_tree(*it);
                array_push(proc->params, new);
            }
            CPY_FIELD(proc, node, ret_type_expr);
            CPY_FIELD(proc, node, block);
            proc->polymorphs = array_make(polymorphed_t);

            return ASTP(proc);
        }
        case AST_STRUCT: {
            ast_struct_t  *st; ALLOC_CPY(st, node);
            ast_t        **it;
            ast_t         *new;

            st->params = array_make(ast_t*);
            array_traverse(((ast_struct_t*)node)->params, it) {
                new = copy_tree(*it);
                array_push(st->params, new);
            }
            st->fields = array_make(ast_t*);
            array_traverse(((ast_struct_t*)node)->fields, it) {
                new = copy_tree(*it);
                array_push(st->fields, new);
            }
            st->children = array_make(ast_t*);
            array_traverse(((ast_struct_t*)node)->children, it) {
                new = copy_tree(*it);
                array_push(st->children, new);
            }
            st->polymorphs = array_make(polymorphed_t);

            return ASTP(st);
        }
        case AST_MACRO: goto unhandled;
        case AST_PARAM: {
            ast_param_t *param; ALLOC_CPY(param, node);

            CPY_FIELD(param, node, type_expr);
            CPY_FIELD(param, node, val);

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

            return ASTP(ident);
        }
        case AST_UNARY_EXPR: {
            ast_unary_expr_t *expr; ALLOC_CPY(expr, node);

            CPY_FIELD(expr, node, child);
            CPY_FIELD(expr, node, array_size_expr);

            return ASTP(expr);
        }
        case AST_BIN_EXPR: {
            ast_bin_expr_t *expr; ALLOC_CPY(expr, node);

            CPY_FIELD(expr, node, left);
            CPY_FIELD(expr, node, right);

            return ASTP(expr);
        }
        case AST_BLOCK:
        case AST_SD_BLOCK: {
            ast_block_t  *block; ALLOC_CPY(block, node);
            ast_t       **it;
            ast_t        *new;

            block->stmts = array_make(ast_t*);
            array_traverse(((ast_block_t*)node)->stmts, it) {
                new = copy_tree(*it);
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
                new_arg.expr = copy_tree(it->expr);
                array_push(arg_list->args, new_arg);
            }

            return ASTP(arg_list);
        }
        case AST_IF: {
            ast_if_t *_if; ALLOC_CPY(_if, node);

            CPY_FIELD(_if, node, expr);
            CPY_FIELD(_if, node, then_block);
            CPY_FIELD(_if, node, els);

            return ASTP(_if);
        }
        case AST_LOOP: {
            ast_loop_t *loop; ALLOC_CPY(loop, node);

            CPY_FIELD(loop, node, init);
            CPY_FIELD(loop, node, cond);
            CPY_FIELD(loop, node, post);
            CPY_FIELD(loop, node, block);

            return ASTP(loop);
        }
        case AST_RETURN: {
            ast_return_t *ret; ALLOC_CPY(ret, node);

            CPY_FIELD(ret, node, expr);

            return ASTP(ret);
        }
        case AST_DEFER: {
            ast_defer_t *defer; ALLOC_CPY(defer, node);

            CPY_FIELD(defer, node, block);

            return ASTP(defer);
        }
        case AST_BREAK:    { ast_break_t    *brk; ALLOC_CPY(brk, node); return ASTP(brk); }
        case AST_CONTINUE: { ast_continue_t *cnt; ALLOC_CPY(cnt, node); return ASTP(cnt); }
        default:
        unhandled:;
    }

#ifdef SIMON_DO_ASSERTIONS
    report_range_err_no_exit(&node->loc, "INTERNAL ERROR: AST_%s unhandled in copy_tree()", ast_get_kind_str(node->kind));
    ASSERT(0, "unhandled AST node kind in copy_tree()");
#endif

    return NULL;
}

void check_all(void) {
    scope_t               *entry_scope;
    ast_t                **rootp;
    check_context_t        cxt;
    poly_backlog_entry_t  *backlog_entry_p;
    poly_backlog_entry_t   backlog_entry;
    polymorphed_t         *polymorphed;
    char                   buff[512];
    const char            *lazy_comma;
    polymorph_constant_t  *it;

    if (array_len(roots) == 0) {
        report_simple_err("no meaningful input provided");
        return;
    }

    poly_backlog     = array_make(poly_backlog_entry_t);
    cycle_check_path = array_make(ast_t*);

    memset(&cxt, 0, sizeof(cxt));

    cxt.scope = global_scope;

    array_traverse(roots, rootp) {
        check_node(cxt, *rootp);
    }

/*     I("======================== BEGIN BACKLOGS ==========================="); */

    while (array_len(poly_backlog) > 0) {
        backlog_entry_p = array_last(poly_backlog);
        memcpy(&backlog_entry, backlog_entry_p, sizeof(backlog_entry));
        array_pop(poly_backlog);

        polymorphed = array_item(*backlog_entry.polymorphs, backlog_entry.cxt.poly_constants_idx);

        if (polymorphed->specialization != NULL) { continue; }

        backlog_entry.cxt.poly_constants  = &polymorphed->constants;
        backlog_entry.cxt.flags          &= ~CHECK_FLAG_DESCENDING;

        buff[0]    = 0;
        lazy_comma = "";
        array_traverse(*backlog_entry.cxt.poly_constants, it) {
            strncat(buff, lazy_comma, sizeof(buff) - strlen(buff) - 1);
            strncat(buff, get_string(it->name), sizeof(buff) - strlen(buff) - 1);
            strncat(buff, " = ", sizeof(buff) - strlen(buff) - 1);
            strncat(buff, get_string(value_to_string_id(it->value, it->type)), sizeof(buff) - strlen(buff) - 1);
            lazy_comma = ", ";
        }

        push_range_breadcrumb(&backlog_entry.range,
                              "in %s (which is polymorphic), where [ %s ] from these arguments:",
                              get_string(backlog_entry.cxt.parent_decl->full_name),
                              buff);

        check_node(backlog_entry.cxt, backlog_entry.node);

        pop_breadcrumb();
    }

    array_free(poly_backlog);

    if (program_entry == NULL) {
        report_simple_err("at least one procedure must be tagged as 'program_entry'");
        return;
    }

    entry_scope = get_subscope_from_node(global_scope, program_entry->val_expr);
    if (entry_scope         == NULL
    ||  entry_scope->parent == NULL
    ||  entry_scope->parent != global_scope) {

        report_range_err(&ASTP(program_entry)->loc,
                         "'program_entry' procedure must be in global scope");
        return;
    }
}

void multiple_entry_error(ast_decl_t *new, ast_decl_t *old) {
    report_range_err_no_exit(&ASTP(new)->loc, "only one procedure can be delcared as 'program_entry'");
    report_range_info(&ASTP(old)->loc, "another 'program_entry' procedure defined here:");
}

int tag_is_string(ast_t *tag_expr, string_id id) {
    if (tag_expr->kind != AST_IDENT) { return 0; }
    return ((ast_ident_t*)tag_expr)->str_rep == id;
}

string_id value_to_string_id(value_t val, u32 type) {
    string_id ret;
    char      buff[128];
    int       ast_kind;

    ret = 0;

    switch (type_kind(type)) {
        case TY_GENERIC_INT:
            switch (type) {
                case TY_U8:
                case TY_U16:
                case TY_U32:
                case TY_U64:
                    snprintf(buff, sizeof(buff), "%"PRIu64, val.u);
                    ret = get_string_id(buff);
                    break;
                case TY_S8:
                case TY_S16:
                case TY_S32:
                case TY_S64:
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
        case TY_MODULE: ast_kind = AST_MODULE; goto ast;
        case TY_PROC:   ast_kind = AST_PROC;   goto ast;
        case TY_STRUCT: ast_kind = AST_STRUCT; goto ast;
ast:;
            snprintf(buff, sizeof(buff), "%s at %p", ast_get_kind_str(ast_kind), val.a);
            ret = get_string_id(buff);
            break;
        default:
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
#define X(kind) #kind,
X_AST
#undef X
};

const char *ast_get_kind_str(int kind) {
    return ast_kind_to_name[kind] + 4;
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

        if (ASTP(decl)->kind      != AST_DECL_VAR
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


static u32 get_polymorph_type(check_context_t cxt, ast_t *node, array_t *polymorphs, array_t *params, poly_arg_t *args, u32 n_args, src_range_t args_loc, u32 *idx_out);
static u32 get_proc_polymorph_type(check_context_t cxt, ast_proc_t *proc, poly_arg_t *args, u32 n_args, src_range_t args_loc, u32 *idx_out);
static u32 get_struct_polymorph_type(check_context_t cxt, ast_struct_t *st, poly_arg_t *args, u32 n_args, src_range_t args_loc, u32 *idx_out);


static void check_specialization(check_context_t cxt, ast_t *tag, ast_t *node, ast_t *arg) {
    ast_decl_t    *decl;
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
    polymorphed_t *polymorphed;

    decl = (ast_decl_t*)node;

    ASSERT(arg->kind == AST_IDENT, "specialization arg not an ident");

    arg_ident = (ast_ident_t*)arg;

    ASSERT(arg_ident->resolved_node != NULL, "ident not resolved");
    ASSERT(ast_kind_is_decl(arg_ident->resolved_node->kind), "ident does not resolve to a declaration");

    arg_decl = (ast_decl_t*)arg_ident->resolved_node;

    if (!(ASTP(arg_decl)->flags & AST_FLAG_POLYMORPH)) {
        report_range_err_no_exit(&arg->loc, "can't make '%s' a specialization of something not polymorphic", get_string(decl->name));
        report_range_info_no_context(&ASTP(arg_decl)->loc, "'%s', which is not polymorphic, is declared here", get_string(arg_decl->full_name));
    }

    if (ASTP(decl)->flags & AST_FLAG_POLYMORPH) {
        report_range_err_no_exit(&ASTP(decl)->loc, "specializations may not be polymorphic");
        report_range_info_no_context(&tag->loc, "'%s' tagged as a specialization of '%s' here", get_string(decl->name), get_string(arg_decl->full_name));
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
                              "when trying to make '%s' a specialization of '%s'",
                              get_string(decl->full_name),
                              get_string(arg_decl->full_name));
        get_proc_polymorph_type(cxt, arg_proc, pargs, n_pargs, proc->params_loc, &idx);
        pop_breadcrumb();

        polymorphed = array_item(arg_proc->polymorphs, idx);

        if (polymorphed->specialization != NULL) {
            report_range_err_no_exit(&tag->loc, "specialization of '%s' with these parameters is already specified", get_string(arg_decl->full_name));
            report_range_info_no_context(&polymorphed->specialization->loc, "previous specialization here:");
        }

        polymorphed->specialization = ASTP(decl);

    } else if (decl->val_expr->kind == AST_STRUCT) {
        ASSERT(0, "unimplemented");
    } else {
        ASSERT(0, "bad node kind in check_specialization");
    }
}

static void check_tag(check_context_t cxt, ast_t *node, ast_t *tag) {
    ast_decl_t         *decl  = NULL;
    ast_proc_t         *proc  = NULL;
    ast_struct_t       *st    = NULL;
    ast_module_t       *mod   = NULL;
    ast_decl_t         *field = NULL;
    ast_ident_t        *ident;
    ast_bin_expr_t     *expr;
    ast_arg_list_t     *arg_list;
    array_t            *args;
    int                 n_args;
    ast_t              *arg;
    int                 i;
    u64                 bits[2];

    if (ast_kind_is_decl(node->kind)) { decl = (ast_decl_t*)node; }

    switch (node->kind) {
        case AST_DECL_VAR:
            break;
        case AST_DECL_PROC:
            proc = (ast_proc_t*)decl->val_expr;
            break;
        case AST_DECL_STRUCT:
            st = (ast_struct_t*)decl->val_expr;
            break;
        case AST_DECL_STRUCT_FIELD:
            field = decl;
            break;
        case AST_DECL_MACRO:
            ASSERT(0, "unimplemented");
            break;
        case AST_DECL_MODULE:
            mod = (ast_module_t*)decl->val_expr;
            break;
        default:
            ASSERT(0, "this kind of node should not have tags");
            break;
    }

    (void)proc;
    (void)st;
    (void)mod;
    (void)field;

    if (tag->kind == AST_IDENT) {
        ident = (ast_ident_t*)tag;

        if (ident->str_rep == PROGRAM_ENTRY_ID) {
            if (proc == NULL) {
                report_range_err(&node->loc, "'%s' is tagged 'program_entry', but is not a procedure", get_string(decl->name));
            }

            if (program_entry != NULL && program_entry != decl) {
                multiple_entry_error(decl, program_entry);
            }

            program_entry = decl;
        } else if (ident->str_rep == EXTERN_ID) {
            if (node->kind != AST_DECL_VAR
            &&  proc == NULL) {
                report_range_err(&node->loc, "'%s' is tagged 'extern', but is not a procedure or a variable", get_string(decl->name));
            }

            node->flags |= AST_FLAG_IS_EXTERN;

            if (proc != NULL) {
                ASTP(proc)->flags |= AST_FLAG_IS_EXTERN;
            }
        } else {
            report_range_err(&tag->loc, "unknown tag '%s'", get_string(ident->str_rep));
        }
    } else if (tag->kind == AST_BIN_EXPR) {
        expr = (ast_bin_expr_t*)tag;
        if (expr->left->kind != AST_IDENT
        ||  expr->op != OP_CALL
        ||  expr->right->kind != AST_ARG_LIST) {
            report_range_err_no_exit(&tag->loc, "invalid tag expression");
            report_simple_info("tags may be of the form `tag` or `tag(arguments)`");
        }

        ident    = (ast_ident_t*)expr->left;
        arg_list = (ast_arg_list_t*)expr->right;

        check_node(cxt, ASTP(arg_list));

        args   = &arg_list->args;
        n_args = array_len(*args);

        if (ident->str_rep == BITFIELD_STRUCT_ID) {
            if (st == NULL) {
                report_range_err(&ASTP(ident)->loc, "tag 'bitfield_struct' only applies to structs");
            }

            if (n_args != 1) {
                report_range_err(&ASTP(arg_list)->loc, "tag 'bitfield_struct' expects exactly 1 argument");
            }

            arg = ((arg_t*)array_item(*args, 0))->expr;

            if (arg->type != TY_TYPE) {
                report_range_err_no_exit(&arg->loc, "tag 'bitfield_struct' argument must have type type");
                report_simple_info("got %s", get_string(get_type_string_id(arg->type)));
            }

            if (type_kind(arg->value.t) != TY_GENERIC_INT) {
                report_range_err_no_exit(&arg->loc, "tag 'bitfield_struct' argument must be an unsigned integer type");
                report_simple_info("have type %s", get_string(get_type_string_id(arg->value.t)));
            }

            switch (arg->value.t) {
                case TY_S8:
                case TY_S16:
                case TY_S32:
                case TY_S64:
                    report_range_err_no_exit(&arg->loc, "tag 'bitfield_struct' argument must be an unsigned integer type");
                    report_simple_err("have type %s", get_string(get_type_string_id(arg->value.t)));
                    break;
                case TY_U8:  st->bitfield_struct_bits = 8;  break;
                case TY_U16: st->bitfield_struct_bits = 16; break;
                case TY_U32: st->bitfield_struct_bits = 32; break;
                case TY_U64: st->bitfield_struct_bits = 64; break;
                default:
                    ASSERT(0, "huh?");
                    break;
            }
        } else if (ident->str_rep == BITFIELD_ID) {
            st = (ast_struct_t*)cxt.parent_decl->val_expr;

            if (!st->bitfield_struct_bits) {
                report_range_err(&tag->loc,
                                 "field '%s' is tagged with 'bitfield', but its parent struct '%s' is not a 'bitfield_struct'",
                                 get_string(field->name),
                                 get_string(cxt.parent_decl->name));
            }

            if (n_args != 2) {
                report_range_err(&ASTP(arg_list)->loc, "tag 'bitfield' expects exactly 2 arguments");
            }

            for (i = 0; i < n_args; i += 1) {
                arg = ((arg_t*)array_item(*args, i))->expr;

                if (arg->kind != AST_INT
                ||  arg->value.s < 0
                ||  arg->value.s >= st->bitfield_struct_bits) {

                    report_range_err_no_exit(&arg->loc, "tag 'bitfield' argument must be a valid bit position");
                    report_simple_info("valid bit positions are 0-%d", st->bitfield_struct_bits - 1);
                }

                bits[i] = arg->value.u;
            }

            if (bits[1] < bits[0]) {
                report_range_err(&arg->loc, "second argument of tag 'bitfield' must be greater than or equal to the first");
            }

            for (i = 0; i < st->bitfield_struct_bits; i += 1) {
                if (i >= bits[0] && i <= bits[1]) {
                    field->bitfield_mask |= 1ULL << i;
                }
            }

            field->bitfield_shift = bits[0];
        } else if (ident->str_rep == SPECIALIZATION_ID) {
            if (n_args != 1) {
                report_range_err(&ASTP(arg_list)->loc, "tag 'specialization' expects exactly 1 argument");
            }

            arg = ((arg_t*)array_item(*args, 0))->expr;

            if (arg->kind != AST_IDENT) {
                report_range_err(&arg->loc, "tag 'specialization' argument must be an identifier");
            }

            check_specialization(cxt, tag, node, arg);
        } else {
            report_range_err(&expr->left->loc, "unknown tag '%s'", get_string(ident->str_rep));
        }
    }
}

static void check_tags(check_context_t cxt, ast_t *node, array_t *tags) {
    ast_t **it;

    array_traverse(*tags, it) {
        check_tag(cxt, node, *it);
    }
}

static void reinstall(ast_t *node, string_id name, scope_t *scope) {
    int        i;
    string_id *it;

    i = 0;
    array_traverse(scope->symbols, it) {
        if (*it == name) {
            *(ast_t**)array_item(scope->nodes, i) = node;
            break;
        }
        i += 1;
    }
}

static void check_for_decl_cycle(ast_t *node) {
    ast_t       **it;
    int           idx;
    ast_ident_t  *ident;
    int           last;

    ASSERT(ast_kind_is_decl(node->kind) || node->kind == AST_PARAM, "bad node kind");

#define CYCLE_NODE_NAME_ID(_node)                 \
    (ast_kind_is_decl((_node)->kind)              \
        ? (((ast_decl_t*)(_node))->scope->in_proc \
            ? ((ast_decl_t*)(_node))->name        \
            : ((ast_decl_t*)(_node))->full_name)  \
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

    if (do_cycle_check) {
        check_for_decl_cycle(ASTP(decl));
        CYCLE_PATH_PUSH(ASTP(decl));
    }

    if (cxt.poly_constants != NULL) { reinstall(ASTP(decl), decl->name, cxt.scope); }

    cxt.parent_decl = decl;

    ASSERT(decl->type_expr || decl->val_expr,
           "decl misssing both type and val");

    if (ASTP(decl)->kind == AST_DECL_VAR) {
        cxt.flags |= CHECK_FLAG_DESCENDING;
        check_tags(cxt, ASTP(decl), &decl->tags);
    }

    if (decl->type_expr != NULL) {
        check_node(cxt, decl->type_expr);

        if (decl->type_expr->type != TY_TYPE) {
            report_range_err_no_exit(&decl->type_expr->loc,
                                     "expression declaring type of '%s' is not a type",
                                     get_string(decl->name));
            report_simple_info("got %s instead of type", get_string(get_type_string_id(decl->type_expr->type)));
        }

        decl_t = decl->type_expr->value.t;
    }

    if (decl->val_expr != NULL) {
        check_node(cxt, decl->val_expr);
        val_t = decl->val_expr->type;

        if (val_t == TY_NOT_TYPED) {
            report_range_err(&decl->val_expr->loc,
                             "initialization expression of '%s' does not produce a value",
                             get_string(decl->name));
        }

        if (ASTP(decl)->flags & AST_FLAG_CONSTANT
        &&  !(decl->val_expr->flags & AST_FLAG_CONSTANT)) {
            if (ASTP(decl)->kind == AST_DECL_VAR) {
                report_range_err_no_exit(&decl->val_expr->loc,
                                        "'%s' is declared as a constant, but has a non-constant initialization",
                                        get_string(decl->name));
                report_fixit(ASTP(decl)->loc.beg,
                            "if you meant for '%s' to be a variable, use this syntax:\a%s :=    OR    %s: <type> =",
                            get_string(decl->name),
                            get_string(decl->name),
                            get_string(decl->name));
            } else {
                report_range_err(&decl->val_expr->loc,
                                "'%s' is declared as a constant, but has a non-constant initialization",
                                get_string(decl->name));
            }
        }

        ASTP(decl)->value = decl->val_expr->value;
    }

    if (decl->type_expr == NULL) {
        decl_t = val_t;
    }

    if (cxt.scope->in_proc && decl_t == TY_PROC) {
        report_range_err(&ASTP(decl)->loc, "procedures may not be defined within another procedure");
        return;
    }

    if (decl->type_expr != NULL
    &&  decl->val_expr  != NULL) {

        if (!types_are_compatible(decl_t, val_t)) {
            report_range_err_no_exit(&decl->val_expr->loc,
                                     "initialization of '%s' does not match declared type of %s",
                                     get_string(decl->name),
                                     get_string(get_type_string_id(decl_t)));
            report_range_info_no_context(&decl->type_expr->loc,
                                         "expected %s, but got %s",
                                         get_string(get_type_string_id(decl_t)),
                                         get_string(get_type_string_id(val_t)));
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

    if (do_cycle_check) {
        CYCLE_PATH_POP();
    }
}

static u32 current_poly_vargs_type(check_context_t cxt) {
    polymorph_constant_t *it;

    if (cxt.poly_constants != NULL) {
        array_rtraverse(*cxt.poly_constants, it) {
            if (it->name == ELLIPSIS_ID) {
                ASSERT(it->type == TY_TYPE, "... should be a type value");
                return it->value.t;
            }
        }
    }

    return TY_UNKNOWN;
}

static void check_proc(check_context_t cxt, ast_proc_t *proc) {
    int       descend;
    scope_t  *new_scope;
    u32       n_params;
    type_t    list_type;
    u32      *param_types;
    int       i;
    ast_t   **it;
    int       j;
    u32       ret_type;
    u32       had_type;

    descend = !(cxt.flags & CHECK_FLAG_DESCENDING);

    if (ASTP(proc)->flags & AST_FLAG_POLYMORPH
    &&  cxt.poly_constants == NULL) {

        descend = 0;
    }

    if (!descend
    &&  cxt.poly_constants == NULL
    &&  proc->ast.type != TY_UNKNOWN) {

        goto out;
    }

    cxt.flags |= CHECK_FLAG_DESCENDING;

    new_scope = proc->scope;

    cxt.proc      = proc;
    cxt.unit_decl = cxt.parent_decl;

    cxt.scope = new_scope;

    n_params = array_len(proc->params);

    if (n_params > 0
    &&  cxt.poly_constants != NULL
    &&  (*(ast_t**)array_last(proc->params))->flags & AST_FLAG_POLY_VARARGS) {

        list_type  = get_type_t(current_poly_vargs_type(cxt));
        n_params  -= 1;
        n_params  += list_type.list_len;
    }

    param_types = alloca(sizeof(u32) * n_params);
    i           = 0;
    array_traverse(proc->params, it) {
        check_node(cxt, *it);
        if (cxt.poly_constants != NULL && (*it)->flags & AST_FLAG_POLY_VARARGS) {
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
        check_node(cxt, proc->ret_type_expr);
        if (proc->ret_type_expr->type != TY_TYPE
        &&  proc->ret_type_expr->type != TY_POLY) {
            report_range_err_no_exit(&proc->ret_type_expr->loc,
                                "expression must be a type since it declares the return type of procedure '%s'",
                                get_string(cxt.parent_decl->name));
            report_simple_info("got %s instead", get_string(get_type_string_id(proc->ret_type_expr->type)));
            return;
        }

        ret_type = proc->ret_type_expr->value.t;
    } else {
        ret_type = TY_NOT_TYPED;
    }

    had_type = ASTP(proc)->type != TY_UNKNOWN;

    ASTP(proc)->type = get_proc_type(n_params, param_types, ret_type);

    /* @bad?, @refactor
    ** We have to bubble this type up to the declaration so that identifier lookups
    ** that occur before we return from this routine can get the right type.
    ** I'm not sure if this should just be done everywhere like this (shouldn't
    ** be _that_ many spots), or if something a little smarter should be done.
    */
    if (cxt.poly_constants == NULL) {
        ASTP(cxt.parent_decl)->type  = ASTP(proc)->type;
        ASTP(cxt.parent_decl)->value = ASTP(proc)->value;
    }

    if (!had_type && ASTP(proc)->flags & AST_FLAG_POLYMORPH) {
        /* Check these here _once_ since we won't descend poly procs. */
        check_tags(cxt, ASTP(cxt.parent_decl), &(cxt.parent_decl)->tags);
    }

    if (!descend) { goto out; }

    check_tags(cxt, ASTP(cxt.parent_decl), &(cxt.parent_decl)->tags);

    if (!(cxt.flags & CHECK_FLAG_POLY_TYPE_ONLY)) {
        if (!(ASTP(proc)->flags & AST_FLAG_POLYMORPH) || cxt.poly_constants != NULL) {
            if (proc->block != NULL) {
                check_node(cxt, proc->block);
            } else {
                ASSERT(ASTP(proc)->flags & AST_FLAG_IS_EXTERN,
                    "proc is not extern, but has no body");
            }
        }
    }

out:;
}

static void check_param(check_context_t cxt, ast_param_t *param) {
    check_for_decl_cycle(ASTP(param));
    CYCLE_PATH_PUSH(ASTP(param));

    if (cxt.poly_constants != NULL) { reinstall(ASTP(param), param->name, cxt.scope); }

    cxt.flags |= CHECK_FLAG_IN_PARAM;

    if (ASTP(param)->flags & AST_FLAG_POLY_VARARGS) {
        if (cxt.poly_constants != NULL) {
            ASTP(param)->type = current_poly_vargs_type(cxt);
        } else {
            ASTP(param)->type = get_vargs_type(TY_POLY);
        }
        goto out;
    }

    check_node(cxt, param->type_expr);

    if (param->val != NULL) {
        check_node(cxt, param->val);
    }

    ASTP(param)->type = param->type_expr->value.t;

    if (ASTP(param)->type == TY_TYPE
    ||  ASTP(param)->flags & AST_FLAG_POLYMORPH) {

        ASTP(param)->flags |= AST_FLAG_CONSTANT;
    }

    if (ASTP(param)->flags & AST_FLAG_VARARGS) {
        ASTP(param)->type = get_vargs_type(ASTP(param)->type);
    }

    if (ASTP(param)->flags & AST_FLAG_POLYMORPH) {
        ASTP(param)->value.t = TY_POLY;
    } else if (param->val != NULL) {
        if (param->val->type != ASTP(param)->type) {
            report_range_err(&param->val->loc,
                             "default value for '%s' has type %s, which does not match declared type of %s",
                             get_string(param->name),
                             get_string(get_type_string_id(param->val->type)),
                             get_string(get_type_string_id(ASTP(param)->type)));
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
        ASTP(integer)->flags   |= AST_FLAG_HEX_INT;
        ASTP(integer)->value.u  = strtoll(s, NULL, 16);
        ASTP(integer)->type     = TY_GENERIC_POSITIVE_INT;
    } else if (s[0] == '-') {
        ASTP(integer)->value.u = strtoll(s + 1, NULL, 10);
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
    ASTP(string)->type     = TY_STR;
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

        ASTP(expr)->flags |= AST_FLAG_CALL_IS_BUILTIN_VARG;
        ASTP(expr)->type   = cxt.varg_ty;
    } else {
        ASSERT(0, "unhandled builtin special");
    }

    array_free(path);
}

static void solve_poly_type_expr(array_t *constants, ast_t *type_expr, poly_arg_t *arg) {
    ast_t                *texpr;
    u32                   at;
    u32                   pt;
    polymorph_constant_t  constant;
/*     ast_t                *child; */

    texpr = type_expr;
    at    = arg->type;

    for (;;) {
        pt = texpr->value.t;

        if (ast_kind_is_leaf_expr(texpr->kind)) {
            ASSERT(pt == TY_POLY, "type expression in polymorphic type pattern does not terminate in a polymorphic parameter");
            ASSERT(texpr->kind == AST_IDENT, "not an ident");

            /* Union memcmp() things... */
            memset(&constant, 0, sizeof(constant));

            constant.name    = ((ast_ident_t*)texpr)->str_rep;
            constant.value.t = at;
            constant.type    = TY_TYPE;

            array_push(*constants, constant);
/*             report_range_info_no_context_no_exit( */
/*                 &texpr->loc, "CASE 3: %s resolved to %s", */
/*                 get_string(constant.name), */
/*                 get_string(value_to_string_id(constant.value, constant.type))); */

            break;
        }


#if 0
        if (texpr->kind == AST_BIN_EXPR && ((ast_bin_expr_t*)texpr)->op == OP_CALL) {
            /* The parameter itself describes a polymorphic thing. We must recurse. */
            child = ((ast_bin_expr_t*)texpr)->left;

            /* @bad @todo what about procedures? Is that even legal?  */
            if (type_kind(at) != TY_STRUCT_MONO
            ||  child->kind != AST_IDENT
            ||  !(child->flags & AST_FLAG_POLYMORPH)) {

                goto err;
            }

            /* Union memcmp() things... */
            memset(&constant, 0, sizeof(constant));

            constant.name    = ((ast_ident_t*)child)->str_rep;
            constant.value.t = struct_mono_type_to_poly(at);
            constant.type    = TY_TYPE;
            array_push(*constants, constant);

            /* There should be no more work to do. */
            return;
        } else {
#endif
            if (type_kind(at) != type_kind(pt)) { goto err; }

            switch (type_kind(pt)) {
                case TY_PTR:
                    texpr = ((ast_unary_expr_t*)texpr)->child;
                    at    = get_under_type(at);
                    break;
                default:
                    if (at != pt) {
err:;
                        report_range_err_no_exit(&arg->node->loc,
                                                "incorrect argument type: expected %s, but got %s",
                                                get_string(get_type_string_id(type_expr->value.t)),
                                                get_string(get_type_string_id(arg->type)));
                        report_range_info_no_context(&type_expr->loc,
                                                     "when solving for polymorphic parameters in a type pattern");
                        return;
                    }
            }
#if 0
        }
#endif
    }
}

static void verify_polymorphic_args(array_t *params, poly_arg_t *args, u32 n_args, src_range_t params_loc, src_range_t args_loc) {
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
        if (!(ASTP(param)->flags & AST_FLAG_POLYMORPH)) {
            continue;
        }

        if (!(param->type_expr->flags & AST_FLAG_POLYMORPH)
        &&  !types_are_compatible(ASTP(param)->type, arg->type)) {

            report_range_err_no_exit(arg->node->kind == AST_PARAM ? &(((ast_param_t*)arg->node)->type_expr->loc) : &arg->node->loc,
                                     "incorrect argument type: expected %s, but got %s",
                                     get_string(get_type_string_id(ASTP(param)->type)),
                                     get_string(get_type_string_id(arg->type)));
            report_range_info_no_context(&param->type_expr->loc,
                                    	 "polymorphic parameter '%s' delcared as type %s here:",
                                         get_string(param->name),
                                         get_string(get_type_string_id(ASTP(param)->type)));

        }

#if 0 /* I don't remember why we had this code... */
        if (is_poly_by_value_param && !arg->has_value) {
            if (arg->node->kind == AST_PARAM) {
                loc.beg = arg->node->loc.beg;
                loc.end = ((ast_param_t*)arg->node)->type_expr->loc.end;
                report_range_err_no_exit(&loc, "missing value for polymorph-by-value parameter");
                report_fixit_no_exit(loc.end, "add a value to satisfy the parameter\a = <value>");
            } else {
                report_range_err_no_exit(&arg->node->loc, "missing value for polymorph-by-value parameter");
            }
            report_range_info_no_context(&ASTP(param)->loc, "polymorph-by-value parameter declared here:");
        }
#endif

        i += 1;
    }
}

static void extract_polymorph_constants_from_arg(array_t *constants, int i, ast_param_t *param, poly_arg_t *args, u32 n_args) {
    u32                    n_vargs;
    u32                   *varg_types;
    int                    j;
    u32                    varg_list_ty;
    poly_arg_t            *arg;
    polymorph_constant_t   constant;

    /* Union memcmp() things... */
    memset(&constant, 0, sizeof(constant));

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

        constant.name    = ELLIPSIS_ID;
        constant.value.t = varg_list_ty;
        constant.type    = TY_TYPE;

        array_push(*constants, constant);
/*         report_range_info_no_context_no_exit( */
/*             &ASTP(param)->loc, "CASE 3: %s resolved to %s", */
/*             get_string(constant.name), */
/*             get_string(value_to_string_id(constant.value, constant.type))); */
    } else if (ASTP(param)->flags & AST_FLAG_POLYMORPH) {

        arg = args + i;

        if (TYPE_IS_GENERIC(arg->type)) {
            force_generic_realization(arg->node);
            arg->type = arg->node->type;
        }

        constant.name  = param->name;
        constant.value = arg->value;
        constant.type  = arg->type;

        array_push(*constants, constant);
/*         report_range_info_no_context_no_exit( */
/*             &ASTP(param)->loc, "CASE 1: %s resolved to %s", */
/*             get_string(constant.name), */
/*             get_string(value_to_string_id(constant.value, constant.type))); */
    }

    if (!(ASTP(param)->flags    & AST_FLAG_POLY_VARARGS)
    &&  param->type_expr->flags & AST_FLAG_POLYMORPH) {

        arg = args + i;
        solve_poly_type_expr(constants, param->type_expr, arg);
    }

}

static array_t get_polymorph_constants(array_t *params, poly_arg_t *args, u32 n_args, src_range_t params_loc, src_range_t args_loc) {
    array_t       constants;
    int           i;
    ast_t       **it;
    ast_param_t  *param;

/*     report_range_info_no_context_no_exit(&ASTP(arg_list)->loc, */
/*                                          "====================== POLYMORPH ======================"); */

    verify_polymorphic_args(params, args, n_args, params_loc, args_loc);


    constants = array_make(polymorph_constant_t);

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

static u32 get_already_polymorphed_type(array_t *polymorphs, array_t *constants, u32 *idx_out) {
    u32            idx;
    polymorphed_t *it;

    idx = 0;
    array_traverse(*polymorphs, it) {
        if (array_len(it->constants) != array_len(*constants)) { goto next; }

        if (memcmp(array_data(it->constants),
                   array_data(*constants),
                   array_len(it->constants) * sizeof(polymorph_constant_t)) == 0) {

            if (idx_out != NULL) {
                *idx_out = idx;
            }

            return it->type;
        }
next:;
        idx += 1;
    }

    return TY_NONE;
}

static u32 get_polymorph_type(check_context_t cxt, ast_t *node, array_t *polymorphs, array_t *params, poly_arg_t *args, u32 n_args, src_range_t args_loc, u32 *idx_out) {
    src_range_t           params_loc;
    array_t               constants;
    polymorphed_t         polymorphed;
    polymorphed_t        *polymorphed_p;
    poly_backlog_entry_t  backlog_entry;

    ASSERT(!(node->flags & AST_FLAG_IS_COPY), "polymorph should not be a copy");

    params_loc       = node->kind == AST_PROC ? ((ast_proc_t*)node)->params_loc : ((ast_struct_t*)node)->params_loc;
    constants        = get_polymorph_constants(params, args, n_args, params_loc, args_loc);
    polymorphed.type = get_already_polymorphed_type(polymorphs, &constants, idx_out);

    if (polymorphed.type == TY_NONE) {
        polymorphed.constants      = constants;
        polymorphed.node           = copy_tree(node);
        polymorphed.specialization = NULL;
        polymorphed_p              = array_push(*polymorphs, polymorphed);

        cxt.scope = cxt.parent_decl->scope;
        ASSERT(cxt.scope != NULL, "did not get scope");

        if (node->kind == AST_PROC) {
            cxt.proc = (ast_proc_t*)node;
        }
        cxt.poly_constants_idx  = array_len(*polymorphs) - 1;
        cxt.poly_constants      = &constants;
        cxt.flags              |= CHECK_FLAG_POLY_TYPE_ONLY;
        check_node(cxt, polymorphed.node);

        if (idx_out != NULL) {
            *idx_out = cxt.poly_constants_idx;
        }

        if (node->kind == AST_PROC) {
            polymorphed.type = polymorphed_p->type = polymorphed.node->type;
        } else {
            polymorphed.type = polymorphed_p->type = polymorphed.node->value.t;
        }

        cxt.flags &= ~(CHECK_FLAG_POLY_TYPE_ONLY |
                       CHECK_FLAG_IN_DEFER       |
                       CHECK_FLAG_IN_LOOP        |
                       CHECK_FLAG_IN_PARAM       |
                       CHECK_FLAG_IN_VARGS);

        backlog_entry.node       = polymorphed.node;
        backlog_entry.polymorphs = polymorphs;
        backlog_entry.cxt        = cxt;
        backlog_entry.range      = args_loc;
        array_push(poly_backlog, backlog_entry);
    } else {
/*         report_simple_info_no_exit("polymorph already checked!"); */
        array_free(constants);
    }

    return polymorphed.type;
}

static u32 get_proc_polymorph_type(check_context_t cxt, ast_proc_t *proc, poly_arg_t *args, u32 n_args, src_range_t args_loc, u32 *idx_out) {
    return get_polymorph_type(cxt, ASTP(proc), &proc->polymorphs, &proc->params, args, n_args, args_loc, idx_out);
}

static u32 get_struct_polymorph_type(check_context_t cxt, ast_struct_t *st, poly_arg_t *args, u32 n_args, src_range_t args_loc, u32 *idx_out) {
    return get_polymorph_type(cxt, ASTP(st), &st->polymorphs, &st->params, args, n_args, args_loc, idx_out);
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
            report_range_err_no_exit(&ASTP(expr)->loc,
                                     "attempting to pass arguments to something that does not have parameters");
            report_range_info_no_context_no_exit(&expr->left->loc,
                                                 "expression has type %s, and value %s",
                                                 get_string(get_type_string_id(proc_ty)),
                                                 get_string(get_type_string_id(expr->left->value.t)));
            report_range_info_no_context(&ASTP(struct_decl)->loc,
                                         "%s is not polymorphic, so it does not take arguments",
                                         get_string(struct_decl->full_name));
        }

        cxt.parent_decl = struct_decl;

        poly_args = alloca(sizeof(*poly_args) * n_args);
        for (i = 0; i < n_args; i += 1) {
            parg        = poly_args + i;
            parg->value = ((arg_t*)array_item(arg_list->args, i))->expr->value;
            parg->type  = ((arg_t*)array_item(arg_list->args, i))->expr->type;
            parg->node  = ((arg_t*)array_item(arg_list->args, i))->expr;
        }

        ASTP(expr)->type    = TY_TYPE;
        ASTP(expr)->value.t = get_struct_polymorph_type(cxt, (ast_struct_t*)struct_decl->val_expr, poly_args, n_args, ASTP(arg_list)->loc, &idx);

        if (left_ident != NULL) {
            left_ident->poly_idx = idx;
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
        report_range_err_no_exit(&ASTP(expr)->loc,
                                 "attempting to pass arguments to something that does not have parameters");
        report_range_info_no_context(&expr->left->loc,
                                     "expression has type %s",
                                     get_string(get_type_string_id(proc_ty)));
        return;
    }

    proc_origin = NULL;
    proc        = NULL;
    if (left_ident != NULL) {
        proc_origin = try_get_decl_and_path(left_ident, &path);
        if (proc_origin != NULL) {
            proc = (ast_proc_t*)((ast_decl_t*)proc_origin)->val_expr;
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
    && ASTP(proc)->flags & AST_FLAG_POLYMORPH) {
        cxt.parent_decl  = (ast_decl_t*)proc_origin;

        poly_args = alloca(sizeof(*poly_args) * n_args);
        for (i = 0; i < n_args; i += 1) {
            parg        = poly_args + i;
            parg->value = ((arg_t*)array_item(arg_list->args, i))->expr->value;
            parg->type  = ((arg_t*)array_item(arg_list->args, i))->expr->type;
            parg->node  = ((arg_t*)array_item(arg_list->args, i))->expr;
        }

        poly_proc_ty     = get_proc_polymorph_type(cxt, proc, poly_args, n_args, ASTP(arg_list)->loc, &idx);
        expr->left->type = poly_proc_ty;
        proc_ty          = poly_proc_ty;

        if (left_ident != NULL) {
            left_ident->poly_idx = idx;
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
            }

            return;
        }

        if (TYPE_IS_GENERIC(arg_type)) {
            realize_generic(param_type, arg_p->expr);
        }

        if (arg_p->name != STRING_ID_NULL) {
            if (left_ident == NULL) {
                report_range_err(&arg_p->expr->loc, "named arguments are not allowed in indirect calls");
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

                parm_decl = *(ast_t**)array_item(proc->params, i);

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

    if (varg_ty != TY_NONE) {
        for (i = n_params; i < n_args; i += 1) {
            arg_p    = array_item(arg_list->args, i);
            arg_type = arg_p->expr->type;

            if (!types_are_compatible(varg_ty, arg_type) && !type_is_poly(varg_ty)) {
                report_range_err_no_exit(&arg_p->expr->loc,
                                         "incorrect argument type: expected %s, but got %s",
                                         get_string(get_type_string_id(varg_ty)),
                                         get_string(get_type_string_id(arg_type)));
                report_simple_info_no_exit("argument belongs to a variadic parameter list");

                if (left_ident == NULL) {
                    report_simple_info("indirect call with procedure type %s",
                                       get_string(get_type_string_id(proc_ty)));
                } else {
                    if (proc_origin->kind == AST_BUILTIN) {
                        if (left_ident->resolved_node == proc_origin) {
                            report_simple_info("'%s' is a compiler builtin",
                                               get_string(((ast_builtin_t*)proc_origin)->name));
                        } else {
                            report_declaration_path(path);
                        }
                        return;
                    }

                    parm_decl = *(ast_t**)array_last(proc->params);

                    if (left_ident->resolved_node == proc_origin) {
                        report_range_info_no_context(&parm_decl->loc, "variadic parameter list declared here:");
                    } else if (left_ident->resolved_node != proc_origin) {
                        report_range_info_no_context_no_exit(&parm_decl->loc, "variadic parameter list declared here:");
                        report_declaration_path(path);
                    }
                }
            }

            if (arg_p->name != STRING_ID_NULL) {
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
                                report_range_info_no_context(&parm_decl->loc, "parameter declared here:");
                            } else {
                                report_range_info_no_context_no_exit(&parm_decl->loc, "parameter declared here:");
                                report_declaration_path(path);
                            }
                        }
                    }
                } else {
                    report_range_err_no_exit(&arg_p->expr->loc, "only the first argument for a variadic parameter list may be named");
                    if (left_ident->resolved_node == proc_origin) {
                        report_range_info_no_context(&parm_decl->loc, "parameter declared here:");
                    } else {
                        report_range_info_no_context_no_exit(&parm_decl->loc, "parameter declared here:");
                        report_declaration_path(path);
                    }
                    return;
                }
            }
        }
    }

    if (proc != NULL) {
        array_free(path);
    }

    ASTP(expr)->type = get_ret_type(expr->left->type);
}

static void check_ident(check_context_t cxt, ast_ident_t *ident) {
    scope_t              *resolved_node_scope;
    polymorph_constant_t *it;

    if (ASTP(ident)->flags & AST_FLAG_POLYMORPH
    &&  cxt.poly_constants != NULL) {

        reinstall(ASTP(ident), ident->str_rep, cxt.scope);
    }

    if (ident->str_rep == UNDERSCORE_ID) {
        report_range_err_no_exit(&ASTP(ident)->loc, "'_' can be assigned to, but not referenced");
        report_simple_info("'_' acts as an assignment sink for values meant to be unreferenceable");
        return;
    }

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
    }

    if (cxt.poly_constants != NULL) {
        array_traverse(*(cxt.poly_constants), it) {
            if (it->name == ident->str_rep) {
                ASSERT(ASTP(ident)->flags & AST_FLAG_IS_COPY, "must be a copy");
                ASTP(ident)->type   = it->type;
                ASTP(ident)->value  = it->value;
                ASTP(ident)->flags |= AST_FLAG_CONSTANT;
                return;
            }
        }
    }

    if (ASTP(ident)->flags & AST_FLAG_POLYMORPH) {
        ASTP(ident)->type     = TY_POLY;
        ASTP(ident)->value.t  = TY_POLY;
        ASTP(ident)->flags   |= AST_FLAG_CONSTANT;
    } else {
        if (ident->resolved_node->type == TY_UNKNOWN) {
            if (ident->resolved_node == ASTP(cxt.parent_decl)) {
                CYCLE_PATH_PUSH(ASTP(ident));
                check_for_decl_cycle(ASTP(cxt.parent_decl));
                CYCLE_PATH_POP();
                return;
            }

            cxt.scope          = resolved_node_scope;
            cxt.poly_constants = NULL;
            CYCLE_PATH_PUSH(ASTP(ident));
            check_node(cxt, ident->resolved_node);
            CYCLE_PATH_POP();
        }

        ASTP(ident)->type  = ident->resolved_node->type;
        ASTP(ident)->value = ident->resolved_node->value;
    }
}

static void check_namespace_dot(check_context_t cxt, ast_bin_expr_t *expr) {
    /* @note:
    ** expr->left must have been checked by this point.
    */

    ast_module_t *mod;
    ast_decl_t   *resolved_decl;
    const char   *s_kind;
    scope_t      *containing_scope;
    ast_ident_t  *right_ident;
    ast_t        *found_node;
    const char   *resolved_name;
    ast_ident_t  *new_ident;
    const char   *lname;
    u32           llen;
    const char   *rname;
    u32           rlen;
    u32           new_name_len;
    char         *new_name_buff;


    if (expr->left->type == TY_MODULE) {
        mod = (ast_module_t*)expr->left->value.a;
        ASSERT(mod != NULL, "missing module value");
        resolved_decl = mod->parent_decl;
        s_kind        = "module";
    } else if (expr->left->type == TY_TYPE) {
        resolved_decl = struct_type_to_decl(expr->left->value.t);
        s_kind        = "struct";
    } else {
        ASSERT(0, "not a type I can handle in check_namespace_dot()");
        return;
    }

    ASSERT(resolved_decl != NULL, "did not resolve LHS to a decl");
    ASSERT(   resolved_decl->val_expr->kind == AST_MODULE
           || resolved_decl->val_expr->kind == AST_STRUCT,
           "resolved_decl is not a module or struct declaration");

    containing_scope = get_subscope_from_node(resolved_decl->scope,
                                              resolved_decl->val_expr);

    ASSERT(containing_scope != NULL, "did not find scope");

    right_ident = (ast_ident_t*)expr->right;

    found_node    = find_in_scope(containing_scope, right_ident->str_rep);
    resolved_name = get_string(resolved_decl->full_name);

    if (found_node == NULL) {
        report_range_err(&expr->right->loc,
                         "nothing named '%s' in %s '%s'",
                         get_string(right_ident->str_rep),
                         s_kind,
                         resolved_name);
        return;
    }

    if (found_node->kind == AST_DECL_STRUCT_FIELD) {
        ASSERT(expr->left->type == TY_TYPE, "found a field in a non-struct???");
        report_range_err(&ASTP(expr)->loc,
                         "struct '%s' has a field called '%s', "
                         "but it must be accessed from an instance of type '%s', rather than the type itself",
                         resolved_name,
                         get_string(right_ident->str_rep),
                         get_string(resolved_decl->name));
        return;
    }

    cxt.scope          = containing_scope;
    cxt.poly_constants = NULL;

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

    /* @note -- are there any other flags we need to set/clear? */
    ASTP(new_ident)->flags |= AST_FLAG_CONSTANT;

    lname         = resolved_name;
    llen          = strlen(lname);
    rname         = get_string(right_ident->str_rep);
    rlen          = strlen(rname);
    new_name_len  = llen + 1 + rlen;
    new_name_buff = alloca(new_name_len);
    memcpy(new_name_buff, lname, llen);
    new_name_buff[llen] = '.';
    memcpy(new_name_buff + llen + 1, rname, rlen);

    new_ident->str_rep       = get_string_id_n(new_name_buff, new_name_len);
    new_ident->resolved_node = found_node;
    new_ident->poly_idx      = -1;
    new_ident->varg_idx      = -1;

    CYCLE_PATH_PUSH(ASTP(new_ident));
    check_node(cxt, found_node);
    CYCLE_PATH_POP();
}

static void check_dot(check_context_t cxt, ast_bin_expr_t *expr) {
    u32           st_ty;
    u32           field_ty;
    ast_decl_t   *st_decl;
    ast_struct_t *st;

    if (expr->right->kind != AST_IDENT) {
        report_range_err(&expr->right->loc,
                         "the '.' operator must be followed by an identifier");
        return;
    }

    switch (type_kind(expr->left->type)) {
        case TY_MODULE:
        case TY_TYPE:
            check_namespace_dot(cxt, expr);
            break;
        case TY_STRUCT:
        case TY_STRUCT_MONO:
            st_ty = expr->left->type;
            field_ty = get_struct_field_type(st_ty, ((ast_ident_t*)expr->right)->str_rep);
            if (field_ty == TY_UNKNOWN) {
                report_range_err(&expr->right->loc,
                                 "type %s does not have a field named '%s'",
                                 get_string(get_type_string_id(st_ty)),
                                 get_string(((ast_ident_t*)expr->right)->str_rep));
            }
            ASTP(expr)->type = field_ty;

            st_decl = struct_type_to_decl(st_ty);
            ASSERT(st_decl != NULL, "did not get struct decl");

            st = (ast_struct_t*)st_decl->val_expr;

            if (st->bitfield_struct_bits) { ASTP(expr)->flags |= AST_FLAG_BITFIELD_DOT; }

            break;
        case TY_PTR:
            st_ty = get_under_type(expr->left->type);
            if (type_kind(st_ty) != TY_STRUCT) { goto does_not_apply; }

            field_ty = get_struct_field_type(st_ty, ((ast_ident_t*)expr->right)->str_rep);
            if (field_ty == TY_UNKNOWN) {
                report_range_err(&expr->right->loc,
                                 "type %s does not have a field named '%s'",
                                 get_string(get_type_string_id(st_ty)),
                                 get_string(((ast_ident_t*)expr->right)->str_rep));
            }
            ASTP(expr)->type = field_ty;
            break;
        does_not_apply:;
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
        case TKINDPAIR_FLT_FLT:
            /* @todo check for width incompat */
            /* @todo How to handle this case? Do we promote the type? */
            ASTP(expr)->type = t1;
            break;
        case TKINDPAIR_FLT_INT:
            ASTP(expr)->type = tk1 == TY_GENERIC_FLOAT
                                ? t1
                                : t2;
            break;
        case TKINDPAIR_PTR_INT:
            ASTP(expr)->type = (tk1 == TY_PTR ? t1 : t2);
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
        case TKINDPAIR_PTR_PTR:
            ASTP(expr)->type = TY_INT_PTR;
            break;
        case TKINDPAIR_PTR_INT:
            if (tk1 != TY_PTR) { goto bad; }
            ASTP(expr)->type = t1;
            break;
        bad:;
        default:
            binop_bad_type_error(expr);
            break;
    }

    if (tk1 == TY_GENERIC_INT && tk2 == TY_GENERIC_INT) {
    } else if (tk1 == TY_PTR && tk2 == TY_GENERIC_INT) {
        ASTP(expr)->type = t1;
    } else {
        return;
    }
}

static void check_mul(check_context_t cxt, ast_bin_expr_t *expr) {
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

static void check_div(check_context_t cxt, ast_bin_expr_t *expr) {
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

static void check_mod(check_context_t cxt, ast_bin_expr_t *expr) {
    u32 t1;
    u32 t2;
    u32 tk1;
    u32 tk2;

    t1  = expr->left->type;
    t2  = expr->right->type;
    tk1 = type_kind(t1);
    tk2 = type_kind(t2);

    if (!((tk1 == TY_GENERIC_INT && tk2 == TY_GENERIC_INT))) {

        binop_bad_type_error(expr);
        return;
    }

    /* @todo check for width incompat */
    /* @todo How to handle this case? Do we promote the type? */
    ASTP(expr)->type = t1;
}

static void check_cmp(check_context_t cxt, ast_bin_expr_t *expr) {
    u32 t1;
    u32 t2;
    u32 tk1;
    u32 tk2;

    t1  = expr->left->type;
    t2  = expr->right->type;
    tk1 = type_kind(t1);
    tk2 = type_kind(t2);

    if (!((tk1 == TY_GENERIC_INT && tk2 == TY_GENERIC_INT) || (tk1 == TY_PTR && tk2 == TY_PTR))
    ||  t1 != t2) {

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

    ASTP(expr)->type = TY_U64;
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

static void check_expr_unsatisfied_poly(check_context_t cxt, ast_t *expr) {
    if (!(cxt.flags & CHECK_FLAG_IN_PARAM)
    &&  expr->type == TY_TYPE
    &&  expr->value.t != TY_POLY
    &&  type_is_poly(expr->value.t)) {

        report_range_err_no_exit(&expr->loc,
                                 "this use of polymorphic type %s requires arguments to satisfy its polymorphic parameters",
                                 get_string(get_type_string_id(expr->value.t)));
        report_fixit(expr->loc.end, "provide arguments for polymorphic parameters\a(...)", 123, 456);
    }
}

static void check_bin_expr(check_context_t cxt, ast_bin_expr_t *expr) {
    u32 tkl;
    u32 tkr;

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

    if (!TYPE_IS_GENERIC(expr->left->type)
    &&  TYPE_IS_GENERIC(expr->right->type)) {

        realize_generic(expr->left->type, expr->right);
    } else if (TYPE_IS_GENERIC(expr->left->type)
           &&  !TYPE_IS_GENERIC(expr->right->type)) {

        realize_generic(expr->right->type, expr->left);
    }

    tkl = type_kind(expr->left->type);
    tkr = type_kind(expr->right->type);

    switch (expr->op) {
        case OP_DOT: /* Handled above. */ break;

        case OP_CALL:
            check_call(cxt, expr);
            break;

        case OP_SUBSCRIPT:
            if (tkl == TY_PTR) {
                ASTP(expr)->type = get_under_type(expr->left->type);
            } else if (tkl == TY_STR) {
                ASTP(expr)->type = TY_U8;
            } else {
                report_range_err_no_exit(&ASTP(expr)->loc,
                                         "left-hand-side operand of '[]' operator must be str or a pointer type");
                report_range_info_no_context(&expr->left->loc,
                                             "operand has type %s",
                                             get_string(get_type_string_id(expr->left->type)));
                return;
            }

            if (!type_kind_is_int(tkr)) {
                report_range_err_no_exit(&ASTP(expr)->loc,
                                         "right-hand-side operand of '[]' operator must be an integer type");
                report_range_info_no_context(&expr->right->loc,
                                         "operand has type %s",
                                         get_string(get_type_string_id(expr->right->type)));
            }

            break;

        case OP_PLUS:
            check_add(cxt, expr);
            ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);
            break;
        case OP_MINUS:
            check_sub(cxt, expr);
            ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);
            break;
        case OP_MULT:
            check_mul(cxt, expr);
            ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);
            break;
        case OP_DIV:
            check_div(cxt, expr);
            ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);
            break;
        case OP_MOD:
            check_mod(cxt, expr);
            ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);
            break;

        case OP_EQU:
        case OP_NEQ:
        case OP_LSS:
        case OP_LEQ:
        case OP_GTR:
        case OP_GEQ:
            check_cmp(cxt, expr);
            break;

        case OP_PLUS_ASSIGN:
            check_add(cxt, expr);
            ASSERT(ASTP(expr)->type == expr->left->type, "type should not change in assignment");
            break;
        case OP_MINUS_ASSIGN:
            check_sub(cxt, expr);
            ASSERT(ASTP(expr)->type == expr->left->type, "type should not change in assignment");
            break;
        case OP_MULT_ASSIGN:
            check_mul(cxt, expr);
            ASSERT(ASTP(expr)->type == expr->left->type, "type should not change in assignment");
            break;
        case OP_DIV_ASSIGN:
            check_div(cxt, expr);
            ASSERT(ASTP(expr)->type == expr->left->type, "type should not change in assignment");
            break;
        case OP_MOD_ASSIGN:
            check_mod(cxt, expr);
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

        case OP_AND:
        case OP_OR:
            check_logical(cxt, expr);
            ASTP(expr)->flags |= BIN_EXPR_CONST(expr->left, expr->right);
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
        case OP_ARRAY:
            if (expr->child->type == TY_TYPE || expr->child->type == TY_POLY) {
                ASTP(expr)->type    = TY_TYPE;
                ASTP(expr)->value.t = get_ptr_type(expr->child->value.t);

                ASSERT(expr->child->flags & AST_FLAG_CONSTANT, "type not constant");
                ASTP(expr)->flags |= AST_FLAG_CONSTANT;
            } else {
                /* @todo -- check if rhs is addressable */
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
            ASTP(expr)->type   = expr->child->type;
            ASTP(expr)->flags |= expr->child->flags & AST_FLAG_CONSTANT;
            break;
        case OP_NEG:
            if (!type_kind_is_numeric(type_kind(expr->child->type))) {
                report_range_err_no_exit(&ASTP(expr)->loc,
                                         "right-hand-side operand of '-' operator must be numeric");
                report_range_info_no_context(&expr->child->loc,
                                             "operand has type %s",
                                             get_string(get_type_string_id(expr->child->type)));
            }
            ASTP(expr)->type   = expr->child->type;
            ASTP(expr)->flags |= expr->child->flags & AST_FLAG_CONSTANT;
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

#if 0
static void check_struct_field(check_context_t cxt, ast_struct_field_t *field) {
    ast_struct_t *st;

    check_tags(cxt, ASTP(field), &field->tags);

    st = (ast_struct_t*)cxt.parent_decl->val_expr;

    if (st->bitfield_struct_bits && !field->bitfield_mask) {
        report_range_err_no_exit(&ASTP(field)->loc,
                                 "parent struct '%s' is a bitfield_struct, but field '%s' is not tagged with 'bitfield'",
                                 get_string(cxt.parent_decl->name),
                                 get_string(field->name));
        report_fixit(ASTP(field)->loc.beg, "add the 'bitfield' tag\a[[ bitfield(start_bit, end_bit) ]]");
    }

    check_node(cxt, field->type_expr);

    ASTP(field)->type = field->type_expr->value.t;
}
#endif

static void check_struct(check_context_t cxt, ast_struct_t *st) {
    scope_t  *new_scope;
    u32       n_params;
    u32      *param_types;
    int       i;
    ast_t   **it;

    if (cxt.poly_constants == NULL
    &&  ASTP(st)->type != TY_UNKNOWN) {

        return;
    }

    new_scope = st->scope;

    cxt.unit_decl = cxt.parent_decl;
    cxt.scope     = new_scope;

    n_params    = array_len(st->params);
    param_types = alloca(sizeof(u32) * n_params);
    i           = 0;
    array_traverse(st->params, it) {
        check_node(cxt, *it);
        param_types[i] = (*it)->type;
        i += 1;
    }

    ASTP(st)->type = TY_TYPE;

    check_tags(cxt, ASTP(cxt.parent_decl), &(cxt.parent_decl)->tags);

    array_traverse(st->fields, it) {
        check_node(cxt, *it);
    }

    if (!(cxt.flags & CHECK_FLAG_POLY_TYPE_ONLY)) {
        if (!(ASTP(st)->flags & AST_FLAG_POLYMORPH) || cxt.poly_constants != NULL) {
            array_traverse(st->children, it) {
                check_node(cxt, *it);
            }
        }
    }

    if (cxt.poly_constants == NULL) {
        ASTP(st)->value.t = get_struct_type(cxt.parent_decl);
    } else {
        ASTP(st)->value.t = get_struct_mono_type(cxt.parent_decl, cxt.poly_constants_idx);
    }


    /* @bad?, @refactor
    ** We have to bubble this type up to the declaration so that identifier lookups
    ** that occur before we return from this routine can get the right type.
    ** I'm not sure if this should just be done everywhere like this (shouldn't
    ** be _that_ many spots), or if something a little smarter should be done.
    */
    if (cxt.poly_constants == NULL) {
        ASTP(cxt.parent_decl)->type  = ASTP(st)->type;
        ASTP(cxt.parent_decl)->value = ASTP(st)->value;
    }
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
    }

again:;
    i = 0;
    array_traverse(arg_list->args, arg) {
        if (type_kind(arg->expr->type) == _TY_TYPE_LIST) {
            array_delete(arg_list->args, i);

            list_type = get_type_t(arg->expr->type);

            for (j = 0; j < list_type.list_len; j += 1) {
                new_arg.name = arg->name;
                new_arg.expr = copy_tree(arg->expr);

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

    ASTP(_if)->type = TY_NOT_TYPED;

    new_scope = _if->scope;

    orig_scope = cxt.scope;
    cxt.scope  = new_scope;

    check_node(cxt, _if->expr);

    if (!type_kind_is_int(type_kind(_if->expr->type))) {
        report_range_err_no_exit(&_if->expr->loc, "'if' condition must have an integer type");
        report_simple_info("got %s", get_string(get_type_string_id(_if->expr->type)));
        return;
    }

    check_node(cxt, _if->then_block);

    if (_if->els != NULL) {
        cxt.scope = orig_scope;
        check_node(cxt, _if->els);
    }
}

static void check_loop(check_context_t cxt, ast_loop_t *loop) {
    scope_t *new_scope;

    cxt.flags |= CHECK_FLAG_IN_LOOP;

    ASTP(loop)->type = TY_NOT_TYPED;

    new_scope = loop->scope;

    cxt.scope = new_scope;

    if (loop->init != NULL) {
        check_node(cxt, loop->init);
    }
    if (loop->cond != NULL) {
        check_node(cxt, loop->cond);
        if (!type_kind_is_int(type_kind(loop->cond->type))) {
            report_range_err_no_exit(&loop->cond->loc, "loop condition must have an integer type");
            report_simple_info("got %s", get_string(get_type_string_id(loop->cond->type)));
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
        report_range_err(&ASTP(brk)->loc, "break statement only valid within a loop");
        return;
    }
}

static void check_continue(check_context_t cxt, ast_continue_t *cont) {
    ASTP(cont)->type = TY_NOT_TYPED;

    if (!(cxt.flags & CHECK_FLAG_IN_LOOP)) {
        report_range_err(&ASTP(cont)->loc, "continue statement only valid within a loop");
        return;
    }
}

static void check_return(check_context_t cxt, ast_return_t *ret) {
    if (cxt.proc == NULL) {
        report_range_err(&ASTP(ret)->loc, "return statement only valid in a procedure body");
        return;
    }

/*     if (cxt.flags & CHECK_FLAG_IN_DEFER) { */
/*         report_range_err(&ASTP(ret)->loc, "return statements are not valid in a defer block"); */
/*         return; */
/*     } */

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
            report_range_err(&ret->expr->loc,
                             "incorrect type of returned expression: expected %s, but got %s",
                             get_string(get_type_string_id(cxt.proc->ret_type_expr->value.t)),
                             get_string(get_type_string_id(ret->expr->type)));
            return;
        }
    } else if (cxt.proc->ret_type_expr != NULL) {
        report_range_err(&ASTP(ret)->loc,
                         "return statement missing expression in a procedure that returns %s", get_string(get_type_string_id(cxt.proc->ret_type_expr->value.t)));
        return;
    }

    ASTP(ret)->type = TY_NOT_TYPED;
}

static void check_defer(check_context_t cxt, ast_defer_t *defer) {
    ASTP(defer)->type = TY_NOT_TYPED;

    cxt.flags |= CHECK_FLAG_IN_DEFER;

    check_node(cxt, defer->block);
}

static void check_static_error(check_context_t cxt, ast_static_error_t *static_error) {
    const char *str;
    int         len;
    char       *buff;

    str  = get_string(static_error->str);
    len  = strlen(str);
    buff = mem_alloc(1 + len);
    strcat(buff, str);
    buff[len - 1] = 0;

    report_range_err(&ASTP(static_error)->loc, buff + 1);
}

static void check_static_vargs(check_context_t cxt, ast_static_vargs_t *static_vargs) {
    u32      vargs_ty;
    array_t  blocks;
    type_t   list_type;
    int      i;
    ast_t   *new;
    u32      proc_ty;

    ASTP(static_vargs)->type = TY_NOT_TYPED;

    if (cxt.proc == NULL) {
        report_range_err(&ASTP(static_vargs)->loc, "static directive 'VARGS' is only allowed within a procedure");
        return;
    }

    if (!(ASTP(cxt.proc)->flags & AST_FLAG_VARARGS)) {
        report_range_err_no_exit(&ASTP(static_vargs)->loc, "static directive 'VARGS' is only allowed within a varargs procedure");
        report_range_info_no_context(&ASTP(cxt.unit_decl)->loc, "procedure '%s' is not varargs", get_string(cxt.unit_decl->name));
        return;
    }

    cxt.scope  = ((ast_block_t*)static_vargs->block)->scope;
    cxt.flags |= CHECK_FLAG_IN_VARGS;

    if (ASTP(cxt.proc)->flags & AST_FLAG_POLY_VARARGS) {
        vargs_ty = current_poly_vargs_type(cxt);
        ASSERT(vargs_ty != TY_UNKNOWN, "did not find ... constant");
        ASSERT(type_kind(vargs_ty) == _TY_TYPE_LIST, "... type is not a type list");

        blocks    = array_make(ast_t*);
        list_type = get_type_t(vargs_ty);

        for (i = 0; i < list_type.list_len; i += 1) {
            cxt.varg_ty = list_type.id_list[i];
            new = copy_tree(static_vargs->block);
            check_node(cxt, new);
            array_push(blocks, new);
        }

        ((ast_block_t*)static_vargs->block)->stmts = blocks;

        ASTP(static_vargs)->type = TY_POLY;
    } else {
        proc_ty = ASTP(cxt.proc)->type;
        ASSERT(get_num_param_types(proc_ty) > 0, "proc type must have params to be varargs");

        vargs_ty = get_param_type(proc_ty, get_num_param_types(proc_ty) - 1);
        ASSERT(type_kind(vargs_ty) == TY_VARGS, "last param type is not varargs");

        cxt.varg_ty = get_under_type(vargs_ty);

        check_node(cxt, static_vargs->block);
    }
}

static void check_node(check_context_t cxt, ast_t *node) {
    ast_t **it;

    switch (node->kind) {
#define X(_kind) case _kind:
        X_AST_DECLARATIONS
#undef X
            check_decl(cxt, (ast_decl_t*)node);
            break;
        case AST_MODULE:
            if (node->type == TY_UNKNOWN) {
                /*
                 * Prevent some recursion issues where nodes in the module reference
                 * the module and asking for the type would recurse infinitely.
                 */
                node->type                  = TY_MODULE;
                ASTP(cxt.parent_decl)->type = TY_MODULE;

                check_tags(cxt, ASTP(cxt.parent_decl), &(cxt.parent_decl)->tags);
            }

            cxt.scope = get_subscope_from_node(cxt.scope, node);
            ASSERT(cxt.scope != NULL, "did not get subscope");
            cxt.parent_decl = NULL;
            array_traverse(((ast_module_t*)node)->children, it) {
                check_node(cxt, *it);
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
        case AST_SD_BLOCK:
            node->type      = TY_NOT_TYPED;
            cxt.parent_decl = NULL;
            cxt.scope       = ((ast_block_t*)node)->scope;
            array_traverse(((ast_block_t*)node)->stmts, it) {
                check_node(cxt, *it);
            }
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
            if (node->flags & AST_FLAG_EXPR_TOP) {
                check_expr_unsatisfied_poly(cxt, node);
            }
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
        case AST_STATIC_COMMENT:
            node->type = TY_NOT_TYPED;
            break;
        case AST_STATIC_ERROR:
            check_static_error(cxt, (ast_static_error_t*)node);
            break;
        case AST_STATIC_VARGS:
            check_static_vargs(cxt, (ast_static_vargs_t*)node);
            break;
        case AST_BUILTIN:
            break;
        default:;
#ifdef SIMON_DO_ASSERTIONS
            report_simple_err_no_exit("INTERNAL ERROR: AST_%s unhandled in check_node()", ast_get_kind_str(node->kind));
            print_node(node);
            ASSERT(0, "unhandled AST node kind in check_node()");
#endif
    }

#ifdef SIMON_DO_ASSERTIONS
    if (node->type == TY_UNKNOWN) {
        report_simple_err_no_exit("INTERNAL ERROR: type not resolved in check_node()");
        print_node(node);
    }
    ASSERT(node->type != TY_UNKNOWN, "did not resolve type");
#endif
}
