#include "interp.h"
#include "array.h"
#include "hash_table.h"
#include "hash_utilities.h"
#include "globals.h"
#include "type.h"
#include "ui.h"
#include "parse.h"
#include "memory.h"

typedef struct {
    value_t v;
    u32     t;
    int     is_intermediate;
} interp_value_t;

#define NODE_SCRATCH_IVAL(np) ((interp_value_t){ (np)->value, (np)->type, 1 })

static interp_value_t *intermediate_from_node(ast_t *node) {
    interp_value_t *interm;

    interm  = tmp_mem_alloc(sizeof(*interm));
    *interm = NODE_SCRATCH_IVAL(node);

    return interm;
}

static interp_value_t *intermediate_from_scratch(interp_value_t v) {
    interp_value_t *interm;

    interm  = tmp_mem_alloc(sizeof(*interm));
    *interm = v;

    return interm;
}


use_hash_table(string_id, interp_value_t);
typedef hash_table(string_id, interp_value_t) vtab_t;

#define vtab_make()                    (hash_table_make(string_id, interp_value_t, str_id_hash))
#define vtab_free(_vtab)               (hash_table_free((_vtab)))
#define vtab_insert(_vtab, _key, _val) (hash_table_insert((_vtab), (_key), (_val)))
#define vtab_get(_vtab, _key)          (hash_table_get_val((_vtab), (_key)))

typedef struct {
    vtab_t  vtab;
    int     is_proc_frame;
    array_t stack_allocations;
} data_frame_t;

typedef struct {
    ast_t          *cur_node;
    array_t         frames;
    interp_value_t  tmp_value;
} interp_data_t;

static void push_frame(interp_data_t *in) {
    data_frame_t frame;

    frame.vtab          = vtab_make();
    frame.is_proc_frame = 0;
    ASSERT(frame.vtab != NULL, "couldn't make vtab");

    array_push(in->frames, frame);
}

static void push_proc_frame(interp_data_t *in) {
    data_frame_t frame;

    frame.vtab              = vtab_make();
    frame.is_proc_frame     = 1;
    frame.stack_allocations = array_make(void*);
    ASSERT(frame.vtab != NULL, "couldn't make vtab");

    array_push(in->frames, frame);
}

static void pop_frame(interp_data_t *in) {
    data_frame_t  *frame;
    void         **stack_alloc_it;

    ASSERT(array_len(in->frames) > 0, "no frames to pop");

    frame = array_last(in->frames);

    vtab_free(frame->vtab);

    if (frame->is_proc_frame) {
        array_rtraverse(frame->stack_allocations, stack_alloc_it) {
            tmp_mem_free(*stack_alloc_it);
        }
        array_free(frame->stack_allocations);
    }

    array_pop(in->frames);
}

static data_frame_t *get_proc_frame(interp_data_t *in) {
    data_frame_t *frame_it;

    array_rtraverse(in->frames, frame_it) {
        if (frame_it->is_proc_frame) {
            return frame_it;
        }
    }

    ASSERT(0, "did not find proc frame");

    return NULL;
}

static interp_value_t *frame_stack_alloc(interp_data_t *in, u64 n_bytes) {
    data_frame_t   *frame;
    void           *data;
    interp_value_t  scratch;

    frame = get_proc_frame(in);
    data  = tmp_mem_alloc(n_bytes);
    array_push(frame->stack_allocations, data);

    scratch.t   = get_ptr_type(TY_U8);
    scratch.v.v = data;

    return intermediate_from_scratch(scratch);
}

static void add_to_frame(interp_data_t *in, ast_t *node, interp_value_t *value) {
    data_frame_t   *frame;
    int             save_interm;
    string_id       name;

    frame = array_last(in->frames);

    save_interm            = value->is_intermediate;
    value->is_intermediate = 0;

    ASSERT(frame != NULL, "did not get frame");

    if (ast_kind_is_assign(node->kind)) {
        name = ((ast_assign_t*)node)->name;
    } else if (node->kind == AST_PROC_PARAM) {
        name = ((ast_proc_param_t*)node)->name;
    } else if (node->kind == AST_BUILTIN) {
        name = ((ast_builtin_t*)node)->name;
    } else {
        ASSERT(0, "unexpected node kind in add_to_frame()");
        return;
    }

#ifdef SIMON_DO_ASSERTIONS
    ASSERT(vtab_get(frame->vtab, name) == NULL, "name already exists in data_frame");
#endif

    vtab_insert(frame->vtab, name, *value);

    value->is_intermediate = save_interm;
}

static interp_value_t *find_value(interp_data_t *in, string_id name) {
    data_frame_t   *frame;
    interp_value_t *value;

    value = NULL;
    array_rtraverse(in->frames, frame) {
        value = vtab_get(frame->vtab, name);
        if (value != NULL) { break; }
    }

    ASSERT(value != NULL, "did not find value");

    return value;
}

#define FREE_INTERM(vp)                          \
do {                                             \
    if ((vp) != NULL && (vp)->is_intermediate) { \
        tmp_mem_free((vp));                      \
    }                                            \
    (vp) = NULL;                                 \
} while (0)

static interp_value_t *interp_node(interp_data_t *in, ast_t *node, array_t *call_args, int get_value_ptr) {
    interp_value_t    *value;
    interp_value_t    *lvalue;
    interp_value_t    *rvalue;
    interp_value_t     v;
    u32                i;
    ast_t            **node_it;
    int                op;
    u32                tk;
    u32                tkl;
    u32                tkr;
    array_t            this_call_args;
    u32                ut;
    arg_t             *arg_it;
    interp_value_t   **arg_val_it;

    value = lvalue = rvalue = NULL;

    in->cur_node = node;

    switch(node->kind) {
#define X(_kind) case _kind:
        X_AST_ASSIGNS
#undef X
        if (node->flags & AST_FLAG_ORIGIN) {
            v = NODE_SCRATCH_IVAL(node);
            add_to_frame(in, node, &v);
        }

        value    = find_value(in, ((ast_assign_t*)node)->name);
        rvalue   = interp_node(in, ((ast_assign_t*)node)->val, NULL, 0);
        value->v = rvalue->v;
        FREE_INTERM(rvalue);

        break;

        case AST_PROC:
            ASSERT(call_args != NULL, "no call args");

            push_proc_frame(in);

            i = 0;
            array_traverse(((ast_proc_t*)node)->params, node_it) {
                add_to_frame(in, *node_it, *(interp_value_t**)array_item(*call_args, i));
                i += 1;
            }

            interp_node(in, ((ast_proc_t*)node)->block, NULL, 0);

            pop_frame(in);
            break;

        case AST_BLOCK:
            array_traverse(((ast_block_t*)node)->stmts, node_it) {
                lvalue = interp_node(in, *node_it, NULL, 0);
                FREE_INTERM(lvalue);
            }
            break;

        case AST_IF:
            lvalue = interp_node(in, ((ast_if_t*)node)->expr, NULL, 0);
            if (lvalue->v.b) {
                push_frame(in);
                interp_node(in, ((ast_if_t*)node)->then_block, NULL, 0);
                pop_frame(in);
            } else {
                if (((ast_if_t*)node)->els) {
                    push_frame(in);
                    interp_node(in, ((ast_if_t*)node)->els, NULL, 0);
                    pop_frame(in);
                }
            }
            FREE_INTERM(lvalue);
            break;

        case AST_LOOP:
            push_frame(in);

            if (((ast_loop_t*)node)->init != NULL) {
                lvalue = interp_node(in, ((ast_loop_t*)node)->init, NULL, 0);
                FREE_INTERM(lvalue);
            }

            if (((ast_loop_t*)node)->cond != NULL) {
                while ((lvalue = interp_node(in, ((ast_loop_t*)node)->cond, NULL, 0))->v.b) {
                    FREE_INTERM(lvalue);
                    push_frame(in);
                    interp_node(in, ((ast_loop_t*)node)->block, NULL, 0);
                    pop_frame(in);
                    if (((ast_loop_t*)node)->post != NULL) {
                        rvalue = interp_node(in, ((ast_loop_t*)node)->post, NULL, 0);
                        FREE_INTERM(rvalue);
                    }
                }
                FREE_INTERM(lvalue);
            } else {
                while (1) {
                    push_frame(in);
                    interp_node(in, ((ast_loop_t*)node)->block, NULL, 0);
                    pop_frame(in);
                    if (((ast_loop_t*)node)->post != NULL) {
                        rvalue = interp_node(in, ((ast_loop_t*)node)->post, NULL, 0);
                        FREE_INTERM(rvalue);
                    }
                }
            }

            pop_frame(in);
            break;

        case AST_INT:
            value = intermediate_from_node(node);
            break;

        case AST_STRING:
            value      = intermediate_from_node(node);
            value->v.v = (void*)get_string(value->v.s);
            break;

        case AST_IDENT:
            value = find_value(in, ((ast_ident_t*)node)->str_rep);
            break;

        case AST_UNARY_EXPR:
            v      = NODE_SCRATCH_IVAL(node);
            op     = ((ast_unary_expr_t*)node)->op;
            tk     = type_kind(node->type);
            rvalue = interp_node(in, ((ast_unary_expr_t*)node)->child, NULL, 0);
            tkr    = type_kind(rvalue->t);

            switch (op) {
                case OP_ADDR:
                    if (tkr == TY_TYPE) {
                        v.v.t = node->type;
                    } else {
                        ASSERT(!rvalue->is_intermediate, "can't take address of intermediate value");
                        v.v.v = (void*)&(rvalue->v);
                    }
                    break;

                case OP_DEREF:
                    ASSERT(!rvalue->is_intermediate, "can't dereference an intermediate value");
                    if (get_value_ptr) {
                        v.v = rvalue->v.v;
                    } else {
                        switch (tk) {
                            case TY_GENERIC_INT:
                                switch (node->type) {
                                    case TY_U8:
                                    case TY_U16:
                                    case TY_U32:
                                    case TY_U64:
                                        v.v.u = *(typeof(rvalue->v.u)*)rvalue->v.v;
                                        break;
                                    case TY_S8:
                                    case TY_S16:
                                    case TY_S32:
                                    case TY_S64:
                                        v.v.s = *(typeof(rvalue->v.u)*)rvalue->v.v;
                                        break;
                                }
                                break;

                            case TY_CHAR:
                                v.v.c = *(typeof(rvalue->v.c)*)rvalue->v.v;
                                break;

                            case TY_PTR:
                                v.v.v = *(typeof(rvalue->v.v)*)rvalue->v.v;
                                break;

                            default:
                                goto dont_know_how;
                        }
                    }
                    break;

                default:
                    goto dont_know_how;

            }

            FREE_INTERM(rvalue);
            value = intermediate_from_scratch(v);
            break;

        case AST_BIN_EXPR:
            v  = NODE_SCRATCH_IVAL(node);
            op = ((ast_bin_expr_t*)node)->op;
            tk = type_kind(node->type);

            if (op == OP_ASSIGN
            ||  op == OP_PLUS_ASSIGN
            ||  op == OP_MINUS_ASSIGN
            ||  op == OP_MULT_ASSIGN
            ||  op == OP_DIV_ASSIGN
            ||  op == OP_MOD_ASSIGN) {

                lvalue = interp_node(in, ((ast_bin_expr_t*)node)->left, NULL, 1);
            } else {
                lvalue = interp_node(in, ((ast_bin_expr_t*)node)->left, NULL, 0);
            }


            tkl = type_kind(lvalue->t);

            if (op == OP_CALL) {
                ASSERT(lvalue->v.a->kind == AST_PROC || lvalue->v.a->kind == AST_BUILTIN,
                       "trying to call a value that isn't a proc or builtin");

                this_call_args = array_make(interp_value_t*);

                array_traverse(((ast_arg_list_t*)((ast_bin_expr_t*)node)->right)->args, arg_it) {
                    rvalue  = interp_node(in, arg_it->expr, NULL, 0);
                    array_push(this_call_args, rvalue);
                    rvalue = NULL;
                }

                value = interp_node(in, lvalue->v.a, &this_call_args, 0);
                FREE_INTERM(lvalue);

                array_traverse(this_call_args, arg_val_it) {
                    FREE_INTERM(*arg_val_it);
                }
                array_free(this_call_args);

                break;
            }

            rvalue  = interp_node(in, ((ast_bin_expr_t*)node)->right, NULL, 0);
            tkr     = type_kind(rvalue->t);

            if (op == OP_ASSIGN) {
                v.v = rvalue->v;
                goto do_assign;
            }

            switch (op) {
                case OP_LSS:
                    if (tkl == TY_GENERIC_INT) {
                        switch (lvalue->t) {
                            case TY_U8:
                            case TY_U16:
                            case TY_U32:
                            case TY_U64:
                                v.v.b = lvalue->v.u < rvalue->v.u;
                                break;
                            case TY_S8:
                            case TY_S16:
                            case TY_S32:
                            case TY_S64:
                                v.v.b = lvalue->v.i < rvalue->v.i;
                                break;
                        }
                    } else if (tkl == TY_PTR) {
                        v.v.b = lvalue->v.v < rvalue->v.v;
                    } else { goto dont_know_how; }

                    break;

                case OP_EQU:
                    if (tkl == TY_GENERIC_INT) {
                        switch (lvalue->t) {
                            case TY_U8:
                            case TY_U16:
                            case TY_U32:
                            case TY_U64:
                                v.v.b = lvalue->v.u == rvalue->v.u;
                                break;
                            case TY_S8:
                            case TY_S16:
                            case TY_S32:
                            case TY_S64:
                                v.v.b = lvalue->v.i == rvalue->v.i;
                                break;
                        }
                    } else if (tkl == TY_CHAR) {
                        v.v.b = lvalue->v.c == rvalue->v.c;
                    } else if (tkl == TY_PTR) {
                        v.v.b = lvalue->v.v == rvalue->v.v;
                    } else { goto dont_know_how; }

                    break;

                case OP_NEQ:
                    if (tkl == TY_GENERIC_INT) {
                        switch (lvalue->t) {
                            case TY_U8:
                            case TY_U16:
                            case TY_U32:
                            case TY_U64:
                                v.v.b = lvalue->v.u != rvalue->v.u;
                                break;
                            case TY_S8:
                            case TY_S16:
                            case TY_S32:
                            case TY_S64:
                                v.v.b = lvalue->v.i != rvalue->v.i;
                                break;
                        }
                    } else if (tkl == TY_CHAR) {
                        v.v.b = lvalue->v.c != rvalue->v.c;
                    } else if (tkl == TY_PTR) {
                        v.v.b = lvalue->v.v != rvalue->v.v;
                    } else { goto dont_know_how; }

                    break;

                case OP_PLUS:
                case OP_PLUS_ASSIGN:
                    if (tk == TY_PTR) {
                        /* @todo -- different pointers add different offsets */
                        if (tkl == TY_PTR) {
                            switch (rvalue->t) {
                                case TY_U8:
                                case TY_U16:
                                case TY_U32:
                                case TY_U64:
                                    v.v.v = lvalue->v.v + rvalue->v.u;
                                    break;
                                case TY_S8:
                                case TY_S16:
                                case TY_S32:
                                case TY_S64:
                                    v.v.v = lvalue->v.v + rvalue->v.i;
                                    break;
                            }
                        } else {
                            switch (lvalue->t) {
                                case TY_U8:
                                case TY_U16:
                                case TY_U32:
                                case TY_U64:
                                    v.v.v = lvalue->v.u + rvalue->v.v;
                                    break;
                                case TY_S8:
                                case TY_S16:
                                case TY_S32:
                                case TY_S64:
                                    v.v.v = lvalue->v.i + rvalue->v.v;
                                    break;
                            }
                        }
                    } else if (tk == TY_GENERIC_INT) {
                        switch (lvalue->t) {
                            case TY_U8:
                            case TY_U16:
                            case TY_U32:
                            case TY_U64:
                                v.v.u = lvalue->v.u + rvalue->v.u;
                                break;
                            case TY_S8:
                            case TY_S16:
                            case TY_S32:
                            case TY_S64:
                                v.v.i = lvalue->v.i + rvalue->v.i;
                                break;
                        }
                    } else { goto dont_know_how; }

                    break;

                case OP_MULT:
                    if (tk == TY_GENERIC_INT) {
                        switch (lvalue->t) {
                            case TY_U8:
                            case TY_U16:
                            case TY_U32:
                            case TY_U64:
                                v.v.u = lvalue->v.u * rvalue->v.u;
                                break;
                            case TY_S8:
                            case TY_S16:
                            case TY_S32:
                            case TY_S64:
                                v.v.i = lvalue->v.i * rvalue->v.i;
                                break;
                        }
                    } else { goto dont_know_how; }

                    break;

                case OP_SUBSCRIPT:
                    ut  = get_under_type(lvalue->t);
                    v.t = ut;
                    switch (ut) {
                        case TY_U8:
                        case TY_U16:
                        case TY_U32:
                        case TY_U64:
                            v.v.u = *(((typeof(lvalue->v.u)*)lvalue->v.v) + rvalue->v.s);
                            break;
                        case TY_S8:
                        case TY_S16:
                        case TY_S32:
                        case TY_S64:
                            v.v.s = *(((typeof(lvalue->v.s)*)lvalue->v.v) + rvalue->v.s);
                            break;
                        case TY_CHAR:
                            v.v.c = *(((typeof(lvalue->v.c)*)lvalue->v.v) + rvalue->v.s);
                            break;

                        default:
                            if (tkl == TY_PTR) {
                                v.v.v = *(((typeof(lvalue->v.v)*)lvalue->v.v) + rvalue->v.s);
                            } else {
                                ASSERT(0, "unable to interpret subscript");
                            }
                            break;
                    }
                    break;

                default:
                    goto dont_know_how;
            }

            if (op == OP_ASSIGN
            ||  op == OP_PLUS_ASSIGN
            ||  op == OP_MINUS_ASSIGN
            ||  op == OP_MULT_ASSIGN
            ||  op == OP_DIV_ASSIGN
            ||  op == OP_MOD_ASSIGN) {

do_assign:;
                ASSERT(!lvalue->is_intermediate, "lvalue of assignment shouldn't be intermediate");

                /* @here
                ** This isn't really working the way I want...
                ** This doesn't handle actual writes to pointers correctly,
                ** which is kinda important.
                */

                ((interp_value_t*)lvalue->v)->v = v.v;

                value    = lvalue->v.v;
                lvalue   = NULL;
                value->v = v.v;
            } else {
                value = intermediate_from_scratch(v);
            }

            FREE_INTERM(lvalue);
            FREE_INTERM(rvalue);

            break;

        case AST_BUILTIN:
            if (((ast_builtin_t*)node)->name == __BUILTIN_PRINTS_ID) {
                v = **(interp_value_t**)array_item(*call_args, 0);
                printf("%s", (char*)v.v.v);
            } else if (((ast_builtin_t*)node)->name == __BUILTIN_PRINTP_ID) {
                v = **(interp_value_t**)array_item(*call_args, 0);
                printf("%p", v.v.v);
            } else if (((ast_builtin_t*)node)->name == __BUILTIN_PRINTI_ID) {
                v = **(interp_value_t**)array_item(*call_args, 0);
                printf("%lld", v.v.i);
            } else if (((ast_builtin_t*)node)->name == __BUILTIN_STACK_ALLOC_ID) {
                value = frame_stack_alloc(in, (*(interp_value_t**)array_item(*call_args, 0))->v.u);
            } else if (((ast_builtin_t*)node)->name == CAST_ID) {
                v   = **(interp_value_t**)array_item(*call_args, 1);
                v.t = (*(interp_value_t**)array_item(*call_args, 0))->v.t;
                value = intermediate_from_scratch(v);
            } else {
                ASSERT(0, "unhandled builtin");
            }
            break;

        default:
dont_know_how:;
            report_range_err_no_exit(&node->loc, "INTERNAL ERROR: don't know how to interpret");
            ASSERT(0, "unhandled kind in interp_node");
    }

    ASSERT(lvalue == NULL, "forgot to free lvalue somewhere");
    ASSERT(rvalue == NULL, "forgot to free rvalue somewhere");

    return value;
}

void interp(void) {
    interp_data_t    in;
    ast_t          **rootp;
    array_t          call_args;
    interp_value_t   v;
    interp_value_t  *arg_value;

    in.frames = array_make(data_frame_t);
    push_frame(&in);

    array_traverse(global_scope->nodes, rootp) {
        if ((*rootp)->kind == AST_BUILTIN) {
            v.v = (*rootp)->value;
            v.t = (*rootp)->type;
            add_to_frame(&in, *rootp, &v);
        }
    }
    array_traverse(roots, rootp) {
        if (ast_kind_is_assign((*rootp)->kind)) {
            v.v = (*rootp)->value;
            v.t = (*rootp)->type;
            add_to_frame(&in, *rootp, &v);
        }
    }

    call_args = array_make(interp_value_t*);

    v.t   = TY_S64;
    v.v.i = array_len(options.interp_args);
    arg_value = intermediate_from_scratch(v);
    array_push(call_args, arg_value);
    v.t   = get_ptr_type(get_ptr_type(TY_CHAR));
    v.v.v = array_data(options.interp_args);
    arg_value = intermediate_from_scratch(v);
    array_push(call_args, arg_value);

    interp_node(&in, program_entry->val, &call_args, 0);

    (void)pop_frame;
}
