#include "scope.h"
#include "ui.h"
#include "globals.h"
#include "memory.h"
#include "type.h"

static void insert_builtin_type(const char *name, u32 type_value) {
    string_id      name_id;
    ast_builtin_t *b;

    name_id          = get_string_id(name);
    b                = mem_alloc(sizeof(*b));
    ASTP(b)->kind    = AST_BUILTIN;
    ASTP(b)->type    = TY_TYPE;
    ASTP(b)->value.t = type_value;
    b->name          = name_id;

    add_symbol_if_new(global_scope, name_id, ASTP(b));
}

static void _insert_builtin_proc_like(const char *name, u32 ret_type_or_special, u32 n_params, u32 *param_types) {
    string_id      name_id;
    ast_builtin_t *b;
    u32            proc_type;

    name_id       = get_string_id(name);
    b             = mem_alloc(sizeof(*b));
    ASTP(b)->kind = AST_BUILTIN;
    b->name       = name_id;

    if (ret_type_or_special == TY_BUILTIN_SPECIAL) {
        proc_type = TY_BUILTIN_SPECIAL;
    } else {
        proc_type = get_proc_type(n_params, param_types, ret_type_or_special);
    }

    ASTP(b)->type    = proc_type;
    ASTP(b)->value.a = ASTP(b);

    add_symbol_if_new(global_scope, name_id, ASTP(b));
}

#define INSERT_BUILTIN_PROC_LIKE(_name, _ret_type, ...)          \
do {                                                             \
    u32 _param_types[] = { __VA_ARGS__ };                        \
    _insert_builtin_proc_like((_name),                           \
                             (_ret_type),                        \
                             sizeof(_param_types) / sizeof(u32), \
                             _param_types);                      \
} while (0)

void init_scopes(void) {

    global_scope = create_named_scope(NULL, AST_INVALID, NULL, get_string_id("<global scope>"));

    insert_builtin_type("bool", TY_BOOL);
    insert_builtin_type("char", TY_CHAR);
    insert_builtin_type("u8",   TY_U8);
    insert_builtin_type("u16",  TY_U16);
    insert_builtin_type("u32",  TY_U32);
    insert_builtin_type("u64",  TY_U64);
    insert_builtin_type("s8",   TY_S8);
    insert_builtin_type("s16",  TY_S16);
    insert_builtin_type("s32",  TY_S32);
    insert_builtin_type("s64",  TY_S64);


    INSERT_BUILTIN_PROC_LIKE("cast", TY_BUILTIN_SPECIAL); /* This gets custom typechecking in ast.c */
    INSERT_BUILTIN_PROC_LIKE("__builtin_prints", TY_NOT_TYPED, get_ptr_type(TY_CHAR));
    INSERT_BUILTIN_PROC_LIKE("__builtin_printp", TY_NOT_TYPED, get_ptr_type(TY_U8));
    INSERT_BUILTIN_PROC_LIKE("__builtin_printi", TY_NOT_TYPED, TY_S64);
    INSERT_BUILTIN_PROC_LIKE("__builtin_stack_alloc", get_ptr_type(TY_U8), TY_U64);
}

scope_t *create_scope(scope_t *parent, int kind, ast_t *node) {
    scope_t *scope;

    scope = mem_alloc(sizeof(*scope));

    scope->parent    = parent;
    scope->kind      = kind;
    scope->node      = node;
    scope->symbols   = array_make(string_id);
    scope->nodes     = array_make(ast_t*);
    scope->subscopes = array_make(scope_t*);
    scope->name_id   = parent != NULL ? parent->name_id : STRING_ID_NULL;
    scope->in_proc   = parent != NULL && (kind == AST_PROC || parent->in_proc);

    return scope;
}

scope_t *create_named_scope(scope_t *parent, int kind, ast_t *node, string_id name_id) {
    scope_t    *new_scope;
    const char *parent_name;
    const char *name;
    char        buff[SCOPE_NAME_BUFF_SIZE];

    new_scope = create_scope(parent, kind, node);

    if (parent == NULL || parent->parent == NULL) {
        new_scope->name_id = name_id;
    } else {
        parent_name = get_string(parent->name_id);
        name        = get_string(name_id);

        if (strlen(parent_name) + strlen(name) + 2 > SCOPE_NAME_BUFF_SIZE) {
            report_simple_err("INTERNAL ERROR: name too long");
            ASSERT(0, "name too long");
        }
        strncpy(buff, parent_name, SCOPE_NAME_BUFF_SIZE - strlen(buff) - 1);
        strncat(buff, ".", SCOPE_NAME_BUFF_SIZE - strlen(buff) - 1);
        strncat(buff, name, SCOPE_NAME_BUFF_SIZE - strlen(buff) - 1);
        new_scope->name_id = get_string_id(buff);
    }

    return new_scope;
}

ast_t *find_in_scope(scope_t *scope, string_id name_id) {
    ast_t     **existing_node_p;
    int         i;
    string_id  *sym;

    i = 0;
    array_traverse(scope->symbols, sym) {
        if (name_id == *sym) {
            existing_node_p = array_item(scope->nodes, i);
            ASSERT(existing_node_p != NULL, "symbol/node mismatch");
            return *existing_node_p;
        }
        i += 1;
    }

    return NULL;
}

ast_t *search_up_scopes_return_scope(scope_t *scope, string_id name_id, scope_t **out_scope) {
    ast_t *existing_node;

    existing_node = find_in_scope(scope, name_id);

    if (existing_node != NULL) {
        if (out_scope != NULL) {
            *out_scope = scope;
        }
    } else if (scope->parent != NULL) {
        existing_node = search_up_scopes_return_scope(scope->parent, name_id, out_scope);
    }

    return existing_node;
}

ast_t *search_up_scopes_stop_at_module_return_scope(scope_t *scope, string_id name_id, scope_t **out_scope) {
    ast_t *existing_node;

    existing_node = find_in_scope(scope, name_id);

    if (existing_node != NULL) {
        if (out_scope != NULL) {
            *out_scope = scope;
        }
    } else if (scope->kind != AST_MODULE && scope->parent != NULL) {
        existing_node = search_up_scopes_stop_at_module_return_scope(scope->parent, name_id, out_scope);
    }

    return existing_node;
}

ast_t *search_up_scopes(scope_t *scope, string_id name_id) {
    return search_up_scopes_return_scope(scope, name_id, NULL);
}

ast_t *search_up_scopes_stop_at_module(scope_t *scope, string_id name_id) {
    return search_up_scopes_stop_at_module_return_scope(scope, name_id, NULL);
}

void add_symbol_if_new(scope_t *scope, string_id name_id, ast_t *node) {
    ast_t *existing_node;

    if (name_id == UNDERSCORE_ID) { return; }

    existing_node = search_up_scopes_stop_at_module(scope, name_id);
    if (existing_node != NULL) { return; }

    array_push(scope->symbols, name_id);
    array_push(scope->nodes,   node);
}

scope_t *add_subscope(scope_t *scope, int kind, ast_t *node) {
    scope_t *subscope;

    subscope = create_scope(scope, kind, node);
    array_push(scope->subscopes, subscope);

    return subscope;
}

scope_t *add_named_subscope(scope_t *scope, int kind, ast_t *node, string_id name_id) {
    scope_t *subscope;

    subscope = create_named_scope(scope, kind, node, name_id);
    array_push(scope->subscopes, subscope);

    return subscope;
}

void move_subscope(scope_t *dst, scope_t *subscope) {
    subscope->parent = dst;

    array_push(dst->subscopes, subscope);
}

void free_scope_no_recurse(scope_t *scope) {
    array_free(scope->symbols);
    array_free(scope->nodes);
    array_free(scope->subscopes);
    mem_free(scope);
}

static void scope_find_origins(scope_t *scope) {
    int         i;
    ast_t     **node_p;
    ast_t      *node;
    string_id  *name_p;
    ast_t      *existing_node;
    scope_t   **subscope;
    int         j;

    ASSERT(scope->parent != NULL, "scope_find_origins() may not be called on the global scope");

again:;
    i = 0;
    array_traverse(scope->nodes, node_p) {
        node = *node_p;

        /* Proc parameters _must_ be origins. */
        if (node->kind == AST_PROC_PARAM)          { goto next; }
        /* So must polymorphic type names. */
        if (node->kind == AST_POLYMORPH_TYPE_NAME) { goto next; }

#ifdef SIMON_DO_ASSERTIONS
        if (!ast_kind_can_be_symbol_origin(node->kind)) {
            report_range_err_no_exit(&node->loc, "INTERNAL ERROR: node can't be the origin of a symbol");
            ASSERT(0, "node can't be the origin of a symbol");
        }
#endif

        ASSERT(ast_kind_is_assign(node->kind), "non-assign node being deleted in scope_find_origins()");

        name_p = array_item(scope->symbols, i);
        ASSERT(name_p != NULL, "node/symbol mismatch");
        existing_node = search_up_scopes_stop_at_module(scope->parent, *name_p);
        if (existing_node != NULL) {

            if (scope->in_proc) {
                j = 0;
                array_traverse(scope->subscopes, subscope) {
                    if ((*subscope)->node == existing_node) {
                        array_delete(scope->subscopes, j);
                        break;
                    }
                    j += 1;
                }

                array_delete(scope->symbols, i);
                array_delete(scope->nodes,   i);
                goto again;
            }
        }

        node->flags |= AST_FLAG_ORIGIN;

next:;
        i += 1;
    }

    array_traverse(scope->subscopes, subscope) {
        scope_find_origins(*subscope);
    }
}

static void scope_find_origins_thread(void *arg) { scope_find_origins((scope_t*)arg); }

void scopes_find_origins(scope_t *scope) {
    ast_t   **node_p;
    scope_t **subscope;

    ASSERT(scope->parent == NULL, "scopes_find_origins() must be called with the global scope");

    array_traverse(scope->nodes, node_p) {
#ifdef SIMON_DO_ASSERTIONS
        if (!ast_kind_can_be_symbol_origin((*node_p)->kind)) {
            report_range_err_no_exit(&(*node_p)->loc, "INTERNAL ERROR: node can't be the origin of a symbol");
            ASSERT(0, "node can't be the origin of a symbol");
        }
#endif
        (*node_p)->flags |= AST_FLAG_ORIGIN;
    }

    if (tp == NULL) {
        array_traverse(scope->subscopes, subscope) {
            scope_find_origins(*subscope);
        }
    } else {
        array_traverse(scope->subscopes, subscope) {
            tp_add_task(tp, scope_find_origins_thread, (void*)*subscope);
        }
        tp_wait(tp);
    }
}

scope_t *get_subscope_from_node(scope_t *scope, ast_t *node) {
    scope_t **it;

    array_traverse(scope->subscopes, it) {
        if ((*it)->node == node) { return *it; }
    }

    return NULL;
}

void _show_scope(scope_t *scope, int level) {
    string_id  *symbol_it;
    int         n;
    int         i;
    ast_t     **node_it;
    ast_t      *opening_node;
    scope_t   **subscope_it;

    array_traverse(scope->subscopes, subscope_it) { (*subscope_it)->visited = 0; }

    n = 0;
    array_traverse(scope->symbols, symbol_it) {
        for (i = 0; i < level; i += 1) {
            printf("  ");
        }
        node_it = array_item(scope->nodes, n);
        printf("%s: %s\n", get_string(*symbol_it), AST_STR((*node_it)->kind));

        opening_node = *node_it;
        switch (opening_node->kind) {
            case AST_ASSIGN_PROC:
            case AST_ASSIGN_MACRO:
            case AST_ASSIGN_MODULE:
                opening_node = ((ast_assign_t*)opening_node)->val;
                break;
            default:;
        }

        array_traverse(scope->subscopes, subscope_it) {
            if ((*subscope_it)->node == opening_node) {
                _show_scope(*subscope_it, level + 1);
                (*subscope_it)->visited = 1;
            }
        }
        n += 1;
    }

    array_traverse(scope->subscopes, subscope_it) {
        if (!((*subscope_it)->visited)) {
            for (i = 0; i < level; i += 1) {
                printf("  ");
            }
            printf("%s\n", AST_STR((*subscope_it)->node->kind));
            _show_scope(*subscope_it, level + 1);
        }
    }
}

void show_scope(scope_t *scope) {
    printf("\nBEGIN SCOPES\n");
    _show_scope(scope, 1);
    printf("END SCOPES\n\n");
}
