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

    ASTP(b)->flags |= AST_FLAG_CONSTANT;

    add_symbol(global_scope, name_id, ASTP(b));
}

static void insert_builtin_macro(const char *name) {
    string_id      name_id;
    ast_builtin_t *b;

    name_id          = get_string_id(name);
    b                = mem_alloc(sizeof(*b));
    ASTP(b)->kind    = AST_BUILTIN;
    ASTP(b)->type    = TY_MACRO;
    ASTP(b)->value.a = ASTP(b);
    b->name          = name_id;

    ASTP(b)->flags |= AST_FLAG_CONSTANT;

    add_symbol(global_scope, name_id, ASTP(b));
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

    ASTP(b)->flags |= AST_FLAG_CONSTANT;

    add_symbol(global_scope, name_id, ASTP(b));
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

    global_scope = create_named_scope(NULL, AST_GLOBAL_SCOPE, NULL, get_string_id("<global scope>"));

    insert_builtin_type("type",   TY_TYPE);
    insert_builtin_type("module", TY_MODULE);
    insert_builtin_type("u8",     TY_U8);
    insert_builtin_type("u16",    TY_U16);
    insert_builtin_type("u32",    TY_U32);
    insert_builtin_type("u64",    TY_U64);
    insert_builtin_type("s8",     TY_S8);
    insert_builtin_type("s16",    TY_S16);
    insert_builtin_type("s32",    TY_S32);
    insert_builtin_type("s64",    TY_S64);
    insert_builtin_type("f32",    TY_F32);
    insert_builtin_type("f64",    TY_F64);


    INSERT_BUILTIN_PROC_LIKE("cast", TY_BUILTIN_SPECIAL); /* This gets custom typechecking in ast.c */
    INSERT_BUILTIN_PROC_LIKE("_builtin_prints", TY_NOT_TYPED, get_ptr_type(TY_U8));
    INSERT_BUILTIN_PROC_LIKE("_builtin_printp", TY_NOT_TYPED, get_ptr_type(TY_U8));
    INSERT_BUILTIN_PROC_LIKE("_builtin_printi", TY_NOT_TYPED, TY_S64);
    INSERT_BUILTIN_PROC_LIKE("_builtin_putc",   TY_NOT_TYPED, TY_U8);
    INSERT_BUILTIN_PROC_LIKE("_builtin_stack_alloc", get_ptr_type(TY_U8), TY_U64);
    INSERT_BUILTIN_PROC_LIKE("_builtin_stack_pointer", get_ptr_type(TY_U8));
    INSERT_BUILTIN_PROC_LIKE("_builtin_base_pointer", get_ptr_type(TY_U8));
    INSERT_BUILTIN_PROC_LIKE("_builtin_varg", TY_BUILTIN_SPECIAL);
    INSERT_BUILTIN_PROC_LIKE("_builtin_slice_from", TY_BUILTIN_SPECIAL);
    INSERT_BUILTIN_PROC_LIKE("_builtin_outb", TY_NOT_TYPED, get_ptr_type(TY_U8), TY_U8);
    INSERT_BUILTIN_PROC_LIKE("_builtin_inb", TY_U8, get_ptr_type(TY_U8));

    insert_builtin_macro("compile_error");
    insert_builtin_macro("require");
    insert_builtin_macro("static_if");
    insert_builtin_macro("vargs");
    insert_builtin_macro("code_to_string");
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
    scope->in_macro  = parent != NULL && (kind == AST_MACRO_EXPAND_SCOPE || parent->in_macro);

    if (kind == AST_MACRO_EXPAND_SCOPE) {
        scope->macro_scope_id = macro_scope_counter++;
    } else {
        scope->macro_scope_id = 0;
    }

    return scope;
}

scope_t *copy_scope(scope_t *scope) {
    scope_t  *new_scope;

    new_scope = mem_alloc(sizeof(*scope));

    memcpy(new_scope, scope, sizeof(*new_scope));
    new_scope->symbols   = array_make_with_cap(string_id, array_len(scope->symbols));
    new_scope->nodes     = array_make_with_cap(ast_t*,    array_len(scope->nodes));
    new_scope->subscopes = array_make_with_cap(scope_t*,  array_len(scope->subscopes));

    return new_scope;
}

scope_t *create_named_scope(scope_t *parent, int kind, ast_t *node, string_id name_id) {
    scope_t    *new_scope;
    const char *parent_name;
    const char *name;
    char        buff[SCOPE_NAME_BUFF_SIZE];

    new_scope = create_scope(parent, kind, node);

    if (parent == NULL || parent->kind == AST_GLOBAL_SCOPE) {
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

    /* @performance
     * Maybe binary search instead? They're just integers, so linear search will get us pretty far. */
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

ast_t *search_up_scopes(scope_t *scope, string_id name_id) {
    return search_up_scopes_return_scope(scope, name_id, NULL);
}

static void redecl_error(string_id name, ast_t *bad, ast_t *existing) {
    report_range_err_no_exit(&bad->loc, "redeclaration of '%s'", get_string(name));

    if (bad->kind == AST_IDENT && bad->flags & AST_FLAG_POLYMORPH) {
        report_simple_info_no_exit("'%%%s' declares a polymorphic parameter in a type pattern", get_string(name));
    }

    if (existing->kind == AST_BUILTIN) {
        report_simple_info("'%s' is a compiler builtin", get_string(name));
    } else {
        if (existing->kind == AST_IDENT && existing->flags & AST_FLAG_POLYMORPH) {
            report_range_info_no_exit(&existing->loc, "competing declaration here:");
            report_simple_info("'%%%s' declares a polymorphic parameter in a type pattern", get_string(name));
        } else {
            report_range_info(&existing->loc, "competing declaration here:");
        }
    }
}

void add_symbol(scope_t *scope, string_id name_id, ast_t *node) {
    ast_t   *existing_node;
    scope_t *exists_at;

    existing_node = search_up_scopes_return_scope(scope, name_id, &exists_at);

    if (existing_node != NULL) {
        if (scope->in_macro
        &&  exists_at->in_macro
        &&  node->flags & AST_FLAG_NAME_IN_MACRO
        &&  node->flags & AST_FLAG_MACRO_PUBLIC) {
            /* Let this through. */

        } else if (existing_node->kind == AST_BUILTIN
               ||  scope == exists_at
               ||  exists_at->in_macro
               ||  (exists_at->kind != AST_GLOBAL_SCOPE && (node->flags & AST_FLAG_MACRO_PUBLIC))) {

            redecl_error(name_id, node, existing_node);
            return;
        }
    }

    array_push(scope->symbols, name_id);
    array_push(scope->nodes,   node);
}

static void propagate_in_proc(scope_t *scope) {
    scope_t **it;

    scope->in_proc = 1;

    array_traverse(scope->subscopes, it) {
        propagate_in_proc(*it);
    }
}

static void propagate_in_macro(scope_t *scope) {
    scope_t **it;

    scope->in_macro = 1;

    array_traverse(scope->subscopes, it) {
        propagate_in_macro(*it);
    }
}

void insert_subscope(scope_t *scope, scope_t *subscope) {
    subscope->parent   = scope;
    subscope->in_proc  = scope->kind == AST_PROC || subscope->kind == AST_PROC || scope->in_proc;
    subscope->in_macro = scope->kind == AST_MACRO_EXPAND_SCOPE || subscope->kind == AST_MACRO_EXPAND_SCOPE || scope->in_macro;
    array_push(scope->subscopes, subscope);

    if (subscope->in_proc) {
        propagate_in_proc(subscope);
    }
    if (subscope->in_macro) {
        propagate_in_macro(subscope);
    }
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

void free_scope_no_recurse(scope_t *scope) {
    array_free(scope->symbols);
    array_free(scope->nodes);
    array_free(scope->subscopes);
    mem_free(scope);
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
            case AST_DECL_PROC:
            case AST_DECL_STRUCT:
            case AST_DECL_MACRO:
            case AST_DECL_MODULE:
                opening_node = ((ast_decl_t*)opening_node)->val_expr;
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
