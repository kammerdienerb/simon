#include "scope.h"
#include "ui.h"

scope_t create_scope(scope_t *parent, int kind, ast_t *node) {
    scope_t scope;

    scope.parent    = parent;
    scope.kind      = kind;
    scope.node      = node;
    scope.symbols   = array_make(string_id);
    scope.nodes     = array_make(ast_t*);
    scope.subscopes = array_make(scope_t);

    return scope;
}

ast_t *find_in_scope(scope_t *scope, string_id name_id, ast_t *node) {
    ast_t     *existing_node;
    int        i;
    string_id *sym;

    existing_node = NULL;
    i             = 0;
    array_traverse(scope->symbols, sym) {
        if (name_id == *sym) {
            existing_node = *(ast_t**)array_item(scope->nodes, i);
            return existing_node;
        }
        i += 1;
    }

    return NULL;
}

ast_t *search_up_scopes(scope_t *scope, string_id name_id, ast_t *node) {
    ast_t *existing_node;

    existing_node = find_in_scope(scope, name_id, node);

    if (existing_node == NULL && scope->parent != NULL) {
        existing_node = search_up_scopes(scope->parent, name_id, node);
    }

    return existing_node;
}

void add_symbol_if_new(scope_t *scope, string_id name_id, ast_t *node) {
    ast_t *existing_node;

    switch (scope->kind) {
        case AST_INVALID: /* global scope */
        case AST_MODULE:
            existing_node = find_in_scope(scope, name_id, node);
            if (existing_node != NULL) {
                report_range_err_no_exit(&node->loc, "redeclaration of '%s'", get_string(name_id));
                report_range_info(&existing_node->loc, "competing declaration here:");
                return;
            }
            break;
        case AST_STRUCT:
            existing_node = find_in_scope(scope, name_id, node);
            if (existing_node != NULL) {
                report_range_err_no_exit(&node->loc, "redeclaration of struct field '%s'", get_string(name_id));
                report_range_info(&existing_node->loc, "competing declaration here:");
                return;
            }
            break;
        case AST_PROC:
            existing_node = search_up_scopes(scope, name_id, node);
            if (existing_node != NULL) { return; }
            break;
    }

    array_push(scope->symbols, name_id);
    array_push(scope->nodes,   node);
}

scope_t *add_subscope(scope_t *scope, int kind, ast_t *node) {
    scope_t  subscope;
    scope_t *ret;

    subscope = create_scope(scope, kind, node);
    ret      = array_push(scope->subscopes, subscope);

    return ret;
}

scope_t *move_subscope(scope_t *dst, scope_t *subscope) {
    scope_t *ret;

    subscope->parent = dst;

    ret = array_push(dst->subscopes, *subscope);

    return ret;
}

void free_scope_no_recurse(scope_t *scope) {
    array_free(scope->symbols);
    array_free(scope->nodes);
    array_free(scope->subscopes);
}

void scope_remove_assigns(scope_t *scope) {
/*     int       i; */
    ast_t   **node_p;
    ast_t    *node;
    scope_t  *subscope;

/*     i = 0; */
    array_traverse(scope->nodes, node_p) {
        node = *node_p;
        if (node->kind == AST_ASSIGN_EXPR) {
        }
    }

    array_traverse(scope->subscopes, subscope) {
        scope_remove_assigns(subscope);
    }
}

void _show_scope(scope_t *scope, int level) {
    string_id  *symbol_it;
    int         n;
    int         i;
    ast_t     **node_it;
    ast_t      *opening_node;
    scope_t    *subscope_it;

    array_traverse(scope->subscopes, subscope_it) { subscope_it->visited = 0; }

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
            case AST_ASSIGN_STRUCT:
            case AST_ASSIGN_MACRO:
            case AST_ASSIGN_MODULE:
                opening_node = ((ast_assign_t*)opening_node)->val;
                break;
            default:;
        }

        array_traverse(scope->subscopes, subscope_it) {
            if (subscope_it->node == opening_node) {
                _show_scope(subscope_it, level + 1);
                subscope_it->visited = 1;
            }
        }
        n += 1;
    }

    array_traverse(scope->subscopes, subscope_it) {
        if (!subscope_it->visited) {
            for (i = 0; i < level; i += 1) {
                printf("  ");
            }
            printf("%s\n", AST_STR(subscope_it->node->kind));
            _show_scope(subscope_it, level + 1);
        }
    }
}

void show_scope(scope_t *scope) {
    printf("\nBEGIN SCOPES\n");
    _show_scope(scope, 1);
    printf("END SCOPES\n\n");
}
