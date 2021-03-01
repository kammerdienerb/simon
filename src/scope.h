#ifndef __SCOPE_H__
#define __SCOPE_H__

#include "array.h"
#include "strings.h"
#include "ast.h"

typedef struct scope {
    struct scope *parent;
    int           kind;
    ast_t        *node;
    array_t       symbols;
    array_t       nodes;
    array_t       subscopes;
    int           visited;
} scope_t;

scope_t  create_scope(scope_t *parent, int kind, ast_t *node);
void     add_symbol_if_new(scope_t *scope, string_id name_id, ast_t *node);
scope_t *add_subscope(scope_t *scope, int kind, ast_t *node);
scope_t *move_subscope(scope_t *dst, scope_t *subscope);
void     free_scope_no_recurse(scope_t *scope);
void     scope_remove_assigns(scope_t *scope);

void show_scope(scope_t *scope);

#endif
