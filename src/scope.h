#ifndef __SCOPE_H__
#define __SCOPE_H__

#include "array.h"
#include "strings.h"
#include "ast.h"

#define SCOPE_NAME_BUFF_SIZE (1024)

typedef struct scope {
    struct scope *parent;
    int           kind;
    ast_t        *node;
    array_t       symbols;
    array_t       nodes;
    array_t       subscopes;
    string_id     name_id;
    int           in_proc;
    int           visited;
} scope_t;

void     init_scopes(void);
scope_t *create_scope(scope_t *parent, int kind, ast_t *node);
scope_t *create_named_scope(scope_t *parent, int kind, ast_t *node, string_id name_id);
void     add_symbol_if_new(scope_t *scope, string_id name_id, ast_t *node);
scope_t *add_subscope(scope_t *scope, int kind, ast_t *node);
scope_t *add_named_subscope(scope_t *scope, int kind, ast_t *node, string_id name_id);
void     move_subscope(scope_t *dst, scope_t *subscope);
void     free_scope_no_recurse(scope_t *scope);
void     scopes_find_origins(scope_t *scope);
scope_t *get_subscope_from_node(scope_t *scope, ast_t *node);
ast_t   *find_in_scope(scope_t *scope, string_id name_id);
ast_t   *search_up_scopes_return_scope(scope_t *scope, string_id name_id, scope_t **out_scope);
ast_t   *search_up_scopes_stop_at_module_return_scope(scope_t *scope, string_id name_id, scope_t **out_scope);
ast_t   *search_up_scopes(scope_t *scope, string_id name_id);
ast_t   *search_up_scopes_stop_at_module(scope_t *scope, string_id name_id);

void show_scope(scope_t *scope);

#endif
