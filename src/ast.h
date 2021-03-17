#ifndef __AST_H__
#define __AST_H__

#include "internal.h"
#include "src_range.h"
#include "strings.h"
#include "array.h"

#define MAX_PARAMS_OR_ARGS (128)

#define ASTP(_x) (&((_x)->ast))

#define X_AST                  \
    X(AST_INVALID)             \
                               \
    X(AST_BUILTIN)             \
                               \
    X(AST_STATIC_IF)           \
    X(AST_STATIC_IF_BUILTIN)   \
    X(AST_STATIC_ELIF)         \
    X(AST_STATIC_ELSE)         \
    X(AST_STATIC_ENDIF)        \
    X(AST_STATIC_ASSERT)       \
    X(AST_STATIC_COMMENT)      \
    X(AST_STATIC_ERROR)        \
    X(AST_MODULE)              \
    X(AST_PROC)                \
    X(AST_STRUCT)              \
    X(AST_MACRO)               \
    X(AST_ASSIGN_EXPR)         \
    X(AST_ASSIGN_PROC)         \
    X(AST_ASSIGN_STRUCT)       \
    X(AST_ASSIGN_MACRO)        \
    X(AST_ASSIGN_MODULE)       \
    X(AST_POLYMORPH_TYPE_NAME) \
    X(AST_PROC_PARAM)          \
    X(AST_STRUCT_FIELD)        \
    X(AST_INT)                 \
    X(AST_STRING)              \
    X(AST_BOOL)                \
    X(AST_IDENT)               \
    X(AST_UNARY_EXPR)          \
    X(AST_BIN_EXPR)            \
    X(AST_BLOCK)               \
    X(AST_ARG_LIST)            \
    X(AST_IF)                  \
    X(AST_LOOP)                \
    X(AST_RETURN)              \
    X(AST_DEFER)               \
    X(AST_BREAK)               \
    X(AST_CONTINUE)

#define X_AST_ASSIGNS          \
    X(AST_ASSIGN_EXPR)         \
    X(AST_ASSIGN_PROC)         \
    X(AST_ASSIGN_STRUCT)       \
    X(AST_ASSIGN_MACRO)        \
    X(AST_ASSIGN_MODULE)


enum {
#define X(kind) kind,
X_AST
#undef X
};

#define AST_FLAG_CHECKED              (1 << 0)
#define AST_FLAG_POLYMORPH            (1 << 1)
#define AST_FLAG_VARARGS              (1 << 2)
#define AST_FLAG_ORIGIN               (1 << 3)

struct ast;

typedef union {
    i32         b;
    char        c;
    u64         u;
    i64         i;
    double      f;
    string_id   s;
    void       *v;
    u32         t;
    struct ast *a;
} value_t;

typedef struct ast {
    src_range_t loc;   /*        48 bytes                  */
    u32         type;  /*        +4 bytes                  */
    u16         kind;  /*        +2 bytes                  */
    u16         flags; /*        +2 bytes                  */
    value_t     value; /*        +8 bytes                  */
                       /* Total: 64 bytes (one cache line) */
} ast_t;


int ast_kind_is_assign(int kind);
int ast_kind_can_be_symbol_origin(int kind);
const char *ast_get_kind_str(int kind);
#define AST_STR(kind) (ast_get_kind_str((kind)))




struct scope;
typedef struct scope scope_t;


#define AST_DEFINE(name, ...) \
typedef struct {              \
    ast_t ast;                \
    __VA_ARGS__               \
} ast_##name##_t

AST_DEFINE(builtin,
    string_id name;
);

AST_DEFINE(static_if,
    ast_t *expr;
);

AST_DEFINE(static_if_builtin,
    ast_t *expr;
);

AST_DEFINE(static_elif,
    ast_t *expr;
);

AST_DEFINE(static_else);

AST_DEFINE(static_endif);

AST_DEFINE(static_assert,
    ast_t *expr;
);

AST_DEFINE(static_comment);

AST_DEFINE(static_error,
    string_id str;
);

AST_DEFINE(module,
    array_t children;
);

AST_DEFINE(polymorph_type_name,
    string_id name;
);

AST_DEFINE(proc_param,
    string_id  name;
    ast_t     *type_expr_or_polymorph_type_name;
    ast_t     *val;
);

AST_DEFINE(block,
    array_t stmts;
);

AST_DEFINE(proc,
    array_t  params;
    ast_t   *ret_type_expr;
    ast_t   *block;
);

AST_DEFINE(struct_field,
    string_id name;
);

AST_DEFINE(struct,
    array_t fields;
);

AST_DEFINE(macro,
);

AST_DEFINE(assign,
    scope_t   *scope;
    string_id  name;
    ast_t     *val;
    array_t    tags;
);

AST_DEFINE(int,
    string_id str_rep;
);

AST_DEFINE(string,
    string_id str_rep;
);

AST_DEFINE(bool,
    int is_true;
);

AST_DEFINE(ident,
    string_id  str_rep;
    ast_t     *resolved_node;
);

AST_DEFINE(unary_expr,
    ast_t *child;
    int    op;
);

AST_DEFINE(bin_expr,
    ast_t *left;
    ast_t *right;
    int      op;
);

typedef struct {
    string_id  name;
    ast_t     *expr;
} arg_t;

AST_DEFINE(arg_list,
    array_t args;
);

AST_DEFINE(if,
    ast_t *expr;
    ast_t *then_block;
    ast_t *els; /* May be a block or another if stmt. */
);

AST_DEFINE(loop,
    ast_t *init;
    ast_t *cond;
    ast_t *post;
    ast_t *block;
);

AST_DEFINE(defer,
    ast_t *block;
);

AST_DEFINE(return,
    ast_t *expr;
);

AST_DEFINE(break);
AST_DEFINE(continue);

void check_node(ast_t *node, scope_t *scope, ast_assign_t *parent_assign);

#endif
