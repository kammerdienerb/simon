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
    X(AST_STATIC_ASSERT)       \
    X(AST_STATIC_COMMENT)      \
    X(AST_STATIC_ERROR)        \
    X(AST_STATIC_VARGS)        \
    X(AST_MODULE)              \
    X(AST_PROC)                \
    X(AST_STRUCT)              \
    X(AST_MACRO)               \
    X(AST_DECL_VAR)            \
    X(AST_DECL_PROC)           \
    X(AST_DECL_STRUCT)         \
    X(AST_DECL_MACRO)          \
    X(AST_DECL_MODULE)         \
    X(AST_PARAM)               \
    X(AST_STRUCT_FIELD)        \
    X(AST_INT)                 \
    X(AST_FLOAT)               \
    X(AST_STRING)              \
    X(AST_CHAR)                \
    X(AST_IDENT)               \
    X(AST_UNARY_EXPR)          \
    X(AST_BIN_EXPR)            \
    X(AST_BLOCK)               \
    X(AST_SD_BLOCK)            \
    X(AST_ARG_LIST)            \
    X(AST_IF)                  \
    X(AST_LOOP)                \
    X(AST_RETURN)              \
    X(AST_DEFER)               \
    X(AST_BREAK)               \
    X(AST_CONTINUE)

#define X_AST_DECLARATIONS     \
    X(AST_DECL_VAR)            \
    X(AST_DECL_PROC)           \
    X(AST_DECL_STRUCT)         \
    X(AST_DECL_MACRO)          \
    X(AST_DECL_MODULE)

#define X_AST_LEAF_EXPRS       \
    X(AST_INT)                 \
    X(AST_FLOAT)               \
    X(AST_STRING)              \
    X(AST_IDENT)

enum {
#define X(kind) kind,
X_AST
#undef X
};

enum {
    AST_FLAG_POLYMORPH            = (1 << 1),
    AST_FLAG_VARARGS              = (1 << 2),
    AST_FLAG_POLY_VARARGS         = (1 << 3),
    AST_FLAG_CALL_IS_CAST         = (1 << 4),
    AST_FLAG_CALL_IS_BUILTIN_VARG = (1 << 5),
    AST_FLAG_IS_EXTERN            = (1 << 6),
    AST_FLAG_EXPR_TOP             = (1 << 7),
    AST_FLAG_IS_COPY              = (1 << 8),
    AST_FLAG_PAREN_EXPR           = (1 << 9),
    AST_FLAG_BITFIELD_DOT         = (1 << 10),
    AST_FLAG_HEX_INT              = (1 << 11),
    AST_FLAG_CONSTANT             = (1 << 12),
};

struct ast;

typedef union {
    u64         u;
    i64         i;
    double      f;
    string_id   s;
    void       *v;
    u32         t;
    struct ast *a;
} value_t;

string_id value_to_string_id(value_t val, u32 type);

typedef struct ast {
    src_range_t loc;   /*        48 bytes                  */
    u32         type;  /*        +4 bytes                  */
    u16         kind;  /*        +2 bytes                  */
    u16         flags; /*        +2 bytes                  */
    value_t     value; /*        +8 bytes                  */
                       /* Total: 64 bytes (one cache line) */
} ast_t;


int ast_kind_is_decl(int kind);
const char *ast_get_kind_str(int kind);
#define AST_STR(kind) (ast_get_kind_str((kind)))

typedef struct {
    string_id name;
    value_t   value;
    u32       type;
} polymorph_constant_t;

typedef struct {
    array_t  constants;
    ast_t   *node;
    ast_t   *specialization;
    u32      type;
} polymorphed_t;

typedef struct {
    ast_t   *node;
    value_t  value;
    u32      has_value;
    u32      type;
} poly_arg_t;


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

AST_DEFINE(static_assert,
    ast_t *expr;
);

AST_DEFINE(static_comment);

AST_DEFINE(static_error,
    string_id str;
);

AST_DEFINE(static_vargs,
    scope_t *scope;
    ast_t   *block;
);

AST_DEFINE(module,
    scope_t *scope;
    array_t  children;
);

AST_DEFINE(param,
    string_id  name;
    ast_t     *type_expr;
    ast_t     *val;
);

AST_DEFINE(block,
    scope_t *scope;
    array_t  stmts;
);

AST_DEFINE(proc,
    scope_t     *scope;
    src_range_t  params_loc;
    array_t      params;
    ast_t       *ret_type_expr;
    ast_t       *block;
    array_t      polymorphs;
);

AST_DEFINE(struct_field,
    string_id  name;
    ast_t     *type_expr;
    array_t    tags;
    u64        bitfield_mask;
    u32        bitfield_shift;
);

AST_DEFINE(struct,
    scope_t     *scope;
    src_range_t  params_loc;
    array_t      params;
    array_t      fields;
    array_t      polymorphs;
    u8           bitfield_struct_bits;
);

AST_DEFINE(macro,
);

AST_DEFINE(decl,
    scope_t   *scope;
    string_id  name;
    string_id  full_name;
    ast_t     *type_expr;
    ast_t     *val_expr;
    array_t    tags;
);

AST_DEFINE(int,
    string_id str_rep;
);

AST_DEFINE(float,
    string_id str_rep;
);

AST_DEFINE(string,
    string_id str_rep;
);

AST_DEFINE(char,
    string_id str_rep;
);

AST_DEFINE(ident,
    string_id  str_rep;
    ast_t     *resolved_node;
    int        poly_idx;
    int        varg_idx;
);

AST_DEFINE(unary_expr,
    ast_t *child;
    ast_t *array_size_expr;
    int    op;
);

AST_DEFINE(bin_expr,
    ast_t *left;
    ast_t *right;
    int    op;
);

typedef struct {
    string_id  name;
    ast_t     *expr;
} arg_t;

AST_DEFINE(arg_list,
    array_t args;
);

AST_DEFINE(if,
    scope_t *scope;
    ast_t   *expr;
    ast_t   *then_block;
    ast_t   *els; /* May be a block or another if stmt. */
);

AST_DEFINE(loop,
    scope_t *scope;
    ast_t   *init;
    ast_t   *cond;
    ast_t   *post;
    ast_t   *block;
);

AST_DEFINE(defer,
    ast_t *block;
);

AST_DEFINE(return,
    ast_t *expr;
);

AST_DEFINE(break);
AST_DEFINE(continue);


typedef struct {
    scope_t    *scope;
    ast_decl_t *parent_decl;
    ast_decl_t *unit_decl;
    ast_proc_t *proc;
    array_t    *poly_constants;
    u32         poly_constants_idx;
    u32         flags;
    u32         varg_ty;
    u32         autocast_ty;
} check_context_t;

enum {
    CHECK_FLAG_DESCENDING     = (1 << 0),
    CHECK_FLAG_IN_LOOP        = (1 << 1),
    CHECK_FLAG_IN_PARAM       = (1 << 2),
    CHECK_FLAG_IN_VARGS       = (1 << 3),
    CHECK_FLAG_IN_DEFER       = (1 << 4),
    CHECK_FLAG_POLY_TYPE_ONLY = (1 << 5),
};

void check_all(void);

string_id value_to_string_id(value_t val, u32 type);
int tag_is_string(ast_t *tag_expr, string_id id);

#endif
