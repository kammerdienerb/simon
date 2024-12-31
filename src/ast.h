#ifndef __AST_H__
#define __AST_H__

#include "internal.h"
#include "src_range.h"
#include "strings.h"
#include "array.h"

#define MAX_PARAMS_OR_ARGS (128)

#define ASTP(_x) (&((_x)->ast))

#define AST_DEFINE(name, ...) \
typedef struct {              \
    ast_t ast;                \
    __VA_ARGS__               \
} ast_##name##_t

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

typedef struct ast {
    u16          kind;
    u16          flags;
    u32          type;
    value_t      value;
    src_range_t  loc;
    struct ast  *macro_decl;
} ast_t;

AST_DEFINE(dummy);

#define X_AST                                            \
    X(AST_INVALID,                     dummy)            \
                                                         \
    X(AST_GLOBAL_SCOPE,                dummy)            \
                                                         \
    X(AST_MACRO_EXPAND_SCOPE,          dummy)            \
                                                         \
    X(AST_IGNORE_NODE,                 dummy)            \
                                                         \
    X(AST_COMPILE_ERROR,               compile_error)    \
                                                         \
    X(AST_VARGS_BLOCK,                 vargs_block)      \
                                                         \
    X(AST_BUILTIN,                     dummy)            \
                                                         \
    X(AST_MODULE,                      module)           \
    X(AST_PROC,                        proc)             \
    X(AST_STRUCT,                      struct)           \
    X(AST_MACRO,                       macro)            \
    X(AST_DECL_VAR,                    decl)             \
    X(AST_DECL_PROC,                   decl)             \
    X(AST_DECL_STRUCT,                 decl)             \
    X(AST_DECL_STRUCT_FIELD,           decl)             \
    X(AST_DECL_MACRO,                  decl)             \
    X(AST_DECL_MODULE,                 decl)             \
    X(AST_PARAM,                       param)            \
    X(AST_INT,                         int)              \
    X(AST_FLOAT,                       float)            \
    X(AST_STRING,                      string)           \
    X(AST_CHAR,                        char)             \
    X(AST_IDENT,                       ident)            \
    X(AST_PROC_TYPE,                   proc_type)        \
    X(AST_UNARY_EXPR,                  unary_expr)       \
    X(AST_BIN_EXPR,                    bin_expr)         \
    X(AST_BLOCK,                       block)            \
    X(AST_ARG_LIST,                    arg_list)         \
    X(AST_IF,                          if)               \
    X(AST_LOOP,                        loop)             \
    X(AST_RETURN,                      return)           \
    X(AST_DEFER,                       defer)            \
    X(AST_BREAK,                       break)            \
    X(AST_CONTINUE,                    continue)         \
    X(AST_MACRO_CALL,                  macro_call)       \
    X(AST_MACRO_ARG_EXPAND,            macro_arg_expand) \
    X(AST_MACRO_BLOCK_ARG_EXPAND,      macro_arg_expand) \
    X(AST_POLYMORPHIC_CONSTANT,        poly_constant)    \
    X(AST_POLYMORPHIC_CONSTANTS_SCOPE, dummy)

#define X_AST_DECLARATIONS             \
    X(AST_DECL_VAR)                    \
    X(AST_DECL_PROC)                   \
    X(AST_DECL_STRUCT)                 \
    X(AST_DECL_STRUCT_FIELD)           \
    X(AST_DECL_MACRO)                  \
    X(AST_DECL_MODULE)

#define X_AST_LEAF_EXPRS               \
    X(AST_INT)                         \
    X(AST_FLOAT)                       \
    X(AST_STRING)                      \
    X(AST_CHAR)                        \
    X(AST_IDENT)                       \
    X(AST_BLOCK)

#define X_AST_ALL_EXPRS                \
    X(AST_INT)                         \
    X(AST_FLOAT)                       \
    X(AST_STRING)                      \
    X(AST_CHAR)                        \
    X(AST_IDENT)                       \
    X(AST_UNARY_EXPR)                  \
    X(AST_BIN_EXPR)                    \
    X(AST_BLOCK)

#define X_AST_ALL_EXPRS_BUT_BLOCK      \
    X(AST_INT)                         \
    X(AST_FLOAT)                       \
    X(AST_STRING)                      \
    X(AST_CHAR)                        \
    X(AST_IDENT)                       \
    X(AST_UNARY_EXPR)                  \
    X(AST_BIN_EXPR)

#define X_AST_STATEMENTS               \
    X(AST_DECL_VAR)                    \
    X(AST_DECL_PROC)                   \
    X(AST_DECL_STRUCT)                 \
    X(AST_DECL_STRUCT_FIELD)           \
    X(AST_DECL_MACRO)                  \
    X(AST_DECL_MODULE)                 \
    X(AST_INT)                         \
    X(AST_FLOAT)                       \
    X(AST_STRING)                      \
    X(AST_CHAR)                        \
    X(AST_IDENT)                       \
    X(AST_UNARY_EXPR)                  \
    X(AST_BIN_EXPR)                    \
    X(AST_BLOCK)                       \
    X(AST_IF)                          \
    X(AST_LOOP)                        \
    X(AST_RETURN)                      \
    X(AST_DEFER)                       \
    X(AST_BREAK)                       \
    X(AST_CONTINUE)

#define X_AST_ONLY_STATEMENTS          \
    X(AST_IF)                          \
    X(AST_LOOP)                        \
    X(AST_RETURN)                      \
    X(AST_DEFER)                       \
    X(AST_BREAK)                       \
    X(AST_CONTINUE)

enum {
#define X(kind, name) kind,
X_AST
#undef X
};

enum {
    AST_FLAG_POLYMORPH                  = (1 <<  0),
    AST_FLAG_MONOMORPH                  = (1 <<  1),
    AST_FLAG_SYNTHETIC_BLOCK            = (1 <<  2),
    AST_FLAG_POLY_VARARGS               = (1 <<  3),
    AST_FLAG_CF_MUST_RETURN             = (1 <<  4),
    AST_FLAG_CF_MUST_SKIP_LOOP_BODY     = (1 <<  5),
    AST_FLAG_VISITED                    = (1 <<  6),
    AST_FLAG_IS_EXTERN                  = (1 <<  7),
    AST_FLAG_EXPR_TOP                   = (1 <<  8),
    AST_FLAG_EXPR_CAN_BE_LVAL           = (1 <<  9),
    AST_FLAG_VISIT_WORK_DONE            = (1 << 10),
    AST_FLAG_BITFIELD_DOT               = (1 << 11),
    AST_FLAG_NAME_IN_MACRO              = (1 << 12),
    AST_FLAG_CONSTANT                   = (1 << 13),
    AST_FLAG_POLY_IDENT                 = (1 << 14),
    AST_FLAG_CHECKED                    = (1 << 15),
};

enum {
    MACRO_EXPR,
    MACRO_STMT,
    MACRO_DECL,
};


string_id value_to_string_id(value_t val, u32 type);

int ast_kind_is_decl(int kind);
const char *ast_get_kind_str(int kind);
#define AST_STR(kind) (ast_get_kind_str((kind)))
extern u64 ast_size_table[];

typedef struct {
    array_t  constants;
    ast_t   *node;
    ast_t   *specialization;
    u32      type;
} monomorphed_t;

typedef struct {
    ast_t   *node;
    value_t  value;
    u32      has_value;
    u32      type;
} poly_arg_t;


struct scope;
typedef struct scope scope_t;


AST_DEFINE(scoped,
    scope_t *scope;
);

AST_DEFINE(poly_constant,
    string_id name;
);

AST_DEFINE(builtin,
    string_id name;
);

AST_DEFINE(decl,
    scope_t     *containing_scope;
    string_id    name;
    string_id    full_name;
    src_point_t  name_end;
    ast_t       *type_expr;
    ast_t       *val_expr;
    ast_t       *poly_constant_dependant;
    array_t      tags;
    /* For bitfield struct field. */
    u64          bitfield_mask;
    u32          bitfield_shift;
);

AST_DEFINE(module,
    scope_t    *scope;
    ast_decl_t *parent_decl;
    array_t     children;
);

AST_DEFINE(param,
    string_id    name;
    ast_t       *type_expr;
    ast_t       *val;
    scope_t     *containing_scope;
    src_point_t  name_end;
);

AST_DEFINE(block,
    scope_t     *scope;
    array_t      stmts;
    src_point_t  end_brace_loc;
);

AST_DEFINE(proc,
    scope_t     *scope;
    ast_decl_t  *parent_decl;
    src_range_t  params_loc;
    array_t      params;
    ast_t       *ret_type_expr;
    ast_t       *block;
    array_t      monomorphs;
    int          mono_idx;
);

AST_DEFINE(struct,
    scope_t     *scope;
    ast_decl_t  *parent_decl;
    src_range_t  params_loc;
    array_t      params;
    array_t      fields;
    array_t      children;
    array_t      monomorphs;
    u8           bitfield_struct_bits;
    int          mono_idx;
);

AST_DEFINE(macro,
    ast_decl_t *parent_decl;
    ast_t      *block;
    array_t     param_names;
    int         is_block_macro;
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

AST_DEFINE(proc_type,
    array_t  param_type_exprs;
    ast_t   *ret_type_expr;
);

AST_DEFINE(unary_expr,
    ast_t *child;
    int    op;
);

AST_DEFINE(bin_expr,
    ast_t       *left;
    ast_t       *right;
    src_point_t  op_loc;
    ast_t       *call_decl;
    int          op;
);

AST_DEFINE(ident,
    string_id  str_rep;
    ast_t     *resolved_node;
    int        mono_idx;
    int        varg_idx;
    char       _padding[   sizeof(ast_bin_expr_t)
                         - sizeof(ast_t)
                         - sizeof(string_id)
                         - sizeof(ast_t*)
                         - sizeof(int)
                         - sizeof(int)
                       ];
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

AST_DEFINE(macro_call,
    ast_t   *ident;
    ast_t   *arg_list;
    ast_t   *block;
    scope_t *scope;
    int      expected_kind;
    char     _padding[  sizeof(ast_decl_t)
                      - sizeof(ast_t)
                      - sizeof(ast_t*)
                      - sizeof(ast_t*)
                      - sizeof(ast_t*)
                      - sizeof(scope_t*)
                      - sizeof(int)
                     ];
);

AST_DEFINE(macro_arg_expand,
    string_id name;
    char      _padding[sizeof(ast_decl_t) - sizeof(ast_t) - sizeof(string_id)];
);

AST_DEFINE(compile_error,
    string_id message;
);

AST_DEFINE(vargs_block,
    ast_t   *block;
    array_t  new_blocks;
);


typedef struct {
    scope_t    *scope;
    ast_decl_t *parent_decl;
    ast_decl_t *unit_decl;
    ast_proc_t *proc;
    u32         monomorph_idx;
    u32         flags;
    u32         varg_ty;
} check_context_t;

enum {
    CHECK_FLAG_DESCENDING           = (1 <<  0),
    CHECK_FLAG_IN_LOOP              = (1 <<  1),
    CHECK_FLAG_IN_PARAM             = (1 <<  2),
    CHECK_FLAG_IN_VARGS             = (1 <<  3),
    CHECK_FLAG_IN_DEFER             = (1 <<  4),
    CHECK_FLAG_DEFER_IN_LOOP        = (1 <<  5),
    CHECK_FLAG_POLY_PROC_TYPE_ONLY  = (1 <<  6),
    CHECK_FLAG_FORCE_RECHECK        = (1 <<  7),
    CHECK_FLAG_ALLOW_REF_POLY_PROC  = (1 <<  8),
    CHECK_FLAG_MONOMORPH            = (1 <<  9),
    CHECK_FLAG_POLY_BACKLOG         = (1 << 10),
    CHECK_FLAG_SPECIALIZATION       = (1 << 11),
};

void expand_macro(ast_macro_call_t *call);

void check_all(void);

u32 find_poly_vargs_type(scope_t *scope);
string_id value_to_string_id(value_t val, u32 type);
int tag_is_string(ast_t *tag_expr, string_id id);
void init_tags(void);

#endif
