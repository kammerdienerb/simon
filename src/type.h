#ifndef __TYPE_H__
#define __TYPE_H__

#include "internal.h"
#include "strings.h"
#include "ast.h"

#define X_TYPES      \
    X(TY_UNKNOWN)    \
    X(TY_NOT_TYPED)  \
    X(TY_MODULE)     \
    X(TY_MACRO)      \
    X(TY_TYPE)       \
    X(TY_PROC)       \
    X(TY_BOOL)       \
    X(TY_CHAR)       \
    X(TY_U8)         \
    X(TY_U16)        \
    X(TY_U32)        \
    X(TY_U64)        \
    X(TY_S8)         \
    X(TY_S16)        \
    X(TY_S32)        \
    X(TY_S64)        \
    X(TY_PTR)        \
    X(TY_STRUCT)

enum {
#define X(ty) ty,
    X_TYPES
#undef X
};

#define TY_NONE (TY_UNKNOWN)

#define X_CT_TYPES \
    X(TY_MODULE)   \
    X(TY_MACRO)    \
    X(TY_TYPE)     \
    X(TY_PROC)

#define X_REAL_BUILTIN_TYPES \
    X(TY_UNKNOWN)            \
    X(TY_NOT_TYPED)          \
    X(TY_MODULE)             \
    X(TY_MACRO)              \
    X(TY_TYPE)               \
    X(TY_PROC)               \
    X(TY_BOOL)               \
    X(TY_CHAR)               \
    X(TY_U8)                 \
    X(TY_U16)                \
    X(TY_U32)                \
    X(TY_U64)                \
    X(TY_S8)                 \
    X(TY_S16)                \
    X(TY_S32)                \
    X(TY_S64)

#define X_HAVE_UNDER_TYPES \
    X(TY_PTR)

typedef struct type {
    u32 kind;
    u32 flags;
    union {
        struct {
            u32 under_id;
            u32 len;
        };
        string_id name_id;
        u64       __64;
    };
} type_t;


int init_types(void);
int type_has_compile_time_only_values(u32 ty);
u32 get_ptr_type(u32 ty, u32 flags);
u32 get_struct_type(ast_struct_t *st, string_id name_id, scope_t *scope, u32 flags);
string_id get_type_string_id(u32 ty);

#endif
