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
    X(TY_VARGS)      \
    X(_TY_TYPE_LIST) \
    X(TY_STRUCT)     \
    X(TY_PROC)

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
    X(TY_PTR)              \
    X(TY_VARGS)

#define X_INT_TYPES \
    X(TY_U8)        \
    X(TY_U16)       \
    X(TY_U32)       \
    X(TY_U64)       \
    X(TY_S8)        \
    X(TY_S16)       \
    X(TY_S32)       \
    X(TY_S64)

typedef struct type {
    u32 kind;
    union {
        u32 flags;
        u32 list_len;
    };
    union {
        /* Generic types with underlying types (like pointers or arrays) */
        struct {
            u32 under_id;
            u32 len;
        };
        /* Struct type */
        string_id name_id;
        /* Procedure types */
        struct {
            u32 param_list_id;
            u32 ret_id;
        };
        /* Param list types */
        u32 *id_list;


        u64 __64;
    };
} type_t;


int init_types(void);
int type_has_compile_time_only_values(u32 ty);
int type_kind_has_under(u32 kind);
int type_kind_is_int(u32 kind);
int type_kind(u32 ty);
u32 get_ptr_type(u32 ty);
u32 get_vargs_type(u32 ty);
u32 get_under_type(u32 ty);
u32 get_struct_type(ast_struct_t *st, string_id name_id, scope_t *scope);
u32 get_proc_type(u32 n_param_types, u32 *param_types, u32 ret_type);
u32 get_num_param_types(u32 proc_ty);
u32 get_param_type(u32 proc_ty, u32 idx);
u32 get_ret_type(u32 proc_ty);
string_id get_type_string_id(u32 ty);

#endif
