#ifndef __TYPE_H__
#define __TYPE_H__

#include "internal.h"
#include "strings.h"
#include "ast.h"

/*
 * TY_GENERIC_POSITIVE_INT and TY_GENERIC_NEGATIVE_INT are type values.
 * TY_GENERIC_INT is a type kind.
 * TY_GENERIC_FLOAT can be either.
 */

#define X_TYPES                \
    X(TY_UNKNOWN)              \
    X(TY_NOT_TYPED)            \
    X(TY_BUILTIN_SPECIAL)      \
    X(TY_MODULE)               \
    X(TY_MACRO)                \
    X(TY_TYPE)                 \
    X(TY_U8)                   \
    X(TY_U16)                  \
    X(TY_U32)                  \
    X(TY_U64)                  \
    X(TY_S8)                   \
    X(TY_S16)                  \
    X(TY_S32)                  \
    X(TY_S64)                  \
    X(TY_F32)                  \
    X(TY_F64)                  \
    X(TY_PTR)                  \
    X(TY_STR)                  \
    X(TY_GENERIC_POSITIVE_INT) \
    X(TY_GENERIC_NEGATIVE_INT) \
    X(TY_GENERIC_INT)          \
    X(TY_GENERIC_FLOAT)        \
    X(TY_VARGS)                \
    X(_TY_TYPE_LIST)           \
    X(TY_STRUCT)               \
    X(TY_STRUCT_MONO)          \
    X(TY_PROC)                 \
    X(TY_POLY)

#define TYPE_IS_GENERIC(_t)            \
      ((_t) == TY_GENERIC_INT          \
    || (_t) == TY_GENERIC_POSITIVE_INT \
    || (_t) == TY_GENERIC_NEGATIVE_INT \
    || (_t) == TY_GENERIC_FLOAT)

#define INT_TYPE_IS_SIGNED(_t)         \
       ((_t) == TY_S64                 \
    ||  (_t) == TY_S32                 \
    ||  (_t) == TY_S16                 \
    ||  (_t) == TY_S8)

#define TKINDPAIR_INT_INT ((((u64)TY_GENERIC_INT)   << 32ULL) + TY_GENERIC_INT)
#define TKINDPAIR_PTR_PTR ((((u64)TY_PTR)           << 32ULL) + TY_PTR)
#define TKINDPAIR_PTR_INT ((((u64)TY_PTR)           << 32ULL) + TY_GENERIC_INT)
#define TKINDPAIR_FLT_FLT ((((u64)TY_GENERIC_FLOAT) << 32ULL) + TY_GENERIC_FLOAT)
#define TKINDPAIR_FLT_INT ((((u64)TY_GENERIC_FLOAT) << 32ULL) + TY_GENERIC_INT)

#define TY_INT_PTR (TY_U64)

enum {
#define X(ty) ty,
    X_TYPES
#undef X
};

#define TY_NONE (TY_UNKNOWN)

#define X_CT_TYPES             \
    X(TY_MODULE)               \
    X(TY_MACRO)                \
    X(TY_TYPE)                 \
    X(TY_PROC)

#define X_HAVE_UNDER_TYPES     \
    X(TY_PTR)                  \
    X(TY_VARGS)

#define X_INT_TYPES            \
    X(TY_GENERIC_POSITIVE_INT) \
    X(TY_GENERIC_NEGATIVE_INT) \
    X(TY_U8)                   \
    X(TY_U16)                  \
    X(TY_U32)                  \
    X(TY_U64)                  \
    X(TY_S8)                   \
    X(TY_S16)                  \
    X(TY_S32)                  \
    X(TY_S64)

#define X_FLOAT_TYPES          \
    X(TY_GENERIC_FLOAT)        \
    X(TY_F32)                  \
    X(TY_F64)

#define X_NUM_TYPES            \
    X(TY_GENERIC_POSITIVE_INT) \
    X(TY_GENERIC_NEGATIVE_INT) \
    X(TY_U8)                   \
    X(TY_U16)                  \
    X(TY_U32)                  \
    X(TY_U64)                  \
    X(TY_S8)                   \
    X(TY_S16)                  \
    X(TY_S32)                  \
    X(TY_S64)                  \
    X(TY_F32)                  \
    X(TY_F64)

enum {
    TY_FLAG_IS_POLY = (1),
};

typedef struct type {
    u32 kind;
    union {
        u32 flags;
        u32 list_len;
        u32 mono_constants_idx;
    };
    union {
        /* Generic types with underlying types (like pointers or arrays) */
        struct {
            u32 under_id;
            u32 _pad;
        };
        /* Struct type */
        ast_decl_t *decl;
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
void report_type_stats(void);
int type_is_poly(u32 ty);
int type_has_compile_time_only_values(u32 ty);
int type_kind_has_under(u32 kind);
int type_kind_is_int(u32 kind);
int type_kind_is_float(u32 kind);
int type_kind_is_numeric(u32 kind);
int type_kind(u32 ty);
u32 get_ptr_type(u32 ty);
u32 get_vargs_type(u32 ty);
u32 get_under_type(u32 ty);
u32 get_struct_type(ast_decl_t *st);
u32 get_struct_mono_type(ast_decl_t *st, u32 constants_idx);
u32 get_struct_field_type(u32 ty, string_id field_name);
u32 get_type_list_type(u32 n_types, u32 *types);
u32 get_proc_type(u32 n_param_types, u32 *param_types, u32 ret_type);
u32 get_num_param_types(u32 proc_ty);
u32 get_param_type(u32 proc_ty, u32 idx);
u32 get_ret_type(u32 proc_ty);
string_id get_type_string_id(u32 ty);
ast_decl_t *struct_type_to_decl(u32 ty);
type_t get_type_t(u32 ty);

int  types_are_compatible(u32 ta, u32 tb);
void realize_generic(u32 real, ast_t *expr);
void force_generic_realization(ast_t *expr);

#endif
