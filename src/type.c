#include "type.h"
#include "array.h"
#include "scope.h"
#include "ui.h"
#include "memory.h"

static array_t type_table;
static u32     empty_type_list;

void report_type_stats(void) {
    printf("%d total types in the type table\n", array_len(type_table));
}

static type_t * get_type_structure(u32 ty) {
    ASSERT(ty < array_len(type_table), "invalid type id");

    return array_item(type_table, ty);
}

int type_is_poly(u32 ty) {
    type_t *t;
    int     i;

    if (ty == TY_POLY) { return 1; }

    t = get_type_structure(ty);

    if (t->kind == _TY_TYPE_LIST) {
        for (i = 0; i < t->list_len; i += 1) {
            if (type_is_poly(t->id_list[i])) {
                return 1;
            }
        }
    } else if (t->kind == TY_STRUCT_MONO) {
        return 0;
    } else if (t->flags & TY_FLAG_IS_POLY) {
        return 1;
    }

    return 0;
}

int type_kind_has_under(u32 kind) {
    return
#define X(k) kind == (k) ||
    X_HAVE_UNDER_TYPES
#undef X
    0;
}

int type_kind_is_int(u32 kind) {
    return kind == TY_GENERIC_INT;
}

int type_kind_is_float(u32 kind) {
    return kind == TY_GENERIC_FLOAT;
}

int type_kind_is_numeric(u32 kind) {
    return kind == TY_GENERIC_INT || kind == TY_GENERIC_FLOAT;
}

static u32 insert_new_type(type_t t) {
    u32  id;
    u32 *id_list;

    if (t.kind == _TY_TYPE_LIST) {
        id_list = mem_alloc(sizeof(u32) * t.list_len);
        memcpy(id_list, t.id_list, sizeof(u32) * t.list_len);
        t.id_list = id_list;
    }

    array_push(type_table, t);
    id = array_len(type_table) - 1;
    return id;
}

static u32 get_or_insert_type_list(type_t t) {
    u32     id;
    type_t *it;
    int     i;

    if (t.list_len == 0) {
        return empty_type_list;
    }

    id = *t.id_list;

    array_traverse_from(type_table, it, id) {
        if (it->kind     == _TY_TYPE_LIST
        &&  it->list_len == t.list_len) {
            for (i = 0; i < t.list_len; i += 1) {
                if (it->id_list[i] != t.id_list[i]) { goto next; }
            }

            return id;

            next:;
        }
        id += 1;
    }

    id = insert_new_type(t);

    return id;
}

static u32 get_or_insert_type(type_t t) {
    u32     id;
    type_t *it;

    if (t.kind == _TY_TYPE_LIST) {
        return get_or_insert_type_list(t);
    }

    if (type_kind_has_under(t.kind)) {
        id = t.under_id + 1;
    } else {
        id = 0;
    }

    array_traverse_from(type_table, it, id) {
        if (memcmp(&t, it, sizeof(t)) == 0) { return id; }
        id += 1;
    }

    id = insert_new_type(t);

    return id;
}

int init_types(void) {
    type_t t;
    u32    id;


    ASSERT(sizeof(type_t) == 16, "type_t should be 16 bytes");

    type_table = array_make(type_t);

#define X(ty)                     \
t.kind     = (ty);                \
t.flags    = 0;                   \
t.__64     = 0;                   \
id         = insert_new_type(t);  \
ASSERT(id == (ty), "id != " #ty);

    X_TYPES
#undef X

    /* Add an empty type list type */
    t.kind          = _TY_TYPE_LIST;
    t.list_len      = 0;
    t.id_list       = NULL;
    empty_type_list = insert_new_type(t);

    get_type_structure(TY_POLY)->flags |= TY_FLAG_IS_POLY;

    return 0;
}

int type_has_compile_time_only_values(u32 ty) {
    return
#define X(t) ty == (t) ||
    X_CT_TYPES
#undef X
    0;
}

int type_kind(u32 ty) {
    u32 kind;

    kind = get_type_structure(ty)->kind;

    switch (kind) {
#define X(t) case t:
        X_INT_TYPES
#undef X
            kind = TY_GENERIC_INT;
            break;

#define X(t) case t:
        X_FLOAT_TYPES
#undef X
            kind = TY_GENERIC_FLOAT;
            break;
    }

    return kind;
}

u32 get_ptr_type(u32 ty) {
    type_t new_t;

    new_t.kind     = TY_PTR;
    new_t.flags    = type_is_poly(ty) ? TY_FLAG_IS_POLY : 0;
    new_t.under_id = ty;
    new_t._pad     = 0;

    return get_or_insert_type(new_t);
}

u32 get_vargs_type(u32 ty) {
    type_t new_t;

    new_t.kind     = TY_VARGS;
    new_t.flags    = type_is_poly(ty) ? TY_FLAG_IS_POLY : 0;
    new_t.under_id = ty;
    new_t._pad     = 0;

    return get_or_insert_type(new_t);
}

u32 get_under_type(u32 ty) {
    ASSERT(type_kind_has_under(type_kind(ty)), "ty does not have underlying type");

    return get_type_structure(ty)->under_id;
}

u32 get_struct_type(ast_decl_t *st) {
    type_t t;

    ASSERT(ASTP(st)->kind == AST_DECL_STRUCT, "not a struct declaration");

    t.kind  = TY_STRUCT;
    t.flags = ASTP(st)->flags & AST_FLAG_POLYMORPH ? TY_FLAG_IS_POLY : 0;
    t.decl  = st;

    return get_or_insert_type(t);
}

u32 get_struct_mono_type(ast_decl_t *st, u32 constants_idx) {
    type_t t;

    ASSERT(ASTP(st)->kind == AST_DECL_STRUCT, "not a struct declaration");

    t.kind               = TY_STRUCT_MONO;
    t.mono_constants_idx = constants_idx;
    t.decl               = st;

    return get_or_insert_type(t);
}

u32 get_struct_field_type(u32 ty, string_id field_name) {
    type_t              *t;
    ast_struct_t        *st;
    ast_t              **it;
    ast_struct_field_t  *field;

    t = get_type_structure(ty);
    ASSERT(t != NULL,            "did not find type");
    ASSERT(t->kind == TY_STRUCT, "type is not a struct");

    st = (ast_struct_t*)t->decl->val_expr;

    array_traverse(st->fields, it) {
        field = (ast_struct_field_t*)*it;
        if (field->name == field_name) {
            return ASTP(field)->type;
        }
    }

    return TY_UNKNOWN;
}

u32 get_type_list_type(u32 n_types, u32 *types) {
    type_t list_t;

    list_t.kind     = _TY_TYPE_LIST;
    list_t.list_len = n_types;
    list_t.id_list  = types;

    return get_or_insert_type(list_t);
}

u32 get_proc_type(u32 n_param_types, u32 *param_types, u32 ret_type) {
    type_t params_t;
    u32    params_type;
    type_t t;

    params_t.kind     = _TY_TYPE_LIST;
    params_t.list_len = n_param_types;
    params_t.id_list  = param_types;

    params_type = get_or_insert_type(params_t);

    t.kind          = TY_PROC;
    t.flags         = type_is_poly(params_type) || type_is_poly(ret_type) ? TY_FLAG_IS_POLY : 0;
    t.param_list_id = params_type;
    t.ret_id        = ret_type;

    return get_or_insert_type(t);
}

u32 get_num_param_types(u32 proc_ty) {
    type_t *t;
    type_t *params_t;

    t = get_type_structure(proc_ty);
    ASSERT(t->kind == TY_PROC, "type is not TY_PROC");
    params_t = get_type_structure(t->param_list_id);

    return params_t->list_len;
}

u32 get_param_type(u32 proc_ty, u32 idx) {
    type_t *t;
    type_t *params_t;

    t = get_type_structure(proc_ty);
    ASSERT(t->kind == TY_PROC, "type is not TY_PROC");
    params_t = get_type_structure(t->param_list_id);

    if (idx >= params_t->list_len) { return TY_UNKNOWN; }

    return params_t->id_list[idx];
}

u32 get_ret_type(u32 proc_ty) {
    type_t *t;

    t = get_type_structure(proc_ty);
    ASSERT(t->kind == TY_PROC, "type is not TY_PROC");

    return t->ret_id;
}

#define TYPE_STRING_BUFF_SIZE (4096)

static void build_type_string(u32 ty, char *buff) {
    type_t               *tp;
    type_t                t;
    polymorphed_t        *poly;
    polymorph_constant_t *it;
    const char           *lazy_comma;
    char                  under_buff[TYPE_STRING_BUFF_SIZE];
    int                   i;

    buff[0] = 0;

    tp = get_type_structure(ty);

    if (tp == NULL) {
        strncat(buff, "<invalid type>", TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
        return;
    }

    t = *tp;

    switch (t.kind) {
        case TY_UNKNOWN:     strncat(buff, "<unknown type>",              TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_NOT_TYPED:   strncat(buff, "<not typed>",                 TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_MODULE:      strncat(buff, "module",                      TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_MACRO:       strncat(buff, "macro",                       TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_TYPE:        strncat(buff, "type",                        TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_PROC:        strncat(buff, "procedure",                   TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_PTR:         strncat(buff, "*",                           TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_STR:         strncat(buff, "str",                         TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_VARGS:       strncat(buff, "...",                         TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_U8:          strncat(buff, "u8",                          TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_U16:         strncat(buff, "u16",                         TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_U32:         strncat(buff, "u32",                         TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_U64:         strncat(buff, "u64",                         TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_S8:          strncat(buff, "s8",                          TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_S16:         strncat(buff, "s16",                         TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_S32:         strncat(buff, "s32",                         TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_S64:         strncat(buff, "s64",                         TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_F32:         strncat(buff, "f32",                         TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_F64:         strncat(buff, "f64",                         TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_STRUCT:      strncat(buff, get_string(t.decl->full_name), TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_STRUCT_MONO: {
            strncat(buff, get_string(t.decl->full_name), TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
            strncat(buff, "(", TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
            lazy_comma = "";
            poly = array_item(((ast_struct_t*)t.decl->val_expr)->polymorphs, t.mono_constants_idx);
            array_traverse(poly->constants, it) {
                strncat(buff, lazy_comma, TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
                strncat(buff, get_string(it->name), TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
                strncat(buff, ": ", TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
                strncat(buff, get_string(value_to_string_id(it->value, it->type)), TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
                lazy_comma = ", ";

            }
            strncat(buff, ")", TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
            break;
        }
        case TY_POLY:        strncat(buff, "<polymorphic>",               TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case _TY_TYPE_LIST:  break; /* Handled below. */
        default:
            ASSERT(0, "unhandled type kind in build_type_string()");
            return;
    }

    if (type_kind_has_under(t.kind)) {
        build_type_string(t.under_id, under_buff);
        strncat(buff, under_buff, TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
    } else if (t.kind == TY_PROC) {
        build_type_string(t.under_id, under_buff);
        strncat(buff, under_buff, TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
        if (t.ret_id != TY_NOT_TYPED) {
            strncat(buff, ": ", TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
            build_type_string(t.ret_id, under_buff);
            strncat(buff, under_buff, TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
        }
    } else if (t.kind == _TY_TYPE_LIST) {
        strncat(buff, "(", TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
        lazy_comma = "";
        for (i = 0; i < t.list_len; i += 1) {
            strncat(buff, lazy_comma, TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
            build_type_string(t.id_list[i], under_buff);
            strncat(buff, under_buff, TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
            lazy_comma = ", ";
        }
        strncat(buff, ")", TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
    }
}
string_id get_type_string_id(u32 ty) {
    char buff[TYPE_STRING_BUFF_SIZE];

    build_type_string(ty, buff);

    return get_string_id(buff);
}

ast_decl_t *struct_type_to_decl(u32 ty) {
    type_t *t;

    t = get_type_structure(ty);

    return t->decl;
}

type_t get_type_t(u32 ty) {
    type_t *t;

    t = get_type_structure(ty);
    ASSERT(t != NULL, "did not find type");

    return *t;
}
