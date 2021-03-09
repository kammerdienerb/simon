#include "type.h"
#include "array.h"
#include "scope.h"
#include "ui.h"

static array_t type_table;

static int type_kind_has_under(u32 kind) {
    return
#define X(k) kind == (k) ||
    X_HAVE_UNDER_TYPES
#undef X
    0;
}

static u32 insert_new_type(type_t t) {
    u32 id;
    array_push(type_table, t);
    id = array_len(type_table) - 1;
    return id;
}

static u32 get_or_insert_type(type_t t) {
    u32     id;
    type_t *it;

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

    X_REAL_BUILTIN_TYPES
#undef X

    /* Add an empty type list type */
    t.kind     = _TY_TYPE_LIST;
    t.list_len = 0;
    t.id_list  = NULL;
    insert_new_type(t);

    return 0;
}

int type_has_compile_time_only_values(u32 ty) {
    return
#define X(t) ty == (t) ||
    X_CT_TYPES
#undef X
    0;
}

static type_t * get_type_structure(u32 ty) {
    ASSERT(ty < array_len(type_table), "invalid type id");

    return array_item(type_table, ty);
}

u32 get_ptr_type(u32 ty, u32 flags) {
    type_t new_t;

    new_t.kind     = TY_PTR;
    new_t.flags    = flags;
    new_t.under_id = ty;
    new_t.len      = 0;

    return get_or_insert_type(new_t);
}

u32 get_struct_type(ast_struct_t *st, string_id name_id, scope_t *scope, u32 flags) {
    type_t      t;
    const char *scope_name;
    const char *name;
    char        buff[SCOPE_NAME_BUFF_SIZE];

    t.kind  = TY_STRUCT;
    t.flags = flags;

    if (scope->parent == NULL) {
        t.name_id = name_id;
    } else {
        scope_name = get_string(scope->name_id);
        name       = get_string(name_id);

        if (strlen(scope_name) + strlen(name) + 2 > SCOPE_NAME_BUFF_SIZE) {
            report_vague_err("INTERNAL ERROR: name too long");
            ASSERT(0, "name too long");
        }

        strncpy(buff, scope_name, SCOPE_NAME_BUFF_SIZE - strlen(buff) - 1);
        strncat(buff, ".", SCOPE_NAME_BUFF_SIZE - strlen(buff) - 1);
        strncat(buff, name, SCOPE_NAME_BUFF_SIZE - strlen(buff) - 1);

        t.name_id = get_string_id(buff);
    }

    return get_or_insert_type(t);
}

#define TYPE_STRING_BUFF_SIZE (4096)

static void build_type_string(u32 ty, char *buff) {
    type_t *tp;
    type_t  t;
    char    under_buff[TYPE_STRING_BUFF_SIZE];

    buff[0] = 0;

    tp = get_type_structure(ty);

    if (tp == NULL) {
        strncat(buff, "<invalid type>", TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
        return;
    }

    t = *tp;

    switch (t.kind) {
        case TY_UNKNOWN:   strncat(buff, "<unknown type>",      TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_NOT_TYPED: strncat(buff, "<not typed>",         TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_MODULE:    strncat(buff, "module",              TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_MACRO:     strncat(buff, "macro",               TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_TYPE:      strncat(buff, "type",                TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_PROC:      strncat(buff, "procedure",           TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_PTR:       strncat(buff, "pointer",             TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_BOOL:      strncat(buff, "bool",                TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_CHAR:      strncat(buff, "char",                TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_U8:        strncat(buff, "u8",                  TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_U16:       strncat(buff, "u16",                 TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_U32:       strncat(buff, "u32",                 TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_U64:       strncat(buff, "u64",                 TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_S8:        strncat(buff, "s8",                  TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_S16:       strncat(buff, "s16",                 TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_S32:       strncat(buff, "s32",                 TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_S64:       strncat(buff, "s64",                 TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        case TY_STRUCT:    strncat(buff, get_string(t.name_id), TYPE_STRING_BUFF_SIZE - strlen(buff) - 1); break;
        default:
            ASSERT(0, "unhandled type kind in build_type_string()");
            return;
    }

    if (type_kind_has_under(t.kind)) {
        strncat(buff, "(", TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
        build_type_string(t.under_id, under_buff);
        strncat(buff, under_buff, TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
        strncat(buff, ")", TYPE_STRING_BUFF_SIZE - strlen(buff) - 1);
    }
}
string_id get_type_string_id(u32 ty) {
    char buff[TYPE_STRING_BUFF_SIZE];

    build_type_string(ty, buff);

    return get_string_id(buff);
}
