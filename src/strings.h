#ifndef __STRINGS_H__
#define __STRINGS_H__

#include "internal.h"
#include "array.h"


#define STRING_HASH_TABLE (1)
#define STRING_RB_TREE    (2)

#ifndef STRING_INTERN_STRUCTURE
#define STRING_INTERN_STRUCTURE STRING_HASH_TABLE
#endif

typedef const char *string_t;
typedef u64         string_id;

#define STRING_ID_NULL (0)
extern string_id UNDERSCORE_ID;
extern string_id __BUILTIN_PRINTS_ID;
extern string_id __BUILTIN_PRINTP_ID;
extern string_id __BUILTIN_PRINTI_ID;
extern string_id __BUILTIN_STACK_ALLOC_ID;
extern string_id CAST_ID;

void        init_strings(void);
string_id   get_string_id(const char *string);
string_id   get_string_id_n(const char *string, u64 len);
const char *get_string(string_id id);
int         is_kwd(string_id id);

void print_all_strings(void);

array_t sh_split(char *s);

#endif
