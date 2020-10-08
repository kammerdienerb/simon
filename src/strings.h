#ifndef __STRINGS_H__
#define __STRINGS_H__

#include "internal.h"

typedef u64 string_id;

void        init_strings(void);
string_id   get_string_id(const char *string);
string_id   get_string_id_n(const char *string, u64 len);
const char *get_string(string_id id);

#endif
