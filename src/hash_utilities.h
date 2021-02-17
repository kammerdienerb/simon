#ifndef __HASH_UTILITIES_H__
#define __HASH_UTILITIES_H__

#include "internal.h"
#include "strings.h"

u64 str_hash(const char *s);
int str_equ(const char *a, const char *b);

u64 str_id_hash(string_id id);

#endif
