#include "hash_utilities.h"

u64 str_hash(const char *s) {
    unsigned long hash = 5381;
    int c;

    while ((c = *s++))
    hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}

int str_equ(const char *a, const char *b) { return strcmp(a, b) == 0; }
