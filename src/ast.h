#ifndef __AST_H__
#define __AST_H__

#include "internal.h"
#include "src_range.h"
#include "strings.h"

enum {
    AST_ASSIGN,
};

typedef struct {
    int         kind;
    src_range_t loc;
} ast_t;

#define AST_DEFINE(name, ...) \
typedef struct {              \
    ast_t ast;                \
    __VA_ARGS__               \
} ast_##name##_t

AST_DEFINE(assign,
    string_id name;
);

#endif
