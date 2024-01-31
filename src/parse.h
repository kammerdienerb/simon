#ifndef __PARSE_H__
#define __PARSE_H__

#include "internal.h"

#define OP_ASSOC_LEFT  (1)
#define OP_ASSOC_RIGHT (2)

#define X_OPS                                                                \
    /* op              prec,    assoc,            arity,  match,    str */    \
    X(OP_INVALID,      0,       0,                0,      NULL,     NULL)     \
    X(OP_CALL,         12,      OP_ASSOC_LEFT,    2,      "(",      "()")     \
    X(OP_DOT,          12,      OP_ASSOC_LEFT,    2,      ".",      ".")      \
    X(OP_SUBSCRIPT,    12,      OP_ASSOC_LEFT,    2,      "[",      "[]")     \
    X(OP_ASSIGN,       1,       OP_ASSOC_RIGHT,   2,      "=",      "=")      \
    X(OP_PLUS_ASSIGN,  1,       OP_ASSOC_RIGHT,   2,      "+=",     "+=")     \
    X(OP_MINUS_ASSIGN, 1,       OP_ASSOC_RIGHT,   2,      "-=",     "-=")     \
    X(OP_MULT_ASSIGN,  1,       OP_ASSOC_RIGHT,   2,      "*=",     "*=")     \
    X(OP_DIV_ASSIGN,   1,       OP_ASSOC_RIGHT,   2,      "/=",     "/=")     \
    X(OP_MOD_ASSIGN,   1,       OP_ASSOC_RIGHT,   2,      "%=",     "%=")     \
    X(OP_PLUS,         9,       OP_ASSOC_LEFT,    2,      "+",      "+")      \
    X(OP_MINUS,        9,       OP_ASSOC_LEFT,    2,      "-",      "-")      \
    X(OP_MULT,         10,      OP_ASSOC_LEFT,    2,      "*",      "*")      \
    X(OP_DIV,          10,      OP_ASSOC_LEFT,    2,      "/",      "/")      \
    X(OP_MOD,          10,      OP_ASSOC_LEFT,    2,      "%",      "%")      \
    X(OP_LEQ,          7,       OP_ASSOC_LEFT,    2,      "<=",     "<=")     \
    X(OP_GEQ,          7,       OP_ASSOC_LEFT,    2,      ">=",     ">=")     \
    X(OP_LSS,          7,       OP_ASSOC_LEFT,    2,      "<",      "<")      \
    X(OP_GTR,          7,       OP_ASSOC_LEFT,    2,      ">",      ">")      \
    X(OP_EQU,          6,       OP_ASSOC_LEFT,    2,      "==",     "==")     \
    X(OP_NEQ,          6,       OP_ASSOC_LEFT,    2,      "!=",     "!=")     \
    X(OP_AND,          2,       OP_ASSOC_LEFT,    2,      "and",    "and")    \
    X(OP_OR,           2,       OP_ASSOC_LEFT,    2,      "or",     "or")     \
    X(OP_BSHL,         8,       OP_ASSOC_LEFT,    2,      "<<",     "<<")     \
    X(OP_BSHR,         8,       OP_ASSOC_LEFT,    2,      ">>",     ">>")     \
    X(OP_BAND,         5,       OP_ASSOC_LEFT,    2,      "&",      "&")      \
    X(OP_BXOR,         4,       OP_ASSOC_LEFT,    2,      "^",      "^")      \
    X(OP_BOR,          3,       OP_ASSOC_LEFT,    2,      "|",      "|")      \
    X(OP_NOT,          11,      OP_ASSOC_RIGHT,   1,      "not",    "not")    \
    X(OP_SLICE,        11,      OP_ASSOC_RIGHT,   1,      "[",      "[]")     \
    X(OP_ADDR,         11,      OP_ASSOC_RIGHT,   1,      "*",      "*")      \
    X(OP_DEREF,        11,      OP_ASSOC_RIGHT,   1,      "@",      "@")      \
    X(OP_NEG,          11,      OP_ASSOC_RIGHT,   1,      "-",      "-")      \
    X(OP_BNEG,         11,      OP_ASSOC_RIGHT,   1,      "~",      "~")      \
    X(OP_SIZEOF,       11,      OP_ASSOC_RIGHT,   1,      "sizeof", "sizeof") \
    X(OP_TYPEOF,       11,      OP_ASSOC_RIGHT,   1,      "typeof", "typeof") \
    X(OP_LENOF,        11,      OP_ASSOC_RIGHT,   1,      "lenof",  "lenof")

enum {
#define X(_op, _prec, _assoc, _arity, _match, _str) _op,
    X_OPS
#undef X
    N_OPS,
};


void parse_file(const char *path);
void start_parsing_file_async(const char *path);
void start_parsing_file(const char *path);
void wait_for_parsing_async(void);

extern u32         op_prec_table[];
extern int         op_assoc_table[];
extern int         op_arity_table[];
extern const char *op_str_table[];

#define OP_PREC(_op)          (op_prec_table[(_op)])
#define OP_ASSOC(_op)         (op_assoc_table[(_op)])
#define OP_IS_UNARY(_op)      (op_arity_table[(_op)] == 1)
#define OP_IS_BINARY(_op)     (op_arity_table[(_op)] == 2)
#define OP_STR(_op)           (op_str_table[(_op)])
#define OP_MATCH(_op)         (op_match_table[(_op)])
#define OP_STRLEN(_op)        (strlen(OP_STR((_op))))
#define ASSIGNMENT_PREC       (OP_PREC(OP_ASSIGN))
#define HIGHEST_BIN_PREC      (12)
#define LOWEST_TYPE_EXPR_PREC (10)

#endif
