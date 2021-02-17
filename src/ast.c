#include "ast.h"

static const char *ast_kind_to_name[] = {
#define X(kind) #kind,
X_AST
#undef X
};

const char *ast_get_kind_str(int kind) {
    return ast_kind_to_name[kind] + 4;
}
