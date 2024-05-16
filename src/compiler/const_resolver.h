#pragma once

#include <defines.h>

#include "type.h"

namespace Novo {

struct AST_Expression;
struct AST_Identifier;

enum class Resolved_Constant_Status : u8
{
    UNRESOLVED,
    RESOLVED,
};

struct Resolved_Constant
{
    Resolved_Constant_Status status;

    Type* type;

    union {
        u64 integer;
    };
};

NAPI Resolved_Constant const_resolve(Instance* inst, AST_Expression* expr);
NAPI Resolved_Constant const_resolve_identifier(Instance* inst, AST_Identifier* ident);

}

