#pragma once

#include <defines.h>

namespace Novo {

struct Instance;

enum class Type_Kind : u32
{
    INVALID,
    INTEGER,
};

struct Type
{
    Type_Kind kind;    
    u32 bit_size;

    union {
        struct {
            bool sign;
        } integer;
    };
};

NAPI Type *type(Instance *instance, Type_Kind kind, u32 bit_size);
NAPI Type *type_integer(Instance *instance, bool sign, u32 bit_size);

}
