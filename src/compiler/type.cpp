#include "type.h"

#include "instance.h"

namespace Novo {

Type *type(Instance *instance, Type_Kind kind, u32 bit_size)
{
    auto result = allocate<Type>(&instance->ast_allocator);
    result->kind = kind;
    result->bit_size = bit_size;
    return result;
}

Type *type_integer(Instance *instance, bool sign, u32 bit_size)
{
    auto result = type(instance, Type_Kind::INTEGER, bit_size);
    result->integer.sign = sign;
    return result;
}

}
