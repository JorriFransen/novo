#include "type.h"

#include <memory/allocator.h>
#include <string_builder.h>

#include "instance.h"

#include <assert.h>

namespace Novo {

Type *type_new(Instance *instance, Type_Kind kind, u32 bit_size)
{
    auto result = allocate<Type>(&instance->ast_allocator);
    result->kind = kind;
    result->bit_size = bit_size;
    return result;
}

Type *void_type_new(Instance *inst)
{
    auto result = type_new(inst, Type_Kind::VOID, 0);
    return result;
}

Type *integer_type_new(Instance *instance, bool sign, u32 bit_size)
{
    auto result = type_new(instance, Type_Kind::INTEGER, bit_size);
    result->integer.sign = sign;
    return result;
}

Type *boolean_type_new(Instance *inst, u32 bit_size)
{
    auto result = type_new(inst, Type_Kind::BOOLEAN, bit_size);
    return result;
}

Type *function_type_new(Instance *inst, DArray<Type *> param_types, Type *return_type)
{
    auto result = type_new(inst, Type_Kind::FUNCTION, 64);
    result->function.param_types = param_types;
    result->function.return_type = return_type;
    return result;
}

Type *function_type_get(Instance *inst, Temp_Array<Type *> param_types, Type *return_type)
{
    for (s64 i = 0; i < inst->function_types.count; i++) {

        auto fn_type = inst->function_types[i];

        if (fn_type->function.return_type != return_type) continue;
        if (fn_type->function.param_types.count != param_types.array.count) continue;

        bool param_match = true;
        for (s64 pi = 0; pi < fn_type->function.param_types.count; pi++) {

            if (fn_type->function.param_types[pi] != param_types[pi]) {
                param_match = false;
                break;
            }
        }

        if (!param_match) continue;

        return fn_type;
    }

    Type *new_type = function_type_new(inst, temp_array_finalize(&inst->ast_allocator, &param_types), return_type);
    darray_append(&inst->function_types, new_type);
    return new_type;
}

String temp_type_string(Instance *inst, Type *type)
{
    String_Builder sb;
    string_builder_init(&sb, &inst->temp_allocator, 64);

    type_to_string(&sb, type);

    return string_builder_to_string(&sb);
}

void type_to_string(String_Builder *sb, Type *type)
{
    switch (type->kind) {

        case Type_Kind::INVALID: assert(false); break;

        case Type_Kind::VOID: string_builder_append(sb, "void"); break;

        case Type_Kind::INTEGER: {
            string_builder_append(sb, "%c%d", type->integer.sign ? 's' : 'u', type->bit_size);
            break;
        }

        case Type_Kind::BOOLEAN: {
            string_builder_append(sb, "bool%d", type->bit_size);
            break;
        }

        case Type_Kind::FUNCTION: assert(false); break;
    }
}

}
