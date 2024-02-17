#include "type.h"

#include <memory/allocator.h>
#include <string_builder.h>

#include "instance.h"

#include <assert.h>

namespace Novo {

Type* type_new(Instance* instance, Type_Kind kind, Type_Flags flags, u32 bit_size, u32 alignment)
{
    auto result = allocate<Type>(&instance->ast_allocator);
    result->kind = kind;
    result->flags = flags;
    result->bit_size = bit_size;
    result->alignment = alignment;
    result->pointer_to = nullptr;
    return result;
}

Type* void_type_new(Instance* inst)
{
    auto result = type_new(inst, Type_Kind::VOID, TYPE_FLAG_NONE, 0, 0);
    return result;
}

Type* integer_type_new(Instance* instance, bool sign, u32 bit_size)
{
    assert(bit_size % 8 == 0);
    auto result = type_new(instance, Type_Kind::INTEGER, TYPE_FLAG_NONE, bit_size, bit_size / 8);
    result->integer.sign = sign;
    return result;
}

Type* boolean_type_new(Instance* inst, u32 bit_size)
{
    assert(bit_size % 8 == 0);
    auto result = type_new(inst, Type_Kind::BOOLEAN, TYPE_FLAG_NONE, bit_size, bit_size / 8);
    return result;
}

Type* pointer_type_new(Instance* inst, Type* base)
{
    assert(!base->pointer_to);

    auto result = type_new(inst, Type_Kind::POINTER, TYPE_FLAG_NONE, inst->pointer_byte_size * 8, inst->pointer_byte_size);
    result->pointer.base = base;
    base->pointer_to = result;

    return result;
}

Type* function_type_new(Instance* inst, DArray<Type*> param_types, Type* return_type, Type_Flags flags)
{
    auto result = type_new(inst, Type_Kind::FUNCTION, flags, inst->pointer_byte_size * 8, inst->pointer_byte_size);
    result->function.param_types = param_types;
    result->function.return_type = return_type;
    return result;
}

Type* struct_type_new(Instance* inst, Atom name, Array_Ref<Type*> member_types, Scope* scope)
{
    DArray<Type_Struct_Member> members;
    darray_init(&inst->ast_allocator, &members, member_types.count);

    s64 total_size = 0;
    u32 max_alignment = 1;

    for (s64 i = 0; i < member_types.count; i++) {

        Type_Struct_Member member;
        Type* member_type = member_types[i];
        member.type = member_type;

        s64 member_size = member_type->bit_size;
        assert(member_size % 8 == 0);

        total_size = get_aligned(total_size, member_type->alignment * 8);
        max_alignment = max(max_alignment, member_type->alignment);

        member.offset = total_size;

        darray_append(&members, member);

        total_size += member_size;
    }

    auto result = type_new(inst, Type_Kind::STRUCT, TYPE_FLAG_NONE, total_size, max_alignment);
    result->structure.name = name;
    result->structure.members = members;
    result->structure.scope = scope;
    return result;
}

Type* pointer_type_get(Instance *inst, Type* base)
{
    if (base->pointer_to) return base->pointer_to;

    return pointer_type_new(inst, base);
}

Type* function_type_get(Instance* inst, Temp_Array<Type*> param_types, Type* return_type, Type_Flags flags)
{
    for (s64 i = 0; i < inst->function_types.count; i++) {

        auto fn_type = inst->function_types[i];

        if (fn_type->flags != flags) continue;
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

    Type* new_type = function_type_new(inst, temp_array_finalize(&inst->ast_allocator, &param_types), return_type, flags);
    darray_append(&inst->function_types, new_type);
    return new_type;
}

String temp_type_string(Instance* inst, Type* type)
{
    String_Builder sb;
    string_builder_init(&sb, &inst->temp_allocator, 64);

    type_to_string(&sb, type);

    return string_builder_to_string(&sb);
}

void type_to_string(String_Builder* sb, Type* type)
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

        case Type_Kind::POINTER: {
            string_builder_append(sb, "*");
            type_to_string(sb, type->pointer.base);
            break;
        }

        case Type_Kind::FUNCTION: assert(false); break;

        case Type_Kind::STRUCT: {
            string_builder_append(sb, "%s", atom_string(type->structure.name).data);
            break;
        }
    }
}

}
