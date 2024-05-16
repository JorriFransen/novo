#include "type.h"

#include <containers/darray.h>
#include <memory/allocator.h>
#include <string_builder.h>

#include "ast.h"
#include "instance.h"
#include "token.h"
#include "typer.h"

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

Type* struct_type_new(Instance* inst, Atom name, Array_Ref<Type_Struct_Member> members, Scope* scope)
{
    DArray<Type_Struct_Member_Resolved> resolved_members;
    darray_init(&inst->ast_allocator, &resolved_members, members.count);

    s64 total_size = 0;
    u32 max_alignment = 1;

    for (s64 i = 0; i < members.count; i++) {

        Type_Struct_Member_Resolved member;
        Type* member_type = members[i].type;
        member.name = members[i].name;
        member.type = member_type;

        s64 member_size = member_type->bit_size;
        assert(member_size % 8 == 0);

        total_size = get_aligned(total_size, member_type->alignment * 8);
        max_alignment = max(max_alignment, member_type->alignment);

        member.offset = total_size;

        darray_append(&resolved_members, member);

        total_size += member_size;
    }

    auto result = type_new(inst, Type_Kind::STRUCT, TYPE_FLAG_NONE, total_size, max_alignment);
    result->structure.name = name;
    result->structure.members = resolved_members;
    result->structure.scope = scope;

    darray_append(&inst->struct_types, result);

    return result;
}

Type* enum_type_new(Instance* inst, Atom name, Type* strict_type, Array_Ref<Type_Enum_Member> members, Scope* scope)
{
    Type* result = type_new(inst, Type_Kind::ENUM, TOK_FLAG_NONE, strict_type->bit_size, strict_type->alignment);

    result->enumeration.name = name;
    result->enumeration.strict_type = strict_type;
    result->enumeration.members = darray_copy(&inst->ast_allocator, members);
    result->enumeration.scope = scope;

    darray_append(&inst->enum_types, result);

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

bool is_pointer_or_parent_of_pointer(Type* type)
{
    switch (type->kind) {
        case Type_Kind::INVALID: assert(false); break;

        case Type_Kind::VOID:
        case Type_Kind::INTEGER:
        case Type_Kind::BOOLEAN:
        case Type_Kind::FUNCTION:
        case Type_Kind::ENUM: {
            return false;
        }

        case Type_Kind::POINTER: {
            return true;
        }


        case Type_Kind::STRUCT: {
            bool result = false;

            for (s64 i = 0; i < type->structure.members.count; i++) {

                if (is_pointer_or_parent_of_pointer(type->structure.members[i].type)) {
                    result = true;
                    break;
                }

            }

            return result;
        }
    }

    return false;
    assert(false);
}

bool valid_implicit_type_conversion(Instance* inst, Type* from, Type* to)
{
    // Implicit conversion to cstring
    if ((from == pointer_type_get(inst, inst->builtin_type_u8) || from == pointer_type_get(inst, inst->builtin_type_s8)) &&
        to == inst->builtin_type_cstring) {

        return true;

    }
    return false;
}

String temp_type_string(Instance* inst, Type* type)
{
    String_Builder sb;
    string_builder_init(&sb, &inst->temp_allocator, 64);

    type_to_string(inst, &sb, type);

    return string_builder_to_string(&sb);
}

void type_to_string(Instance* instance, String_Builder* sb, Type* type)
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
            if (type == instance->builtin_type_cstring) {
                string_builder_append(sb, "cstring");
            } else {
                string_builder_append(sb, "*");
                type_to_string(instance, sb, type->pointer.base);
            }
            break;
        }

        case Type_Kind::FUNCTION: assert(false); break;

        case Type_Kind::STRUCT: {
            string_builder_append(sb, "%s", atom_string(type->structure.name).data);
            break;
        }

        case Type_Kind::ENUM: {
            string_builder_append(sb, "%s", atom_string(type->structure.name).data);
            break;
        }
    }
}

Infer_Node infer_node()
{
    Infer_Node result;
    result.kind = Infer_Node_Kind::NONE;
    return result;
}

Infer_Node infer_node(Type* type)
{
    Infer_Node result;
    result.kind = Infer_Node_Kind::TYPE;
    result.type = type;
    return result;
}

Infer_Node infer_node(AST_Type_Spec* ts)
{
    Infer_Node result;
    result.kind = Infer_Node_Kind::TYPE_SPEC;
    result.type_spec = ts;
    return result;
}

Type* infer_type(Instance* inst, Type_Task* task, const Infer_Node& infer_node, Scope* scope)
{
    switch (infer_node.kind) {
        case Infer_Node_Kind::NONE: assert(false); break;

        case Infer_Node_Kind::TYPE: {
            return infer_node.type;
        }

        case Infer_Node_Kind::TYPE_SPEC: {

            if (!infer_node.type_spec->resolved_type) {
                bool type_res = type_type_spec(inst, task, infer_node.type_spec, scope);
                assert(type_res);
            }

            return infer_node.type_spec->resolved_type;
        }
    }

    assert(false);
    return nullptr;
}

}
