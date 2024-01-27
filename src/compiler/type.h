#pragma once

#include <containers/darray.h>
#include <defines.h>
#include <nstring.h>

namespace Novo {

struct Instance;
struct String_Builder;

enum class Type_Kind : u32
{
    INVALID,
    VOID,

    INTEGER,
    BOOLEAN,

    FUNCTION,
    STRUCT,
};

struct Type;

struct Type_Struct_Member
{
    Type *type;
    u32 offset;
};

struct Type
{
    Type_Kind kind;
    u32 bit_size;

    union {
        struct {
            bool sign;
        } integer;

        struct {
            DArray<Type *> param_types;
            Type *return_type;
        } function;

        struct {
            DArray<Type_Struct_Member> members;
        } structure;
    };
};

NAPI Type *type_new(Instance *inst, Type_Kind kind, u32 bit_size);
NAPI Type *void_type_new(Instance *inst);
NAPI Type *integer_type_new(Instance *inst, bool sign, u32 bit_size);
NAPI Type *boolean_type_new(Instance *inst, u32 bit_size);
NAPI Type *function_type_new(Instance *inst, DArray<Type *> param_types, Type *return_type);
NAPI Type *struct_type_new(Instance *inst, Array_Ref<Type *> member_types);

NAPI Type *function_type_get(Instance *inst, Temp_Array<Type *> param_types, Type *return_type);

NAPI String temp_type_string(Instance *inst, Type *type);
NAPI void type_to_string(String_Builder *sb, Type *type);

}
