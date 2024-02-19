#pragma once

#include "atom.h"
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

    POINTER,

    FUNCTION,
    STRUCT,
};

struct Scope;
struct Type;

struct Type_Struct_Member
{
    Type* type;
    u32 offset;
};

typedef u32 Type_Flags;
enum Type_Flag : Type_Flags
{
    TYPE_FLAG_NONE              = 0x00,
    TYPE_FLAG_FOREIGN_VARARG    = 0x01,
};

struct Type
{
    Type_Kind kind;
    Type_Flags flags;
    u32 bit_size;
    u32 alignment; // in bytes

    Type *pointer_to;

    union {
        struct {
            bool sign;
        } integer;

        struct {
            Type *base;
        } pointer;

        struct {
            DArray<Type*> param_types;
            Type* return_type;
        } function;

        struct {
            Atom name;
            DArray<Type_Struct_Member> members;
            Scope* scope;
        } structure;
    };
};

NAPI Type* type_new(Instance* instance, Type_Kind kind, Type_Flags flags, u32 bit_size, u32 alignment);
NAPI Type* void_type_new(Instance* inst);
NAPI Type* integer_type_new(Instance* inst, bool sign, u32 bit_size);
NAPI Type* boolean_type_new(Instance* inst, u32 bit_size);
NAPI Type* pointer_type_new(Instance* inst, Type* base);
NAPI Type* function_type_new(Instance* inst, DArray<Type*> param_types, Type* return_type, Type_Flags flags);
NAPI Type* struct_type_new(Instance* inst, Atom name, Array_Ref<Type*> member_types, Scope* scope);

NAPI Type* pointer_type_get(Instance *inst, Type* base);
NAPI Type* function_type_get(Instance* inst, Temp_Array<Type*> param_types, Type* return_type, Type_Flags flags);

NAPI bool is_pointer_or_parent_of_pointer(Type* type);

NAPI String temp_type_string(Instance* inst, Type* type);
NAPI void type_to_string(String_Builder* sb, Type* type);

}
