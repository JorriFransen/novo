#pragma once

#include "atom.h"
#include <containers/darray.h>
#include <defines.h>
#include <nstring.h>

namespace Novo {

struct AST_Type_Spec;
struct Instance;
struct String_Builder;
struct Type_Task;

#undef VOID // windows...

enum class Type_Kind : u32
{
    INVALID,
    VOID,

    INTEGER,
    BOOLEAN,

    POINTER,

    FUNCTION,
    STRUCT,
    ENUM,
};

struct Scope;
struct Type;

// Used when creating new struct types
struct Type_Struct_Member
{
    Atom name;
    Type* type;
};

// Stored in the final struct type
struct Type_Struct_Member_Resolved
{
    Atom name;
    Type* type;
    u32 offset;
};

struct Type_Enum_Member
{
    Atom name;
    s64 value;
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
            DArray<Type_Struct_Member_Resolved> members;
            Scope* scope;
        } structure;

        struct {
            Atom name;
            Type* strict_type;
            DArray<Type_Enum_Member> members;
            Scope* scope;
        } enumeration;
    };
};

enum class Infer_Node_Kind : u8
{
    NONE,
    TYPE,
    TYPE_SPEC,
};

struct Infer_Node
{
    Infer_Node_Kind kind;

    union {
        Type* type;
        AST_Type_Spec* type_spec;
    };
};

NAPI Type* type_new(Instance* instance, Type_Kind kind, Type_Flags flags, u32 bit_size, u32 alignment);
NAPI Type* void_type_new(Instance* inst);
NAPI Type* integer_type_new(Instance* inst, bool sign, u32 bit_size);
NAPI Type* boolean_type_new(Instance* inst, u32 bit_size);
NAPI Type* pointer_type_new(Instance* inst, Type* base);
NAPI Type* function_type_new(Instance* inst, DArray<Type*> param_types, Type* return_type, Type_Flags flags);
NAPI Type* struct_type_new(Instance* inst, Atom name, Array_Ref<Type_Struct_Member> members, Scope* scope);
NAPI Type* enum_type_new(Instance* inst, Atom name, Type* strict_Type,  Array_Ref<Type_Enum_Member> members, Scope* scope);

NAPI Type* pointer_type_get(Instance *inst, Type* base);
NAPI Type* function_type_get(Instance* inst, Temp_Array<Type*> param_types, Type* return_type, Type_Flags flags);

NAPI bool is_pointer_or_parent_of_pointer(Type* type);

NAPI bool valid_implicit_type_conversion(Instance* inst, Type* from, Type* to);

NAPI String temp_type_string(Instance* inst, Type* type);
NAPI void type_to_string(Instance* instance, String_Builder* sb, Type* type);

NAPI Infer_Node infer_node();
NAPI Infer_Node infer_node(Type* type);
NAPI Infer_Node infer_node(AST_Type_Spec* ts);
NAPI Type* infer_type(Instance* inst, Type_Task* task, const Infer_Node& infer_node, Scope* scope);

}
