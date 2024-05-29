#pragma once

#include "defines.h"

#include <nstring.h>

#define NOVO_C_BACKEND_PRINT_SSA_COMMENTS 1

namespace Novo {

template <typename T> struct Stack;

struct Arena;
struct AST_Expression;
struct Instance;
struct SSA_Function;
struct String_Builder;
struct Type;

struct C_Backend;

NAPI bool c_backend_emit(Instance* inst);

NAPI void c_backend_emit_struct_declaration(C_Backend* cb, String_Builder* sb, Type *type);
NAPI void c_backend_emit_enum_declaration(C_Backend* cb, String_Builder* sb, Type *type);

NAPI void c_backend_emit_c_type(C_Backend* cb, String_Builder* sb, Type* type, String_Ref name);
NAPI String c_backend_emit_c_type(C_Backend* cb, Type* type, String_Ref name);

NAPI void c_backend_emit_function_decl(C_Backend* cb, String_Builder* sb, SSA_Function* func);
NAPI void c_backend_emit_function_body(C_Backend* cb, String_Builder* sb, u32 fn_index, Stack<u32> *arg_stack);

NAPI void c_backend_emit_constant_expression(C_Backend* cb, String_Builder* sb, AST_Expression* const_expr);
NAPI void c_backend_emit_constant_int(C_Backend* cb, String_Builder* sb,  Type* type, u64 value);
NAPI String_Ref c_backend_block_name(C_Backend* cb, SSA_Function* func, s64 block_index, Arena* arena);

}
