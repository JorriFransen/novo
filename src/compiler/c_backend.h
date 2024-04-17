#pragma once

#include "defines.h"

#include <nstring.h>

namespace Novo {

template <typename T> struct Stack;

struct AST_Expression;
struct Instance;
struct SSA_Function;
struct String_Builder;
struct Type;

NAPI bool c_backend_emit(Instance* inst);

NAPI void c_backend_emit_struct_declaration(Instance* inst, String_Builder* sb, Type *type);

NAPI void c_backend_emit_c_type(Instance* inst, String_Builder* sb, Type* type, String_Ref name);
NAPI String c_backend_emit_c_type(Instance* inst, Type* type, String_Ref name);

NAPI void c_backend_emit_function_decl(Instance* inst, String_Builder* sb, SSA_Function* func);
NAPI void c_backend_emit_function_body(Instance* inst, String_Builder* sb, SSA_Function *func, Stack<u32> *arg_stack);

NAPI void c_backend_emit_constant_expression(Instance* inst, String_Builder* sb, AST_Expression* const_expr);


}
