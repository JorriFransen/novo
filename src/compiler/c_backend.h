#pragma once

#include "defines.h"

#include <nstring.h>

namespace Novo {

template <typename T> struct Stack;

struct Type;
struct Instance;
struct SSA_Function;
struct String_Builder;

NAPI bool c_backend_emit(Instance* inst);

NAPI void c_backend_emit_c_type(Instance* inst, String_Builder* sb, Type* type, String_Ref name);
NAPI void c_backend_emit_function_body(Instance* inst, String_Builder* sb, SSA_Function *func, Stack<u32> *arg_stack);


}
