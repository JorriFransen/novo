#pragma once

#include <defines.h>

namespace Novo
{

struct AST_Declaration;
struct AST_Identifier;
struct AST_Type_Spec;
struct Instance;
struct Scope;
struct Task;

NAPI bool size_declaration(Instance *inst, Task *task, AST_Declaration *decl, Scope *scope);
NAPI bool size_type_spec(Instance *inst, Task *task, AST_Type_Spec *ts, Scope *scope);
NAPI bool size_identifier(Instance *inst, Task *task, AST_Identifier *ident, Scope *scope);

}
