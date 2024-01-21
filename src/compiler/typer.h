#pragma once

#include <defines.h>

namespace Novo {

struct AST_Declaration;
struct AST_Expression;
struct AST_Statement;
struct AST_Type_Spec;
struct Instance;
struct Scope;
struct Task;

NAPI bool type_declaration(Instance *inst, Task *task, AST_Declaration *decl, Scope *scope);
NAPI bool type_statement(Instance *inst, Task *task, AST_Statement *stmt, Scope *scope);
NAPI bool type_expression(Instance *inst, Task *task, AST_Expression *expr, Scope *scope);
NAPI bool type_type_spec(Instance *inst, Task *task, AST_Type_Spec *ts, Scope *scope);

}
