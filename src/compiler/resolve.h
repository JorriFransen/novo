#pragma once

#include <defines.h>

namespace Novo {

struct AST_Declaration;
struct AST_Expression;
struct AST_Identifier;
struct AST_Statement;
struct AST_Type_Spec;
struct Instance;
struct Scope;
struct Task;

NAPI bool resolve_declaration(Instance *instance, Task *task, AST_Declaration *decl, Scope *scope);
NAPI bool resolve_statement(Instance *instance, Task *task, AST_Statement *stmt, Scope *scope);
NAPI bool resolve_expression(Instance *instance, Task *task, AST_Expression *expr, Scope *scope);
NAPI bool resolve_ts(Instance *instance, Task *task, AST_Type_Spec *ts, Scope *scope);
NAPI bool resolve_ident(Instance *instance, Task *task, AST_Identifier *ident, Scope *scope);

}
