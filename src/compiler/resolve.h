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

NAPI bool resolve_declaration(Instance *instance, AST_Declaration *decl, Scope *scope);
NAPI bool resolve_statement(Instance *instance, AST_Statement *stmt, Scope *scope);
NAPI bool resolve_expression(Instance *instance, AST_Expression *expr, Scope *scope);
NAPI bool resolve_ts(Instance *instance, AST_Type_Spec *ts, Scope *scope);
NAPI bool resolve_ident(Instance *instance, AST_Identifier *ident, Scope *scope);

}
