#pragma once

#include <defines.h>

namespace Novo {

struct AST_Declaration;
struct AST_Expression;
struct AST_Node;
struct AST_Node;
struct AST_Statement;
struct AST_Type_Spec;
struct Instance;
struct Scope;

NAPI bool type_node(Instance *inst, AST_Node *node, Scope *scope);
NAPI bool type_declaration(Instance *inst, AST_Declaration *decl, Scope *scope);
NAPI bool type_statement(Instance *inst, AST_Statement *stmt, Scope *scope);
NAPI bool type_expression(Instance *inst, AST_Expression *expr, Scope *scope);
NAPI bool type_type_spec(Instance *inst, AST_Type_Spec *ts, Scope *scope);

}
