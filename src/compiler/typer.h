#pragma once

#include <defines.h>

namespace Novo {

struct AST_Declaration;
struct AST_Expression;
struct AST_Node;
struct AST_Statement;
struct AST_Type_Spec;
struct Instance;
struct Scope;
struct Type;
struct Type_Task;

NAPI bool type_node(Instance *inst, Type_Task *task, AST_Node *node, Scope *scope);
NAPI bool type_declaration(Instance *inst, Type_Task *task, AST_Declaration *decl, Scope *scope);
NAPI bool type_statement(Instance *inst, Type_Task *task, AST_Statement *stmt, Scope *scope);
NAPI bool type_expression(Instance *inst, Type_Task *task, AST_Expression *expr, Scope *scope, Type *suggested_type);
NAPI bool type_type_spec(Instance *inst, Type_Task *task, AST_Type_Spec *ts, Scope *scope);

}
