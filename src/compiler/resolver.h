#pragma once

#include "type.h"
#include <defines.h>

namespace Novo {

struct AST_Declaration;
struct AST_Expression;
struct AST_Identifier;
struct AST_Node;
struct AST_Statement;
struct AST_Type_Spec;
struct Instance;
struct Scope;
struct Resolve_Task;

NAPI bool resolve_node(Instance* inst, Resolve_Task* task, AST_Node* node, Scope* scope);
NAPI bool resolve_declaration(Instance* inst, Resolve_Task* task, AST_Declaration* decl, Scope* scope);
NAPI bool resolve_statement(Instance* inst, Resolve_Task* task, AST_Statement* stmt, Scope* scope);
NAPI bool resolve_expression(Instance* inst, Resolve_Task* task, AST_Expression* expr, Scope* scope);
NAPI bool resolve_ts(Instance* inst, Resolve_Task* task, AST_Type_Spec* ts, Scope* scope);
NAPI bool resolve_identifier(Instance* inst, Resolve_Task* task, AST_Identifier* ident, Scope* scope);

}
