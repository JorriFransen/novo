#pragma once

#include <containers/stack.h>
#include <defines.h>

#include "ast.h"
#include "atom.h"

namespace Novo {

struct Instance;
struct Scope;

struct Parse_Task
{
    Atom file_name;
};

struct Resolve_Task
{
    AST_Node node;
    Scope *scope;

    Stack<AST_Statement *> break_stack;

    AST_Declaration *fn_decl;
    AST_Identifier *waiting_for;
};

struct Type_Task
{
    AST_Node node;
    Scope *scope;

    AST_Declaration *fn_decl;
};

struct SSA_Task
{
    AST_Declaration *func_decl;
};

NAPI void add_parse_task(Instance *inst, Atom path);

NAPI void add_resolve_tasks(Instance *inst, AST_File *file, Scope *scope);
NAPI void add_resolve_tasks(Instance *inst, AST_Declaration *decl, Scope *scope, AST_Declaration *fn);
NAPI void add_resolve_tasks(Instance *inst, AST_Statement *stmt, Scope *scope, AST_Declaration *fn);
NAPI void add_resolve_tasks(Instance *inst, AST_Type_Spec *ts, Scope *scope);

NAPI void add_type_task(Instance *inst, AST_Node node, Scope *scope, AST_Declaration *fn);

NAPI void add_ssa_task(Instance *inst, AST_Node node);

NAPI Resolve_Task resolve_task_create(Instance *inst, AST_Node node, Scope *scope, AST_Declaration *fn_decl);

}
