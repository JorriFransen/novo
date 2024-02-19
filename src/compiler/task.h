#pragma once

#include <containers/stack.h>
#include <defines.h>

#include "ast.h"
#include "atom.h"

namespace Novo {

struct Instance;
struct Scope;
struct Type;

template <typename Element_Type> struct DArray;

struct Parse_Task
{
    Atom file_name;
    s64 imported_file_index;
};

struct Resolve_Task
{
    AST_Node node;
    Scope* scope;

    Stack<AST_Statement*> loop_control_stack;

    DArray<AST_Node> *bytecode_deps;

    AST_Declaration* fn_decl;
    AST_Identifier* waiting_for;
};

struct Type_Task
{
    AST_Node node;
    Type* suggested_type;
    Scope* scope;

    DArray<AST_Node> *bytecode_deps;

    AST_Declaration* fn_decl;
    AST_Identifier* waiting_for;
};

struct SSA_Task
{
    AST_Declaration* func_decl;

    DArray<AST_Node> *bytecode_deps;

    Scope* run_scope;
    AST_Expression* run_expr;
    bool is_run;
};

struct Run_Task
{
    AST_Expression* run_expr;
    Scope* scope;
    s64 wrapper_index;
};

NAPI void add_parse_task(Instance* inst, Atom path);

NAPI void add_resolve_tasks(Instance* inst, AST_File* file, Scope* scope);
NAPI void add_resolve_tasks(Instance* inst, AST_Declaration* decl, Scope* scope, AST_Declaration* fn, DArray<AST_Node> *bc_deps);
NAPI void add_resolve_tasks(Instance* inst, AST_Statement* stmt, Scope* scope, AST_Declaration* fn, DArray<AST_Node> *bc_deps);
NAPI void add_resolve_tasks(Instance* inst, AST_Type_Spec* ts, Scope* scope);

NAPI void add_type_task(Instance* inst, AST_Node node, Type* suggested_type, Scope* scope, AST_Declaration* fn, DArray<AST_Node> *bc_deps);

NAPI void add_ssa_task(Instance* inst, AST_Declaration* decl, DArray<AST_Node> *bc_deps);
NAPI void add_ssa_task(Instance* inst, AST_Expression* expr, Scope* scope, DArray<AST_Node> *bc_deps);

NAPI void add_run_task(Instance* inst, AST_Expression* run_expr, Scope* scope, s64 wrapper_index);

NAPI Resolve_Task resolve_task_create(Instance* inst, AST_Node node, Scope* scope, AST_Declaration* fn_decl, DArray<AST_Node> *bc_deps);
NAPI Type_Task type_task_create(Instance* inst, AST_Node node, Type* suggested_type, Scope* scope, AST_Declaration* fn_decl, DArray<AST_Node> *bc_deps);

}
