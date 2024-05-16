#pragma once

#include <containers/stack.h>
#include <defines.h>
#include <nstring.h>

#include "ast.h"
#include "atom.h"
#include "type.h"

namespace Novo {

struct Instance;
struct Scope;
struct Type;

template <typename Element_Type> struct DArray;
enum class Parse_Context : u32;

enum class Parse_Task_Kind : u32
{
    INVALID,
    FILE,
    INSERT,
};

struct Parse_Task
{
    Parse_Task_Kind kind;
    Parse_Context context;

    Atom name;
    String content;
    s64 imported_file_index;

    struct {
        Scope* scope;
        DArray<AST_Node>* bc_deps;
        AST_Declaration* fn_decl;
        AST_Statement* stmt;
    } insert;

    u32 offset; // start offset in inserted_strings file
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
    Infer_Node infer_type_from;
    Scope* scope;

    DArray<AST_Node> *bytecode_deps;

    AST_Declaration* fn_decl;
    AST_Identifier* waiting_for;
};

enum class SSA_Task_Kind : u32
{
    INVALID,
    FUNCTION,
    CONSTANT,
    GLOBAL_VAR,
    RUN,
    INSERT,
};

struct SSA_Task
{
    SSA_Task_Kind kind;
    AST_Node node;

    DArray<AST_Node> *bytecode_deps;
    DArray<AST_Node>* insert_bc_deps;

    Scope* scope;
};

enum class Run_Task_Kind : u32
{
    INVALID,
    RUN,
    INSERT,
};

struct Run_Task
{
    Run_Task_Kind kind;
    AST_Node node;
    Scope* scope;
    s64 wrapper_index;

    DArray<AST_Node>* insert_bc_deps;
};

NAPI void add_parse_task(Instance* inst, Atom path_or_name);
NAPI void add_parse_task(Instance* inst, Atom path_or_name, String content, Scope* insert_scope, AST_Declaration *fn_decl, DArray<AST_Node>* insert_bc_deps, AST_Statement* insert_stmt, s64 import_index, u32 offset);

NAPI void add_resolve_tasks(Instance* inst, AST_File* file, Scope* scope, AST_Declaration* fn);
NAPI void add_resolve_tasks(Instance* inst, DArray<AST_Node> nodes, Scope* scope, AST_Declaration *fn, DArray<AST_Node>* bc_deps);
NAPI void add_resolve_tasks(Instance* inst, AST_Declaration* decl, Scope* scope, AST_Declaration* fn, DArray<AST_Node>* bc_deps);
NAPI void add_resolve_tasks(Instance* inst, AST_Statement* stmt, Scope* scope, AST_Declaration* fn, DArray<AST_Node>* bc_deps);
NAPI void add_resolve_tasks(Instance* inst, AST_Type_Spec* ts, Scope* scope);

NAPI void add_type_task(Instance* inst, AST_Node node, Infer_Node infer_type_from, Scope* scope, AST_Declaration* fn, DArray<AST_Node> *bc_deps);

NAPI void add_ssa_task(Instance* inst, AST_Declaration* decl, DArray<AST_Node>* bc_deps, DArray<AST_Node>* insert_bc_deps);
NAPI void add_ssa_task(Instance* inst, AST_Statement* stmt,Scope* scope, DArray<AST_Node>* bc_deps, DArray<AST_Node>* insert_bc_deps);
NAPI void add_ssa_task(Instance* inst, AST_Expression* expr, Scope* scope, DArray<AST_Node>* bc_deps, DArray<AST_Node>* insert_bc_deps);

NAPI void add_run_task(Instance* inst, AST_Node node, Scope* scope, DArray<AST_Node>* insert_bc_deps, s64 wrapper_index);

NAPI Resolve_Task resolve_task_create(Instance* inst, AST_Node node, Scope* scope, AST_Declaration* fn_decl, DArray<AST_Node>* bc_deps);
NAPI Type_Task type_task_create(Instance* inst, AST_Node node, Infer_Node infer_type_from, Scope* scope, AST_Declaration* fn_decl, DArray<AST_Node> *bc_deps);

}
