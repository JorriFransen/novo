#include "task.h"

#include <containers/darray.h>
#include <memory/freelist.h>

#include "instance.h"
#include "parser.h"
#include "scope.h"
#include "type.h"

#include <assert.h>

namespace Novo {

void add_parse_task(Instance* inst, Atom path_or_name)
{
    s64 imported_file_index = inst->imported_files.count;
    darray_append(&inst->imported_files, { path_or_name, nullptr });

    return add_parse_task(inst, path_or_name, {}, inst->global_scope, nullptr, nullptr, nullptr, imported_file_index, 0);
}

void add_parse_task(Instance* inst, Atom path_or_name, String content, Scope* insert_scope, AST_Declaration* fn_decl, DArray<AST_Node>* insert_bc_deps, AST_Statement* insert_stmt, s64 imported_file_index, u32 offset)
{
    Parse_Context context = Parse_Context::INVALID;
    if (insert_scope->kind == Scope_Kind::GLOBAL) {
        context = Parse_Context::FILE_SCOPE;
    } else {
        assert(insert_scope->kind == Scope_Kind::FUNCTION_LOCAL);
        context = Parse_Context::LOCAL_STATEMENT_SCOPE;
    }
    assert(context != Parse_Context::INVALID);

    Parse_Task task = {
        .kind = content.length ? Parse_Task_Kind::INSERT : Parse_Task_Kind::FILE,
        .context = context,
        .name = path_or_name,
        .content = content,
        .imported_file_index = imported_file_index,
        .insert = {
            .scope = insert_scope,
            .bc_deps = insert_bc_deps,
            .fn_decl = fn_decl,
            .stmt = insert_stmt,
        },
        .offset = offset,
    };

    darray_append(&inst->parse_tasks, task);
}

void add_resolve_tasks(Instance* inst, AST_File* file, Scope* scope, AST_Declaration* fn_decl)
{
    for (s64 i = 0; i < file->nodes.count; i++) {

        auto node = file->nodes[i];

        switch (node.kind) {

            case AST_Node_Kind::INVALID: assert(false); break;

            case AST_Node_Kind::DECLARATION: {
                add_resolve_tasks(inst, node.declaration, scope, fn_decl, nullptr);
                break;
            }

            case AST_Node_Kind::STATEMENT: {
                add_resolve_tasks(inst, node.statement, scope, fn_decl, nullptr);
                break;
            }

            case AST_Node_Kind::EXPRESSION: assert(false); break;
            case AST_Node_Kind::TYPE_SPEC: assert(false); break;
            case AST_Node_Kind::IDENTIFIER: assert(false); break;
        }
    }
}

void add_resolve_tasks(Instance* inst, DArray<AST_Node> nodes, Scope* scope, AST_Declaration *fn_decl, DArray<AST_Node>* bc_deps)
{
    for (s64 i = 0; i < nodes.count; i++) {
        auto node = nodes[i];

        switch (node.kind) {
            case AST_Node_Kind::INVALID: assert(false); break;

            case AST_Node_Kind::DECLARATION: {
                add_resolve_tasks(inst, node.declaration, scope, fn_decl, bc_deps);
                break;
            }

            case AST_Node_Kind::STATEMENT: {
                add_resolve_tasks(inst, node.statement, scope, fn_decl, bc_deps);
                break;
            }

            case AST_Node_Kind::EXPRESSION: assert(false); break;
            case AST_Node_Kind::TYPE_SPEC: assert(false); break;
            case AST_Node_Kind::IDENTIFIER: assert(false); break;
        }
    }
}

void add_resolve_tasks(Instance* inst, AST_Declaration* decl, Scope* scope, AST_Declaration* fn, DArray<AST_Node> *bc_deps)
{
    switch (decl->kind) {

        case AST_Declaration_Kind::INVALID: assert(false); break;

        case AST_Declaration_Kind::STRUCT_MEMBER: assert(false); break;

        case AST_Declaration_Kind::ENUM_MEMBER: {
            assert(false);
            break;
        }

        case AST_Declaration_Kind::ENUM: break;
        case AST_Declaration_Kind::STRUCT: break;
        case AST_Declaration_Kind::VARIABLE: break;
        case AST_Declaration_Kind::CONSTANT: break;

        case AST_Declaration_Kind::FUNCTION: {

            Scope* fn_scope = decl->function.scope;

            auto fn_deps = nallocate(fl_allocator(), DArray<AST_Node>);
            darray_init(fl_allocator(), fn_deps, 0);

            for (s64 i = 0; i < decl->function.params.count; i++) {
                add_resolve_tasks(inst, decl->function.params[i], fn_scope, decl, bc_deps);
            }

            if (decl->function.return_ts) {
                add_resolve_tasks(inst, decl->function.return_ts, scope);
            }

            for (s64 i = 0; i < decl->function.body.count; i++) {
                add_resolve_tasks(inst, decl->function.body[i], fn_scope, decl, fn_deps);
            }

            bc_deps = fn_deps;
            break;
        }

        case AST_Declaration_Kind::BUILTIN_TYPE: assert(false); break;
    }

    Resolve_Task decl_task = resolve_task_create(inst, ast_node(decl), scope, fn, bc_deps);
    darray_append(&inst->resolve_tasks, decl_task);
}

void add_resolve_tasks(Instance* inst, AST_Statement* stmt, Scope* scope, AST_Declaration* fn, DArray<AST_Node> *bc_deps)
{
    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID:
        case AST_Statement_Kind::IMPORT:
        case AST_Statement_Kind::CALL:
        case AST_Statement_Kind::DECLARATION:
        case AST_Statement_Kind::ASSIGNMENT:
        case AST_Statement_Kind::ARITHMETIC_ASSIGNMENT:
        case AST_Statement_Kind::RETURN:
        case AST_Statement_Kind::IF:
        case AST_Statement_Kind::WHILE:
        case AST_Statement_Kind::FOR:
        case AST_Statement_Kind::BREAK:
        case AST_Statement_Kind::CONTINUE:
        case AST_Statement_Kind::BLOCK:
        case AST_Statement_Kind::RUN:
        case AST_Statement_Kind::INSERT:
        case AST_Statement_Kind::ASSERT: {

            Resolve_Task task = resolve_task_create(inst, ast_node(stmt), scope, fn, bc_deps);

            darray_append(&inst->resolve_tasks, task);
            break;
        }

    }
}

void add_resolve_tasks(Instance* inst, AST_Type_Spec* ts, Scope* scope)
{
    Resolve_Task task = resolve_task_create(inst, ast_node(ts), scope, nullptr, nullptr);
    darray_append(&inst->resolve_tasks, task);
}

void add_type_task(Instance* inst, AST_Node node, Infer_Node infer_type_from, Scope* scope, AST_Declaration* fn, DArray<AST_Node> *bc_deps)
{
    Type_Task task = type_task_create(inst, node, infer_type_from, scope, fn, bc_deps);
    darray_append(&inst->type_tasks, task);
}

void add_ssa_task(Instance* inst, AST_Declaration* decl, DArray<AST_Node> *bc_deps, DArray<AST_Node>* insert_bc_deps)
{
    SSA_Task_Kind kind = SSA_Task_Kind::INVALID;

    if (decl->kind == AST_Declaration_Kind::FUNCTION) {
        kind = SSA_Task_Kind::FUNCTION;
    } else if (decl->kind == AST_Declaration_Kind::CONSTANT) {

        assert(decl->resolved_type->kind == Type_Kind::STRUCT);
        kind = SSA_Task_Kind::CONSTANT;

    } else {
        assert(decl->kind == AST_Declaration_Kind::VARIABLE && decl->flags & AST_DECL_FLAG_GLOBAL);
        kind = SSA_Task_Kind::GLOBAL_VAR;
    }

    SSA_Task task = {
        .kind = kind,
        .node = ast_node(decl),
        .bytecode_deps = bc_deps,
        .insert_bc_deps = insert_bc_deps,
        .scope = nullptr,
    };
    darray_append(&inst->ssa_tasks, task);
}

void add_ssa_task(Instance* inst, AST_Statement* stmt, Scope* scope, DArray<AST_Node>* bc_deps, DArray<AST_Node>* insert_bc_deps)
{
    SSA_Task_Kind kind = SSA_Task_Kind::INVALID;

    if (stmt->kind == AST_Statement_Kind::RUN) {
        kind = SSA_Task_Kind::RUN;
    } else {
        assert(stmt->kind == AST_Statement_Kind::INSERT);
        kind = SSA_Task_Kind::INSERT;
    }

    assert(kind != SSA_Task_Kind::INVALID);

    SSA_Task task = {
        .kind = kind,
        .node = ast_node(stmt),
        .bytecode_deps = bc_deps,
        .insert_bc_deps = insert_bc_deps,

        .scope = scope,
    };
    darray_append(&inst->ssa_tasks, task);
}

void add_ssa_task(Instance* inst, AST_Expression* expr, Scope* scope, DArray<AST_Node> *bc_deps, DArray<AST_Node>* insert_bc_deps)
{
    assert(expr->kind == AST_Expression_Kind::RUN);

    SSA_Task task = {
        .kind = SSA_Task_Kind::RUN,
        .node = ast_node(expr),
        .bytecode_deps = bc_deps,
        .insert_bc_deps = insert_bc_deps,
        .scope = scope,
    };
    darray_append(&inst->ssa_tasks, task);
}

void add_run_task(Instance* inst, AST_Node node, Scope* scope, DArray<AST_Node>* insert_bc_deps, s64 wrapper_index)
{
    Run_Task_Kind kind = Run_Task_Kind::INVALID;

    if (node.kind == AST_Node_Kind::EXPRESSION) {
        assert(node.expression->kind == AST_Expression_Kind::RUN);
        kind = Run_Task_Kind::RUN;
    } else {
        assert(node.kind == AST_Node_Kind::STATEMENT);

        if (node.statement->kind == AST_Statement_Kind::RUN) {
            kind = Run_Task_Kind::RUN;
        } else {
            assert(node.statement->kind == AST_Statement_Kind::INSERT);
            kind = Run_Task_Kind::INSERT;
        }

    }

    assert(kind != Run_Task_Kind::INVALID);


    Run_Task task = {
        .kind = kind,
        .node = node,
        .scope = scope,
        .wrapper_index = wrapper_index,
        .insert_bc_deps = insert_bc_deps,
    };
    darray_append(&inst->run_tasks, task);
}

Resolve_Task resolve_task_create(Instance* inst, AST_Node node, Scope* scope, AST_Declaration* fn_decl, DArray<AST_Node> *bc_deps)
{

    Resolve_Task result;
    result.node = node;
    result.scope = scope;
    result.fn_decl = fn_decl;
    result.waiting_for = nullptr;
    result.loop_control_stack = {};

    result.bytecode_deps = bc_deps;

    return result;
}

Type_Task type_task_create(Instance* inst, AST_Node node, Infer_Node infer_type_from, Scope* scope, AST_Declaration* fn_decl, DArray<AST_Node> *bc_deps)
{
    Type_Task result;
    result.node = node;
    result.infer_type_from = infer_type_from;
    result.scope = scope;
    result.fn_decl = fn_decl;
    result.waiting_for = nullptr;
    result.bytecode_deps = bc_deps;

    return result;
}


}
