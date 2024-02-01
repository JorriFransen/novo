#include "task.h"

#include <containers/darray.h>

#include "instance.h"

#include <assert.h>

namespace Novo {

void add_parse_task(Instance *inst, Atom path)
{
    Parse_Task task = {
        .file_name = path,
    };

    darray_append(&inst->parse_tasks, task);
    darray_append(&inst->imported_files, path);
}

void add_resolve_tasks(Instance *inst, AST_File *file, Scope *scope)
{
    for (s64 i = 0; i < file->nodes.count; i++) {

        auto node = file->nodes[i];

        switch (node.kind) {

            case AST_Node_Kind::INVALID: assert(false); break;

            case AST_Node_Kind::DECLARATION: {
                add_resolve_tasks(inst, node.declaration, scope, nullptr);
                break;
            }

            case AST_Node_Kind::STATEMENT: {
                add_resolve_tasks(inst, node.statement, scope, nullptr);
                break;
            }

            case AST_Node_Kind::EXPRESSION: assert(false); break;
            case AST_Node_Kind::TYPE_SPEC: assert(false); break;
        }
    }
}

void add_resolve_tasks(Instance *inst, AST_Declaration *decl, Scope *scope, AST_Declaration *fn)
{
    switch (decl->kind) {

        case AST_Declaration_Kind::INVALID: assert(false); break;

        case AST_Declaration_Kind::STRUCT_MEMBER: assert(false); break;

        case AST_Declaration_Kind::STRUCT: break;
        case AST_Declaration_Kind::VARIABLE: break;

        case AST_Declaration_Kind::FUNCTION: {

            Scope *fn_scope = decl->function.scope;

            for (s64 i = 0; i < decl->function.params.count; i++) {
                add_resolve_tasks(inst, decl->function.params[i], fn_scope, decl);
            }

            if (decl->function.return_ts) {
                add_resolve_tasks(inst, decl->function.return_ts, scope);
            }

            for (s64 i = 0; i < decl->function.body.count; i++) {
                add_resolve_tasks(inst, decl->function.body[i], fn_scope, decl);
            }

            break;
        }

        case AST_Declaration_Kind::BUILTIN_TYPE: assert(false); break;
    }

    Resolve_Task decl_task = {
        .node = ast_node(decl),
        .scope = scope,
        .fn_decl = fn,
        .waiting_for = nullptr,
    };
    darray_append(&inst->resolve_tasks, decl_task);
}

void add_resolve_tasks(Instance *inst, AST_Statement *stmt, Scope *scope, AST_Declaration *fn)
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
        case AST_Statement_Kind::BLOCK: {

            Resolve_Task task = {
                .node = ast_node(stmt),
                .scope = scope,
                .fn_decl = fn,
                .waiting_for = nullptr,
            };

            darray_append(&inst->resolve_tasks, task);
            break;
        }

    }
}

void add_resolve_tasks(Instance *inst, AST_Type_Spec *ts, Scope *scope)
{
    Resolve_Task task = {
        .node = ast_node(ts),
        .scope = scope,
        .fn_decl = nullptr,
        .waiting_for = nullptr,
    };
    darray_append(&inst->resolve_tasks, task);
}

void add_type_task(Instance *inst, AST_Node node, Scope *scope, AST_Declaration *fn)
{
    Type_Task task = {
        .node = node,
        .scope = scope,
        .fn_decl = fn,
    };
    darray_append(&inst->type_tasks, task);
}

void add_ssa_task(Instance *inst, AST_Node node)
{
    assert(node.kind == AST_Node_Kind::DECLARATION);
    assert(node.declaration->kind == AST_Declaration_Kind::FUNCTION);

    SSA_Task task = {
        .func_decl = node.declaration,
    };
    darray_append(&inst->ssa_tasks, task);
}

}
