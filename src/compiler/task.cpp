#include "task.h"

#include "instance.h"

namespace Novo {

void add_parse_task(Instance *inst, String_Ref path)
{
    Parse_Task task = {
        .file_name = path,
    };

    darray_append(&inst->parse_tasks, task);
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

            case AST_Node_Kind::STATEMENT: assert(false); break;
            case AST_Node_Kind::EXPRESSION: assert(false); break;
            case AST_Node_Kind::TYPE_SPEC: assert(false); break;
        }
    }
}

void add_resolve_tasks(Instance *inst, AST_Declaration *decl, Scope *scope, AST_Declaration *fn)
{
    switch (decl->kind) {

        case AST_Declaration_Kind::INVALID: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {

            Resolve_Task var_task = {
                .node = ast_node(decl),
                .scope = scope,
                .fn_decl = fn,
            };
            darray_append(&inst->resolve_tasks, var_task);

            break;
        }

        case AST_Declaration_Kind::STRUCT_MEMBER: assert(false); break;

        case AST_Declaration_Kind::STRUCT: {

            Resolve_Task struct_task = {
                .node = ast_node(decl),
                .scope = scope,
                .fn_decl = fn,
            };
            darray_append(&inst->resolve_tasks, struct_task);

            break;
        }

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

    Resolve_Task fn_task = {
        .node = ast_node(decl),
        .scope = scope,
        .fn_decl = fn,
    };
    darray_append(&inst->resolve_tasks, fn_task);

}

void add_resolve_tasks(Instance *inst, AST_Statement *stmt, Scope *scope, AST_Declaration *fn)
{
    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID: assert(false); break;
        case AST_Statement_Kind::IMPORT: assert(false); break;
        case AST_Statement_Kind::CALL: assert(false); break;

        case AST_Statement_Kind::DECLARATION:
        case AST_Statement_Kind::ASSIGNMENT:
        case AST_Statement_Kind::RETURN: {
            Resolve_Task task = {
                .node = ast_node(stmt),
                .scope = scope,
                .fn_decl = fn,
            };
            darray_append(&inst->resolve_tasks, task);
        }
    }
}

void add_resolve_tasks(Instance *inst, AST_Type_Spec *ts, Scope *scope)
{
    Resolve_Task task = {
        .node = ast_node(ts),
        .scope = scope,
        .fn_decl = nullptr,
    };
    darray_append(&inst->resolve_tasks, task);
}

void add_type_task(Instance *inst, AST_Node node, Scope *scope)
{
    Type_Task task = {
        .node = node,
        .scope = scope,
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
