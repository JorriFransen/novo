#include "task.h"

#include <containers/darray.h>

#include "ast.h"
#include "ast_print.h"
#include "atom.h"
#include "instance.h"
#include "logger.h"
#include "options.h"
#include "parser.h"
#include "resolver.h"
#include "sizer.h"
#include "typer.h"

#include <cassert>
#include <cstdio>

namespace Novo {

void create_task(Instance *inst, Task *task, Task_Kind kind)
{
    *task = {};
    task->kind = kind;
    task->done = false;
}

void parse_task_create(Instance *inst, Task *task, const String_Ref file_path)
{
    create_task(inst, task, Task_Kind::PARSE);
    task->parse = {
        file_path,
    };
}

void resolve_task_create(Instance *inst, Task *task, AST_Declaration *decl, Scope *scope)
{
    create_task(inst, task, Task_Kind::RESOLVE);
    task->resolve = {
        .decl = decl,
        .scope = scope,
        .waiting_for = nullptr
    };
}

void size_task_create(Instance *inst, Task *task, AST_Declaration *decl, Scope *scope)
{
    create_task(inst, task, Task_Kind::SIZE);
    task->size = {
        .decl = decl,
        .scope = scope,
    };
}

void type_task_create(Instance *inst, Task *task, AST_Declaration *decl, Scope *scope)
{
    create_task(inst, task, Task_Kind::TYPE);
    task->type = {
        .decl = decl,
        .scope = scope,
        .waiting_for = nullptr,
        .current_function_type = nullptr,
    };
}

bool task_execute(Instance *inst, Task *task)
{
    bool result = false;

    switch (task->kind) {

        case Task_Kind::INVALID: assert(false); break;

        case Task_Kind::PARSE: {
            result = parse_task_execute(inst, task);

            if (result) {
                queue_resolve_tasks(inst, task->parse.result, inst->global_scope);
            }
            break;
        }

        case Task_Kind::RESOLVE: {
            result = resolve_task_execute(inst, task);

            if (result) {
                queue_size_tasks(inst, task->resolve.decl, task->resolve.scope);
            }
            break;
        };

        case Task_Kind::SIZE: {
            result = size_task_execute(inst, task);

            if (result) {
                queue_type_tasks(inst, task->size.decl, task->size.scope);
            }
            break;
        }

        case Task_Kind::TYPE: {
            result = type_task_execute(inst, task);
            break;
        }
    }

    if (result) task->done = true;

    return result;
}

bool parse_task_execute(Instance *inst, Task *task)
{
    assert(task->kind == Task_Kind::PARSE);

    log_trace("Parsing: %s", task->parse.full_path.data);

    auto file = parse_file(inst, task->parse.full_path.data);

    if (!file) {
        return false;
    }

    task->parse.result = file;

    if (inst->options.print_ast) {
        auto ast_str = ast_to_string(inst, file, &inst->temp_allocator);
        printf("%s\n", ast_str.data);
    }

    return true;
}

bool resolve_task_execute(Instance *inst, Task *task)
{
    assert(task->kind == Task_Kind::RESOLVE);

    auto name = atom_string(task->resolve.decl->ident->atom);
    log_trace("Resolving: %s...", name.data);

    bool result = resolve_declaration(inst, task, task->resolve.decl, task->resolve.scope);
    log_trace("Resolving: %s...%s", name.data, result ? "success" : "fail");

    return result;
}

bool size_task_execute(Instance *inst, Task *task)
{
    assert(task->kind == Task_Kind::SIZE);

    auto name = atom_string(task->resolve.decl->ident->atom);
    log_trace("Sizing: %s...", name.data);

    bool result = size_declaration(inst, task, task->size.decl, task->size.scope);

    log_trace("Sizing: %s...%s", name.data, result ? "success" : "fail");

    return result;
}

bool type_task_execute(Instance *inst, Task *task)
{
    assert(task->kind == Task_Kind::TYPE);

    auto name = atom_string(task->resolve.decl->ident->atom);
    log_trace("Typeing: %s...", name.data);

    bool result = type_declaration(inst, task, task->type.decl, task->type.scope);

    log_trace("Typeing: %s...%s", name.data, result ? "success" : "fail");

    return result;
}

void queue_resolve_tasks(Instance *inst, AST_File *file, Scope *scope)
{
    for (s64 i = 0; i < file->nodes.count; i++) {
        auto &node = file->nodes[i];

        switch (node.kind) {
            case AST_Node_Kind::INVALID: assert(false); break;

            case AST_Node_Kind::DECLARATION: {
                assert(node.declaration->kind == AST_Declaration_Kind::FUNCTION);

                Task task;
                resolve_task_create(inst, &task, node.declaration, scope);
                darray_append(&inst->tasks, task);
                break;
            }

            case AST_Node_Kind::STATEMENT: {
                assert(node.statement->kind == AST_Statement_Kind::IMPORT);

                Task task;
                parse_task_create(inst, &task, node.statement->import_path);

                darray_append(&inst->tasks, task);
                break;
            }

            case AST_Node_Kind::EXPRESSION: assert(false); break;
        }
    }
}

void queue_size_tasks(Instance *inst, AST_Declaration *decl, Scope *scope)
{
    assert(decl->kind == AST_Declaration_Kind::FUNCTION);

    Task task;
    size_task_create(inst, &task, decl, scope);
    darray_append(&inst->tasks, task);
}

void queue_type_tasks(Instance *inst, AST_Declaration *decl, Scope *scope)
{
    assert(decl->kind == AST_Declaration_Kind::FUNCTION);

    Task task;
    type_task_create(inst, &task, decl, scope);
    darray_append(&inst->tasks, task);
}

}
