#include "task.h"

#include <containers/darray.h>

#include "atom.h"
#include "ast.h"
#include "ast_print.h"
#include "instance.h"
#include "logger.h"
#include "parser.h"
#include "resolve.h"

#include <cassert>
#include <cstdio>

namespace Novo {

void create_task(Task *task, Task_Kind kind)
{
    *task = {};
    task->kind = kind;
}

void parse_task_create(Task *task, const String_Ref file_path)
{
    create_task(task, Task_Kind::PARSE);
    task->parse = {
        file_path,
    };
}

void resolve_task_create(Task *task, AST_Declaration *decl)
{
    create_task(task, Task_Kind::RESOLVE);
    task->resolve = {
        decl,
    };
}

bool task_execute(Instance *instance, Task *task)
{
    switch (task->kind) {

        case Task_Kind::INVALID: assert(false); break;

        case Task_Kind::PARSE: {
            log_trace("Parsing: %s", task->parse.full_path.data);

            auto file = parse_file(instance, task->parse.full_path.data);
            if (!file) return false;

            for (s64 i = 0; i < file->nodes.count; i++) {
                auto &node = file->nodes[i];

                switch (node.kind) {
                    case AST_Node_Kind::INVALID: assert(false); break;

                    case AST_Node_Kind::DECLARATION: {
                        Task task;
                        resolve_task_create(&task, node.declaration);
                        darray_append(&instance->tasks, task);
                        break;
                    }

                    case AST_Node_Kind::STATEMENT: {
                        assert(node.statement->kind == AST_Statement_Kind::IMPORT);

                        Task task;
                        parse_task_create(&task, node.statement->import_path);
                        darray_append(&instance->tasks, task);
                        break;
                    }

                    case AST_Node_Kind::EXPRESSION: assert(false); break;
                }

            }

            if (instance->options.print_ast) {
                auto ast_str = ast_to_string(instance, file, &instance->temp_allocator);
                printf("%s\n", ast_str.data);
            }
            break;
        }

        case Task_Kind::RESOLVE: {
            auto name = atom_string(task->resolve.decl->ident->atom);
            log_trace("Resolving: %s...", name.data);

            bool result = resolve_declaration(instance, task->resolve.decl, task->resolve.scope);
            log_trace("Resolving: %s...%s", name.data, result ? "success" : "fail");

            return result;
        };

    }
    return true;
}

}
