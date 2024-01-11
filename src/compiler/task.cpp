#include "task.h"

#include <containers/darray.h>
#include <defines.h>
#include <nstring.h>

#include "atom.h"
#include "ast.h"
#include "instance.h"
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
            printf("Parsing: %s\n", task->parse.full_path.data);

            auto file = parse_file(instance, task->parse.full_path.data);
            if (!file) return false;

            for (s64 i = 0; i < file->declarations.count; i++) {
                Task task;
                resolve_task_create(&task, file->declarations[i]);
                darray_append(&instance->tasks, task);
            }

            // auto ast_str = ast_to_string(instance, file, &instance->temp_allocator);
            // printf("\"%s\"\n", ast_str.data);
            break;
        }

        case Task_Kind::RESOLVE: {
            auto name = atom_string(task->resolve.decl->ident->atom);
            printf("Resolving: %s...", name.data);

            bool result = resolve_declaration(instance, task->resolve.decl, task->resolve.scope);
            printf("%s\n", result ? "success" : "fail");

            return result;
        };

    }
    return true;
}

}
