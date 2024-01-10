#include "task.h"

#include "instance.h"
#include "parser.h"

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

bool task_execute(Instance *instance, Task *task)
{
    switch (task->kind) {

        case Task_Kind::INVALID: assert(false); break;

        case Task_Kind::PARSE: {
            printf("Parsing: %s\n", task->parse.full_path.data);

            parse_file(instance, task->parse.full_path.data);
            break;
        }

    }
    return true;
}

}
