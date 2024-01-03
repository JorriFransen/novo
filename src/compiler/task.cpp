#include "task.h"
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
        .full_path = file_path,
    };
}

bool task_execute(Instance *instance, Task *task)
{
    printf("Parsing: %s\n", task->parse.full_path.data);
    return true;
}

}
