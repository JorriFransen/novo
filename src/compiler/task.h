#pragma once

#include <nstring.h>

namespace Novo {

struct Instance;

enum class Task_Kind {
    INVALID = 0,
    PARSE,
};

struct Parse_Task {
    String_Ref full_path;
};

struct Task {
    Task_Kind kind = Task_Kind::INVALID;

    union {
        Parse_Task parse;
    };

    Task() : kind(Task_Kind::INVALID) {}
};

NAPI void create_task(Task *task, Task_Kind kind);
NAPI void parse_task_create(Task *task, const String_Ref file_path);

NAPI bool task_execute(Instance *instance, Task *task);

}
