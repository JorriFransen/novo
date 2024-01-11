#pragma once

#include <defines.h>
#include <nstring.h>

namespace Novo {

struct AST_Declaration;
struct Scope;
struct Instance;

enum class Task_Kind {
    INVALID = 0,
    PARSE,
    RESOLVE,
};

struct Parse_Task {
    String_Ref full_path;
};

struct Resolve_Task {
    AST_Declaration *decl;
    Scope *scope;
};

struct Task {
    Task_Kind kind = Task_Kind::INVALID;

    union {
        Parse_Task parse;
        Resolve_Task resolve;
    };

    Task() : kind(Task_Kind::INVALID) {}
};

NAPI void create_task(Task *task, Task_Kind kind);
NAPI void parse_task_create(Task *task, const String_Ref file_path);
NAPI void resolve_task_create(Task *task, AST_Declaration *decl);

NAPI bool task_execute(Instance *instance, Task *task);

}
