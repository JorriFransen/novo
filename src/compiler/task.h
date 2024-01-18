#pragma once

#include <atom.h>
#include <defines.h>
#include <nstring.h>

namespace Novo {

struct AST_Declaration;
struct AST_Identifier;
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

    AST_Identifier *waiting_for;
};

struct Task {
    Task_Kind kind = Task_Kind::INVALID;

    bool done;
    union {
        Parse_Task parse;
        Resolve_Task resolve;
    };

    Task() : kind(Task_Kind::INVALID) {}
};

NAPI void create_task(Instance *inst, Task *task, Task_Kind kind);
NAPI void parse_task_create(Instance *inst, Task *task, const String_Ref file_path);
NAPI void resolve_task_create(Instance *inst, Task *task, AST_Declaration *decl);

NAPI bool task_execute(Instance *inst, Task *task);

}
