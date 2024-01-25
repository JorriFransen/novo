#pragma once

#include <defines.h>
#include <nstring.h>

namespace Novo {

struct AST_Declaration;
struct AST_File;
struct AST_Identifier;
struct Scope;
struct Instance;
struct Type;

enum class Task_Kind {
    INVALID = 0,
    PARSE,
    RESOLVE,
    SIZE,
    TYPE,
    SSA,
};

struct Parse_Task {
    String_Ref full_path;
    AST_File *result;
};

struct Resolve_Task {
    AST_Declaration *decl;
    Scope *scope;

    AST_Identifier *waiting_for;
};

struct Size_Task {
    AST_Declaration *decl;
    Scope *scope;
};

struct Type_Task {
    AST_Declaration *decl;
    Scope *scope;

    AST_Declaration *waiting_for;

    Type *current_function_type;
};

struct SSA_Task {
    AST_Declaration *decl;
};

struct Task {
    Task_Kind kind = Task_Kind::INVALID;

    bool done;

    union {
        Parse_Task parse;
        Resolve_Task resolve;
        Size_Task size;
        Type_Task type;
        SSA_Task ssa;
    };

    Task() : kind(Task_Kind::INVALID) {}
};

NAPI void create_task(Instance *inst, Task *task, Task_Kind kind);
NAPI void parse_task_create(Instance *inst, Task *task, const String_Ref file_path);
NAPI void resolve_task_create(Instance *inst, Task *task, AST_Declaration *decl, Scope *scope);
NAPI void size_task_create(Instance *inst, Task *task, AST_Declaration *decl, Scope *scope);
NAPI void type_task_create(Instance *inst, Task *task, AST_Declaration *decl, Scope *scope);
NAPI void ssa_task_create(Instance *inst, Task *task, AST_Declaration *decl);

NAPI bool task_execute(Instance *inst, Task *task);
NAPI bool parse_task_execute(Instance *inst, Task *task);
NAPI bool resolve_task_execute(Instance *inst, Task *task);
NAPI bool size_task_execute(Instance *inst, Task *task);
NAPI bool type_task_execute(Instance *inst, Task *task);
NAPI bool ssa_task_execute(Instance *inst, Task *task);

NAPI void queue_resolve_tasks(Instance *inst, AST_File *file, Scope *scope);
NAPI void queue_size_tasks(Instance *inst, AST_Declaration *decl, Scope *scope);
NAPI void queue_type_tasks(Instance *inst, AST_Declaration *decl, Scope *scope);
NAPI void queue_ssa_tasks(Instance *inst, AST_Declaration *decl);

}
