#pragma once

#include <containers/darray.h>
#include <defines.h>
#include <memory/allocator.h>
#include <memory/linear_allocator.h>
#include <memory/temp_allocator.h>
#include <nstring.h>

#include "atom.h"
#include "options.h"

namespace Novo {

struct Parse_Task;
struct Resolve_Task;
struct Scope;
struct Source_Pos;
struct Source_Range;
struct SSA_Task;
struct Type;
struct Type_Task;
struct SSA_Program;

struct Instance
{
    Options options;

    String_Ref cwd = "./";

    Allocator *default_allocator = c_allocator();

    Temp_Allocator temp_allocator_data;
    Allocator temp_allocator;

    Linear_Allocator ast_allocator_data;
    Allocator ast_allocator;
    Allocator scope_allocator;

    DArray<Parse_Task> parse_tasks;
    DArray<Resolve_Task> resolve_tasks;
    DArray<Type_Task> type_tasks;
    DArray<SSA_Task> ssa_tasks;

    Scope *global_scope;

    DArray<Atom> imported_files;

    DArray<Type *> function_types;

    bool fatal_error;
    DArray<Source_Pos> source_positions;
    DArray<Source_Range> source_ranges;

    SSA_Program *ssa_program;

    Type *builtin_type_void;
    Type *builtin_type_u8;
    Type *builtin_type_s64;
    Type *builtin_type_bool;

};

NAPI void instance_init(Instance *inst, Options options);
NAPI void instance_free(Instance *inst);

NAPI bool instance_start(Instance *inst);

NAPI void instance_error(Instance *inst, Source_Pos sp, const char* fmt, ...);
NAPI void instance_error(Instance *inst, u32 sp_id, const char* fmt, ...);

NAPI void instance_fatal_error(Instance *inst, Source_Pos sp, const char* fmt, ...);
NAPI void instance_fatal_error(Instance *inst, u32 sp_id, const char* fmt, ...);

NAPI void instance_fatal_error_note(Instance *inst, Source_Pos sp, const char* fmt, ...);
NAPI void instance_fatal_error_note(Instance *inst, u32 sp_id, const char* fmt, ...);

}
