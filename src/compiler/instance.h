#pragma once

#include <containers/darray.h>
#include <defines.h>
#include <memory/allocator.h>
#include <memory/linear_allocator.h>
#include <memory/temp_allocator.h>
#include <nstring.h>

#include "options.h"

namespace Novo {

struct Scope;
struct Source_Pos;
struct Source_Range;
struct Task;

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

    DArray<Task> tasks;
    Scope *global_scope;

    bool fatal_error;
    DArray<Source_Pos> source_positions;
    DArray<Source_Range> source_ranges;

};

NAPI void instance_init(Instance *inst, Options options);
NAPI bool instance_start(Instance *inst);

NAPI void instance_error(Instance *inst, Source_Pos sp, const char* fmt, ...);
NAPI void instance_error(Instance *inst, u32 sp_id, const char* fmt, ...);

NAPI void instance_fatal_error(Instance *inst, Source_Pos sp, const char* fmt, ...);
NAPI void instance_fatal_error(Instance *inst, u32 sp_id, const char* fmt, ...);

NAPI void instance_fatal_error_note(Instance *inst, Source_Pos sp, const char* fmt, ...);
NAPI void instance_fatal_error_note(Instance *inst, u32 sp_id, const char* fmt, ...);

}
