#pragma once

#include <containers/darray.h>
#include <defines.h>
#include <memory/allocator.h>
#include <memory/linear_allocator.h>
#include <memory/temp_allocator.h>
#include <nstring.h>

namespace Novo {

struct Source_Pos;
struct Source_Range;
struct Task;

struct Instance
{
    String_Ref cwd = "./";
    String_Ref first_file_name;

    Allocator *default_allocator = c_allocator();

    Temp_Allocator temp_allocator_data;
    Allocator temp_allocator;

    Linear_Allocator ast_allocator_data;
    Allocator ast_allocator;
    Allocator scope_allocator;

    DArray<Task> tasks;

    bool fatal_error;
    DArray<Source_Pos> source_positions;
    DArray<Source_Range> source_ranges;

};

NAPI void instance_init(Instance *instance);
NAPI bool instance_start(Instance *instance, const String_Ref first_file_name);

NAPI void instance_fatal_error(Instance *instance, Source_Pos sp, const char* fmt, ...);
NAPI void instance_fatal_error(Instance *instance, u32 sp_id, const char* fmt, ...);

NAPI void instance_fatal_error_note(Instance *instance, Source_Pos sp, const char* fmt, ...);
NAPI void instance_fatal_error_note(Instance *instance, u32 sp_id, const char* fmt, ...);

}
