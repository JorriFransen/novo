#pragma once

#include "lexer.h"

#include <containers/darray.h>
#include <defines.h>
#include <memory/linear_allocator.h>
#include <memory/temp_allocator.h>
#include <nstring.h>

namespace Novo {

struct Instance
{
    String_Ref cwd = "./";
    String_Ref first_file_name;

    Allocator *default_allocator = c_allocator();

    Temp_Allocator temp_allocator_data;
    Allocator temp_allocator;

    Linear_Allocator ast_allocator_data;
    Allocator ast_allocator;

    DArray<Source_Pos> source_positions;
    DArray<Source_Range> source_ranges;
};

NAPI void instance_init(Instance *instance);
NAPI bool instance_start(Instance *instance, const String_Ref first_file_name);

}
