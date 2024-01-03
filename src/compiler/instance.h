#pragma once

#include <defines.h>
#include <nstring.h>

namespace Novo {

struct Instance
{
    String_Ref cwd = "./";
    String_Ref first_file_name;

    Allocator default_allocator = c_allocator();
};

NAPI bool instance_start(Instance *instance);

}
