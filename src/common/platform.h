#pragma once

#include "defines.h"
#include "nstring.h"
#include <containers/darray.h>

#if NPLATFORM_LINUX
#define NPLATFORM_PATH_SEPARATOR "/"
#elif NPLATFORM_WINDOWS
#define NPLATFORM_PATH_SEPARATOR "\\"
#else // NPLATFORM_LINUX
static_assert(false, "Unsupported platform!");
#endif // NPLATFORM_LINUX

namespace Novo {

struct Allocator;

NAPI String platform_dirname(Allocator* allocator, String_Ref path);
NAPI String platform_exe_path(Allocator* allocator, const char* argv_0);

struct Command_Result
{
    s64 exit_code;
    bool success;

    String result_string;
    String error_string;
};

NAPI Command_Result platform_run_command(Array_Ref<String_Ref> command_line, Allocator* debug_allocator);
NAPI void platform_free_command_result(Command_Result *cres);

}
